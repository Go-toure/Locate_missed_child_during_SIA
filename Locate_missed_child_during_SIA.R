# Load required libraries
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(osmdata, sf, dplyr, tidyr, readr, ggplot2, stringr, cowplot, patchwork, stringr,grid, ggforce) # Include necessary libraries 

# Read ES site data from CSV and filter for uppercase Country values
im_data <- read_csv(
  "C:/Users/TOURE/Documents/REPOSITORIES/IM_raw_data/Mappin_missed_child/Ghana_SIA.csv",
  na = c("NA", "n/a", ".", "")
) %>%
  filter(str_detect(Country, "^[A-Z]+$")) # Matches only all-uppercase values

# Data Cleaning and Transformation
im_data <- im_data %>%
  mutate(across(
    starts_with("HH"),
    ~ suppressWarnings(as.numeric(replace(., . %in% c(".", "NA", "n/a", ""), NA)))
  )) %>%
  # Add reasons for not being vaccinated
  mutate(
    Reason_Absent = rowSums(across(matches("^HH\\[\\d+\\]/group\\d+/Tot_child_Absent_HH"), ~ tidyr::replace_na(.x, 0)), na.rm = TRUE),
    Reason_NC = rowSums(across(matches("^HH\\[\\d+\\]/group\\d+/Tot_child_NC_HH"), ~ tidyr::replace_na(.x, 0)), na.rm = TRUE),
    Reason_NotVisited = rowSums(across(matches("^HH\\[\\d+\\]/group\\d+/Tot_child_NotVisited_HH"), ~ tidyr::replace_na(.x, 0)), na.rm = TRUE),
    Reason_NotRevisited = rowSums(across(matches("^HH\\[\\d+\\]/group\\d+/Tot_child_NotRevisited"), ~ tidyr::replace_na(.x, 0)), na.rm = TRUE),
    Reason_Asleep = rowSums(across(matches("^HH\\[\\d+\\]/group\\d+/Tot_child_Asleep_HH"), ~ tidyr::replace_na(.x, 0)), na.rm = TRUE),
    Reason_VaccinatedRoutine = rowSums(across(matches("^HH\\[\\d+\\]/group\\d+/Tot_child_VaccinatedRoutine"), ~ tidyr::replace_na(.x, 0)), na.rm = TRUE),
    Reason_Others = rowSums(across(matches("^HH\\[\\d+\\]/group\\d+/Tot_child_Others_HH"), ~ tidyr::replace_na(.x, 0)), na.rm = TRUE)
  ) %>%
  # Filter and select relevant columns
  filter(Type_Monitoring == "InProcess") %>%
  select(
    COUNTRY = Country, PROVINCE = Region, DISTRICT = District, Facility, 
    Settlement = NameSettlement, Response, roundNumber, date_monitored, 
    lat = `_GPS_hh_latitude`, lon = `_GPS_hh_longitude`, 
    starts_with("HH") & ends_with("Diff_Present_Vacc_FM_HH"),
    Reason_Absent, Reason_NC, Reason_NotVisited, Reason_Asleep, 
    Reason_VaccinatedRoutine, Reason_Others
  ) %>%
  # Explicit renaming of HH columns using mutate
  mutate(
    Household_1 = `HH[1]/Diff_Present_Vacc_FM_HH`,
    Household_2 = `HH[2]/Diff_Present_Vacc_FM_HH`,
    Household_3 = `HH[3]/Diff_Present_Vacc_FM_HH`,
    Household_4 = `HH[4]/Diff_Present_Vacc_FM_HH`,
    Household_5 = `HH[5]/Diff_Present_Vacc_FM_HH`,
    Household_6 = `HH[6]/Diff_Present_Vacc_FM_HH`,
    Household_7 = `HH[7]/Diff_Present_Vacc_FM_HH`,
    Household_8 = `HH[8]/Diff_Present_Vacc_FM_HH`,
    Household_9 = `HH[9]/Diff_Present_Vacc_FM_HH`,
    Household_10 = `HH[10]/Diff_Present_Vacc_FM_HH`
  ) %>%
  # Group and summarize data
  group_by(
    COUNTRY, PROVINCE, DISTRICT, Facility, Settlement, Response, 
    roundNumber, date_monitored, lat, lon
  ) %>%
  summarise(
    across(
      c(starts_with("House"),
        Reason_Absent, Reason_NC, Reason_NotVisited, Reason_Asleep, 
        Reason_VaccinatedRoutine, Reason_Others),
      ~ sum(., na.rm = TRUE)),
    .groups = "drop" # Ungroup after summarization
  ) %>%
  # Add Total_child_missed column
  mutate(
    Total_child_missed = rowSums(across(starts_with("House")), na.rm = TRUE)) |> 
  filter(Total_child_missed>=1)

# # Print the resulting dataset
# print(im_data)

# Retry Function for Overpass Queries
retry_query <- function(query) {
  tryCatch({
    query |> osmdata_sf()
  }, error = function(e) {
    message("Retrying OSM query after failure...")
    Sys.sleep(5)  # Wait before retrying
    query |> osmdata_sf()
  })
}

create_settlement_map <- function(
    ctry, 
    Response,
    roundNumber,
    settlement,  
    bbox_m = 300  
) {
  # Convert inputs to uppercase for consistency in filtering
  ctry <- toupper(ctry)
  settlement <- toupper(settlement)
  
  # Preprocess and clean data - replace 'n/a' with NA in coordinates
  im_data <- im_data |>
    mutate(
      lon = ifelse(lon == "n/a", NA, as.numeric(lon)),
      lat = ifelse(lat == "n/a", NA, as.numeric(lat))
    ) |>
    filter(!is.na(lon) & !is.na(lat))  # Remove invalid coordinates
  
  # Filter site data
  settlement_data <- im_data |>
    filter(
      COUNTRY == ctry, 
      Settlement == settlement,
      Response == Response,
      roundNumber == roundNumber
    )
  
  if (nrow(settlement_data) == 0) {
    print(paste("No matching data found for settlement:", settlement))
    return(NULL)
  }
  
  # Format HH and Reasons
  hh_columns <- colnames(settlement_data)[grepl("^Household_", colnames(settlement_data))]
  reason_columns <- c("Reason_Absent", "Reason_NC", "Reason_NotVisited", "Reason_Asleep", 
                      "Reason_VaccinatedRoutine", "Reason_Others")
  
  hh_values <- sapply(hh_columns, function(col) settlement_data[[col]][1], simplify = TRUE, USE.NAMES = TRUE)
  reason_values <- sapply(reason_columns, function(col) settlement_data[[col]][1], simplify = TRUE, USE.NAMES = TRUE)
  
  # Remove spaces inside parentheses
  # Format HH and Reasons without spaces inside parentheses and replace zeros with NA
  hh_formatted <- if (length(hh_columns) > 0) {
    paste0(paste("      ", hh_columns, "(", hh_values, ")", collapse = ",\n"))
  } else {
    "No Household Data"
  }
  
  reason_values <- sapply(reason_columns, function(col) {
    value <- settlement_data[[col]][1]
    if (value == 0) "NA" else value
  }, simplify = TRUE, USE.NAMES = TRUE)
  
  reasons_formatted <- if (length(reason_columns) > 0) {
    paste0(paste("      ", reason_columns, "(", reason_values, ")", collapse = ",\n"))
  } else {
    "No Reasons Data"
  }
  
  # Generate missed child details
  missed_child_details <- paste0(
    "Country: ", settlement_data$COUNTRY[1], "\n",
    "Province: ", settlement_data$PROVINCE[1], "\n",
    "District: ", settlement_data$DISTRICT[1], "\n",
    "Facility: ", settlement_data$Facility[1], "\n",
    "Settlement: ", settlement_data$Settlement[1], "\n",
    "Total Child Missed: ", settlement_data$Total_child_missed[1], "\n",
    "HH where missed child has been found:\n", hh_formatted, "\n",
    "Reason(s) for not being vaccinated:\n", reasons_formatted
  )
  
  # Split wrapped legend into lines for dynamic size calculation
  text_lines <- unlist(str_split(missed_child_details, "\n"))
  
  # Calculate dynamic text box dimensions
  text_width <- max(nchar(text_lines)) * 0.12  # Increased width scaling for better fit
  text_height <- length(text_lines) * 0.5     # Adjust height scaling for wrapped lines
  
  # Create the missed child details plot
  missed_child_details_plot <- ggplot() +
    # Draw a rectangle that fully encompasses the text
    annotate(
      "rect", xmin = -text_width / 2, xmax = text_width / 2, 
      ymin = -text_height / 2, ymax = text_height / 2,
      fill = "lightgrey", color = "NA"
    ) +
    # Add the wrapped text
    annotate(
      "text", x = -text_width / 2, y = text_height / 2 - (0:(length(text_lines) - 1)) * 0.4, 
      label = text_lines,
      size = 4, hjust = 0, vjust = 1, family = "mono", lineheight = 0.8  # Adjusted lineheight
    ) +
    # Coordinate system adjusted to fit the text box
    coord_cartesian(
      xlim = c(-text_width / 2 - 0.1, text_width / 2 + 0.1), 
      ylim = c(-text_height / 2 - 0.1, text_height / 2 + 0.1), 
      expand = FALSE
    ) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Bounding box calculation for visualization
  lon <- pull(settlement_data, lon)
  lat <- pull(settlement_data, lat)
  
  lon_margin <- bbox_m / (111320 * cos(mean(lat) * pi / 180))
  lat_margin <- bbox_m / 111320
  
  bbox <- st_bbox(c(
    xmin = mean(lon, na.rm = TRUE) - lon_margin, 
    xmax = mean(lon, na.rm = TRUE) + lon_margin, 
    ymin = mean(lat, na.rm = TRUE) - lat_margin, 
    ymax = mean(lat, na.rm = TRUE) + lat_margin
  ), crs = st_crs(4326))
  
  # Pull OSM data
  area_major <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary")))
  area_minor <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "highway", value = c("tertiary", "residential")))
  area_blue <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "waterway"))
  area_buildings <- retry_query(opq(bbox = bbox) |> add_osm_feature(key = "building"))
  
  # Create the base map
  located_missed_child <- ggplot() +
    geom_sf(data = area_major$osm_lines, color = "black", size = 0.4) +
    geom_sf(data = area_minor$osm_lines, color = "gray50", size = 0.3) +
    geom_sf(data = area_blue$osm_lines, color = "blue", size = 0.3) +
    geom_sf(data = area_buildings$osm_polygons, fill = "gray80", color = "gray50", size = 0.2) +
    geom_sf(data = settlement_data |> st_as_sf(coords = c("lon", "lat"), crs = 4326), 
            color = "red", size = 3, shape = 23) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    labs(title = paste0("Missing child during InProcess - ", settlement), 
         subtitle = ctry) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  
  # Combine map and legend
  combined_plot <- located_missed_child + missed_child_details_plot + plot_layout(widths = c(4, 1.5))
  settlement_safe <- gsub("[^A-Za-z0-9]", "_", settlement)
  output_path <- paste0("C:/Users/TOURE/Documents/LCB/outputs/child/", settlement_safe, "_final.png")
  ggsave(output_path, plot = combined_plot, width = 14, height = 8)
  
  return(combined_plot)
}


# use this ----------------------------------------------------------------


# Generate unique settlement list and loop
unique_settlements <- unique(im_data$Settlement)

for (settlement in unique_settlements) {
  print(paste("Generating map for settlement:", settlement))
  create_settlement_map(
    ctry = "GHANA", 
    Response = "GHA-2024-10-01_nOPV", 
    roundNumber = "Rnd1", 
    settlement = settlement
  )
}




# or use this one ---------------------------------------------------------



unique_settlements <- "YENNYASO"

for (settlement in unique_settlements) {
  print(paste("Generating map for settlement:", settlement))
  create_settlement_map(
    ctry = "GHANA",
    Response = "GHA-2024-10-01_nOPV",
    roundNumber = "Rnd1",
    settlement = settlement
  )
}

