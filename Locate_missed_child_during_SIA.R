library(dplyr)
library(sf)
library(ggplot2)
library(osmdata)

# Load required libraries
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(osmdata, sf, dplyr, readr, ggplot2, stringr)

# Read ES site data from CSV
im_data <- read_csv("C:/Users/TOURE/Documents/REPOSITORIES/IM_raw_data/Mappin_missed_child/Ghana_SIA.csv") 

im_data <- im_data %>%
  mutate(across(
    starts_with("HH"),
    ~ as.numeric(replace(., . %in% c(".", "NA", "n/a", ""), NA))
  ))

im_data <- im_data |> 
  filter(Type_Monitoring == "InProcess") %>% 
  select(
    COUNTRY = Country, PROVINCE = Region, DISTRICT = District, Facility, 
    Settlement = NameSettlement, Response, roundNumber, date_monitored, lat = `_GPS_hh_latitude`, 
    lon = `_GPS_hh_longitude`, 
    `HH[1]/Diff_Present_Vacc_FM_HH`, `HH[2]/Diff_Present_Vacc_FM_HH`, 
    `HH[3]/Diff_Present_Vacc_FM_HH`, `HH[4]/Diff_Present_Vacc_FM_HH`, 
    `HH[5]/Diff_Present_Vacc_FM_HH`, `HH[6]/Diff_Present_Vacc_FM_HH`, 
    `HH[7]/Diff_Present_Vacc_FM_HH`, `HH[8]/Diff_Present_Vacc_FM_HH`, 
    `HH[9]/Diff_Present_Vacc_FM_HH`, `HH[10]/Diff_Present_Vacc_FM_HH`, 
    `HH[1]/group1/Tot_child_Absent_HH`, `HH[1]/group1/Tot_child_NC_HH`, 
    `HH[1]/group1/Tot_child_NotVisited_HH`, `HH[1]/group1/Tot_child_NotRevisited`, 
    `HH[1]/group1/Tot_child_Asleep_HH`, `HH[1]/group1/Tot_child_VaccinatedRoutine`, 
    `HH[1]/group1/Tot_child_Others_HH`
  ) %>% 
  mutate(across(
    c(`HH[1]/Diff_Present_Vacc_FM_HH`, `HH[2]/Diff_Present_Vacc_FM_HH`, 
      `HH[3]/Diff_Present_Vacc_FM_HH`, `HH[4]/Diff_Present_Vacc_FM_HH`, 
      `HH[5]/Diff_Present_Vacc_FM_HH`, `HH[6]/Diff_Present_Vacc_FM_HH`, 
      `HH[7]/Diff_Present_Vacc_FM_HH`, `HH[8]/Diff_Present_Vacc_FM_HH`, 
      `HH[9]/Diff_Present_Vacc_FM_HH`, `HH[10]/Diff_Present_Vacc_FM_HH`
    ), as.numeric)) %>% 
  group_by(
    COUNTRY, PROVINCE, DISTRICT, Facility, Settlement, Response, roundNumber, date_monitored, 
    lat, lon, `HH[1]/group1/Tot_child_Absent_HH`, `HH[1]/group1/Tot_child_NC_HH`, 
    `HH[1]/group1/Tot_child_NotVisited_HH`, `HH[1]/group1/Tot_child_NotRevisited`, 
    `HH[1]/group1/Tot_child_Asleep_HH`, `HH[1]/group1/Tot_child_VaccinatedRoutine`, 
    `HH[1]/group1/Tot_child_Others_HH`
  ) %>% 
  summarise(
    across(
      c(`HH[1]/Diff_Present_Vacc_FM_HH`, `HH[2]/Diff_Present_Vacc_FM_HH`, 
        `HH[3]/Diff_Present_Vacc_FM_HH`, `HH[4]/Diff_Present_Vacc_FM_HH`, 
        `HH[5]/Diff_Present_Vacc_FM_HH`, `HH[6]/Diff_Present_Vacc_FM_HH`, 
        `HH[7]/Diff_Present_Vacc_FM_HH`, `HH[8]/Diff_Present_Vacc_FM_HH`, 
        `HH[9]/Diff_Present_Vacc_FM_HH`, `HH[10]/Diff_Present_Vacc_FM_HH`
      ), ~ sum(., na.rm = TRUE), .names = "total_missed_{col}"
    )
  )

# Enhanced create_settlment_map Function
create_settlment_map <- function(
    ctry, 
    Response,
    roundNumber,
    settlement,  # Harmonized argument name
    bbox_m = 600
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
  settlement_data <- im_data %>%
    filter(
      COUNTRY == ctry, 
      Settlement == settlement,
      Response == Response,
      roundNumber == roundNumber,
      !is.na(lon), 
      !is.na(lat)  # Ensure only valid rows with coordinates are considered
    )
  
  # Debugging: Check if data matches
  if (nrow(settlement_data) == 0) {
    print("No matching data found for the specified site or coordinates are invalid.")
    print("Debug Info:")
    print(paste("Available Settlements in the dataset:"))
    print(unique(im_data$Settlement))
    return(NULL)
  }
  
  # Ensure valid lon and lat exist
  if (all(is.na(settlement_data$lon)) || all(is.na(settlement_data$lat))) {
    stop("No valid longitude or latitude values available in filtered data.")
  }
  
  # Convert site data to spatial data and ensure it's properly flattened to 2D
  sp_settlement_data <- settlement_data %>% 
    st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) %>%
    st_zm()  # Ensure geometry is strictly 2D to avoid errors
  
  # Bounding box calculation for visualization
  lon <- pull(settlement_data, lon)
  lat <- pull(settlement_data, lat)
  
  # Remove NA values explicitly before calculating bbox
  lon <- lon[!is.na(lon)]
  lat <- lat[!is.na(lat)]
  
  if (length(lon) == 0 || length(lat) == 0) {
    stop("After filtering, no valid lon/lat values available to calculate the bounding box.")
  }
  
  # Calculate bounding box dynamically based on coordinates and bbox_m
  lon_margin <- bbox_m / (111320 * cos(mean(lat) * pi / 180))
  lat_margin <- bbox_m / 111320
  
  bbox <- st_bbox(c(
    xmin = mean(lon, na.rm = TRUE) - lon_margin, 
    xmax = mean(lon, na.rm = TRUE) + lon_margin, 
    ymin = mean(lat, na.rm = TRUE) - lat_margin, 
    ymax = mean(lat, na.rm = TRUE) + lat_margin
  ), crs = st_crs(4326))
  
  # Pull OSM data
  print("Pulling OSM data...")
  
  area_major <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary")) %>%
    osmdata_sf()
  
  area_minor <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway", value = c("tertiary", "residential")) %>%
    osmdata_sf()
  
  area_blue <- opq(bbox = bbox) %>%
    add_osm_feature(key = "waterway") %>%
    osmdata_sf()
  
  area_cities <- opq(bbox = bbox) %>%
    add_osm_feature(key = "place", value = c("city", "town", "village")) %>%
    osmdata_sf()
  
  area_buildings <- opq(bbox = bbox) %>%
    add_osm_feature(key = "building") %>%
    osmdata_sf()
  
  # Select the smallest place type for labeling
  if (!is.null(area_cities$osm_points) && "place" %in% colnames(area_cities$osm_points)) {
    cities <- area_cities$osm_points %>%
      mutate(place_rank = factor(place, levels = c("city", "town", "village"))) %>%
      filter(as.numeric(place_rank) == min(as.numeric(place_rank), na.rm = TRUE))
  } else {
    cities <- data.frame() # Create an empty data frame if no valid data exists
  }
  
  # Create the base map visualization
  base_map <- ggplot() +
    geom_sf(data = area_major$osm_lines, color = "black", size = 0.4, inherit.aes = FALSE) +
    geom_sf(data = area_minor$osm_lines, color = "gray50", size = 0.3, inherit.aes = FALSE) +
    geom_sf(data = area_blue$osm_lines, color = "blue", size = 0.3, inherit.aes = FALSE) +
    geom_sf(data = area_buildings$osm_polygons, fill = "gray80", color = "gray50", size = 0.2, inherit.aes = FALSE) +
    geom_sf(data = sp_settlement_data, color = "red", size = 3, shape = 23, inherit.aes = FALSE) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
    theme_void() +
    labs(title = paste0("Missing child during InProcess  - ", settlement), 
         subtitle = paste0(ctry)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12))
  
  # Add city labels if available
  if (nrow(cities) > 0) {
    base_map <- base_map +
      geom_sf_label(data = cities, aes(label = name), size = 3, color = "black")
  }
  
  # Save the map
  output_path <- paste0("C:/Users/TOURE/Documents/LCB/outputs/missed_child/", settlement, ".png")
  ggsave(output_path, plot = base_map)
  
  return(base_map)
}

# Generate unique settlement list and loop
unique_settlements <- unique(im_data$Settlement)

for (settlement in unique_settlements) {
  print(paste("Generating map for settlement:", settlement))
  create_settlment_map(
    ctry = "GHANA", 
    Response = "GHA-2024-10-01_nOPV", 
    roundNumber = "Rnd1", 
    settlement = settlement
  )
}
