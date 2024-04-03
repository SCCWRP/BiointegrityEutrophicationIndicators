library(dplyr)
library(ggplot2)

msid.lu <- readr::read_csv("data-raw/Part_1_lu_masterid.csv")
psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp")
studyarea_sf <- sf::st_read("data-raw/Part_1_shapefiles/RB5_StudyAreas.shp")


test_data <- readr::read_csv('data-raw/Part_1_test_data_gis.csv') |> 
  select(masterid, latitude, longitude, RefStatus) |>
  mutate(type = "Test") |> 
  unique() 

biostim_data <- readr::read_csv('data-raw/Part_1_biostim_gis_input_cleaned.csv') |>
  mutate(
    RefStatus = case_when(masterid == "207SUI800" ~ "Non-reference", .default = RefStatus)
  ) |>
  transmute(
    masterid = masterid, 
    latitude = new_lat, 
    longitude = new_long, 
    RefStatus = case_when(RefStatus == "Reference" ~ "Reference", .default = "Non-reference"), 
    type = "Biostimulatory"
  ) |> 
  unique()

asci_data <- readr::read_csv("data-raw/Part_1_asci_caldata.csv")  |>
  mutate(
    RefStatus = case_when(
      StationCode == "207SUI800" ~ "Non-reference",
      SiteSetSample2 == "Reference" ~ "Reference",
      .default = "Non-reference"
    )
  ) |>
  rename(stationcode=StationCode) |>
  left_join(msid.lu) |>
  transmute(
    masterid = stationcode, 
    latitude, 
    longitude,
    RefStatus = case_when(RefStatus == "Reference" ~ "Reference", .default = "Non-reference"),
    type = "ASCI") |> 
  unique()

csci_data <- readr::read_csv("data-raw/Part_1_stations.out.csv") |>
  transmute(
    masterid = StationCode, 
    latitude = New_Lat, 
    longitude = New_Long,
    RefStatus = case_when(SiteStatus == "Reference" ~ "Reference", .default = "Non-reference"),
    type = "CSCI"
  ) |>
  unique()


cal_sf <- bind_rows(biostim_data, csci_data, asci_data, test_data) |>
  mutate(RefStatus = case_when(masterid == "207SUI800" ~ "Non-reference", .default = RefStatus)) |>
  filter(!is.na(latitude)) |>
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = F) |>
  sf::st_transform(crs = sf::st_crs(psa_sf)) |>
  sf::st_join(studyarea_sf) |>
  mutate(
    InStudy = PSA6 %in% c("Central Valley", "Deserts Modoc"),
    type = factor(type, levels = c("CSCI", "ASCI", "Biostimulatory", "Test"))
  ) 

data_sets_map <- ggplot() +
  geom_sf(data = psa_sf) +
  geom_sf(data = studyarea_sf, fill = "white") +
  geom_sf(data = cal_sf, aes(fill = RefStatus, size = InStudy, shape = RefStatus)) +
  facet_wrap(vars(type)) +
  scale_fill_brewer(palette = "Set1", name = "Status") +
  scale_size_manual(values = c(.5, 2), guide = "none") +
  scale_shape_manual(values = c(21, 24), name = "Status") +
  theme_bw() +
  theme(axis.text = element_blank())

ggsave(data_sets_map, filename = "figures/Part_1_Figure_01.jpg", height = 6, width = 6.5, dpi = 300)  