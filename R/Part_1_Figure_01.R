msid.lu <- readr::read_csv("data-raw/Part_1_lu_masterid.csv")
psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp")
studyarea_sf <- sf::st_read("data-raw/Part_1_shapefiles/RB5_StudyAreas.shp")


test_data <- readr::read_csv('data-raw/Part_1_test_data_gis.csv') |> 
  dplyr::select(masterid, latitude, longitude, RefStatus) |>
  dplyr::mutate(type = "Test") |> 
  unique() 

biostim_data <- readr::read_csv('data-raw/Part_1_biostim_gis_input_cleaned.csv') |>
  dplyr::mutate(
    RefStatus = dplyr::case_when(masterid == "207SUI800" ~ "Non-reference", .default = RefStatus)
  ) |>
  dplyr::transmute(
    masterid = masterid, 
    latitude = new_lat, 
    longitude = new_long, 
    RefStatus = dplyr::case_when(RefStatus == "Reference" ~ "Reference", .default = "Non-reference"), 
    type = "Biostimulatory"
  ) |> 
  unique()

asci_data <- readr::read_csv("data-raw/Part_1_asci_caldata.csv")  |>
  dplyr::mutate(
    RefStatus = dplyr::case_when(
      StationCode == "207SUI800" ~ "Non-reference",
      SiteSetSample2 == "Reference" ~ "Reference",
      .default = "Non-reference"
    )
  ) |>
  dplyr::rename(stationcode=StationCode) |>
  dplyr::left_join(msid.lu) |>
  dplyr::transmute(
    masterid = stationcode, 
    latitude, 
    longitude,
    RefStatus = dplyr::case_when(RefStatus == "Reference" ~ "Reference", .default = "Non-reference"),
    type="ASCI") |> 
  unique()

csci_data <- readr::read_csv("data-raw/Part_1_stations.out.csv") |>
  dplyr::transmute(
    masterid = StationCode, 
    latitude = New_Lat, 
    longitude = New_Long,
    RefStatus = dplyr::case_when(SiteStatus == "Reference" ~ "Reference", .default = "Non-reference"),
    type="CSCI"
  ) |>
  unique()


cal_sf <- dplyr::bind_rows(biostim_data, csci_data, asci_data, test_data) |>
  dplyr::mutate(RefStatus = dplyr::case_when(masterid == "207SUI800" ~ "Non-reference", .default = RefStatus)) |>
  dplyr::filter(!is.na(latitude)) |>
  sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = F) |>
  sf::st_transform(crs = sf::st_crs(psa_sf)) |>
  sf::st_join(studyarea_sf) |>
  dplyr::mutate(InStudy = PSA6 %in% c("Central Valley", "Deserts Modoc")) 
cal_sf$type <- factor(cal_sf$type, levels=c("CSCI","ASCI","Biostimulatory","Test"))

data_sets_map <- ggplot2::ggplot()+
  ggplot2::geom_sf(data = psa_sf)+
  ggplot2::geom_sf(data = studyarea_sf, fill = "white")+
  ggplot2::geom_sf(data = cal_sf, ggplot2::aes(fill = RefStatus, size = InStudy, shape = RefStatus))+
  ggplot2::facet_wrap(ggplot2::vars(type))+
  ggplot2::scale_fill_brewer(palette = "Set1", name = "Status")+
  ggplot2::scale_size_manual(values = c(.5, 2), guide = "none")+
  ggplot2::scale_shape_manual(values = c(21, 24), name = "Status")+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text = ggplot2::element_blank())

ggplot2::ggsave(data_sets_map, filename = "figures/Part_1_Figure_01.jpg", height = 6, width = 6.5, dpi = 300)  