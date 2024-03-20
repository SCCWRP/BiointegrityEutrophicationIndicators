cv_ref <- readr::read_csv("data-raw/Part_1_cv_ibi_ref_tbl.csv")

psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp")

cv_ref_gis <- readr::read_csv("data-raw/Part_1_cv_ref_gis.csv") |>
  dplyr::arrange(masterid) |>
  dplyr::mutate(
    Source = dplyr::case_when(
      masterid %in% cv_ref$stationcod ~ "CV-IBI",
      masterid %in% c("504PS0227", "504FC1115") ~ "CSCI",
      masterid %in% c("541OC0010", "541OSCHW5") ~ "USGS",
      .default = "Other"
    )
  )


cv_ref_gis_sf <- cv_ref_gis |>
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = F, crs = 4326)


cv_ref_location_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = psa_sf) +
  ggplot2::geom_sf(data = cv_ref_gis_sf) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text = ggplot2::element_blank())

ggplot2::ggsave(cv_ref_location_map, filename = "figures/Part_1_Figure_09.jpg", dpi = 300, height = 4, width = 4)
