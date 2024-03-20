psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp")
studyarea_sf <- sf::st_read("data-raw/Part_1_shapefiles/RB5_StudyAreas.shp")

test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  dplyr::group_by(masterid) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup()

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv")

## Figure 6 ####

test_data_scores_sf <- test_data_scores |>
  dplyr::left_join(test_data_gis |> dplyr::select(masterid, StudyArea, RefStatus, new_lat, new_long)) |>
  dplyr::group_by(masterid, StudyArea, new_lat, new_long) |>
  dplyr::summarize(
    CSCI_max = max(CSCI, na.rm = T),
    ASCI_D_max = max(ASCI_D, na.rm = T),
    ASCI_H_max = max(ASCI_H, na.rm = T)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    CSCI = dplyr::case_when(
      CSCI_max >= 0.92 ~ "Class1",
      CSCI_max >= 0.79 ~ "Class2",
      CSCI_max >= 0.63 ~ "Class3",
      CSCI_max < 0.63 ~ "Class4",
      .default = NA_character_
    ),
    ASCI_D = dplyr::case_when(
      ASCI_D_max >= 0.94 ~ "Class1",
      ASCI_D_max >= 0.86 ~ "Class2",
      ASCI_D_max >= 0.75 ~ "Class3",
      ASCI_D_max < 0.75 ~ "Class4",
      .default = NA_character_
    ),
    ASCI_H = dplyr::case_when(
      ASCI_H_max >= 0.94 ~ "Class1",
      ASCI_H_max >= 0.86 ~ "Class2",
      ASCI_H_max >= 0.75 ~ "Class3",
      ASCI_H_max < 0.75 ~ "Class4",
      .default = NA_character_
    )
  ) |>
  tidyr::pivot_longer(cols = c("CSCI", "ASCI_D", "ASCI_H"), names_to = "Index", values_to = "Class") |>
  sf::st_as_sf(coords = c("new_long", "new_lat"), remove = F, crs = 4326) |>
  sf::st_transform(crs = sf::st_crs(psa_sf))

hi_scoring_stes_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = psa_sf) +
  ggplot2::geom_sf(
    data = test_data_scores_sf |> 
      dplyr::filter(Class %in% c("Class1", "Class2")), 
    ggplot2::aes(color = Class, shape = Class)
  ) +
  ggplot2::facet_wrap(ggplot2::vars(Index)) +
  ggplot2::scale_color_manual(values = c("#377eb8", "#4daf4a"), name = "Max Score", labels = c(">30th", ">10th")) +
  ggplot2::scale_shape_discrete(name = "Max Score", labels = c(">30th", ">10th")) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text = ggplot2::element_blank(), legend.position = "bottom")

ggplot2::ggsave(hi_scoring_stes_map, filename = "figures/Part_1_Figure_06.jpg", dpi = 300, height = 4, width = 6.5)

## Figure 7 ####

test_data_scores_summary <- test_data_scores |>
  dplyr::left_join(test_data_gis |> dplyr::select(masterid, StudyArea)) |>
  dplyr::select(masterid, StudyArea, CSCI, ASCI_D, ASCI_H) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index") |>
  na.omit() |>
  dplyr::group_by(masterid, StudyArea, Index) |>
  dplyr::summarize(
    index_n = length(value),
    index_mean = mean(value),
    index_max = max(value)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    Class = dplyr::case_when(
      Index == "CSCI" & index_max >= 0.92 ~ "Class1",
      Index == "CSCI" & index_max >= 0.79 ~ "Class2",
      Index == "CSCI" & index_max >= 0.63 ~ "Class3",
      Index == "CSCI" & index_max < 0.63 ~ "Class4",
      Index %in% c("ASCI_D", "ASCI_H") & index_max >= 0.94 ~ "Class1",
      Index %in% c("ASCI_D", "ASCI_H") & index_max >= 0.86 ~ "Class2",
      Index %in% c("ASCI_D", "ASCI_H") & index_max >= 0.75 ~ "Class3",
      Index %in% c("ASCI_D", "ASCI_H") & index_max < 0.75 ~ "Class4"
    )
  ) |>
  dplyr::group_by(StudyArea, Index, Class) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = Class, values_from = n, values_fill = 0)

high_scoring_sites_stack_data <- test_data_scores_summary |>
  tidyr::pivot_longer(cols = c(Class1, Class2, Class3, Class4)) |>
  dplyr::mutate(StudyArea = dplyr::if_else(StudyArea == "Modoc", "Modoc Pleateau", StudyArea)) |>
  dplyr::group_by(Index, StudyArea) |>
  dplyr::mutate(n_tot = sum(value)) |>
  dplyr::ungroup() |>
  dplyr::mutate(percent_sites = 100 * value / n_tot)

high_scoring_sites_stack <- ggplot2::ggplot(
    data = high_scoring_sites_stack_data, 
    ggplot2::aes(x = Index, y = percent_sites, fill = name)
  ) +
  ggplot2::geom_bar(position = ggplot2::position_stack(), stat = "identity", color = "gray20") +
  ggplot2::facet_wrap(ggplot2::vars(StudyArea), scales = "free_y") +
  ggplot2::scale_fill_brewer(palette = "Reds", name = "Max Score", labels = c(">30th", ">10th", ">1st","<1st")) +
  ggplot2::ylab("% sites") +
  ggplot2::theme_bw()

ggplot2::ggsave(high_scoring_sites_stack, filename = "figures/Part_1_Figure_07.jpg", dpi = 300, height = 4, width = 6)
