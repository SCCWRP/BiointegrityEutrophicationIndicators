library(dplyr)
library(ggplot2)

psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp")
studyarea_sf <- sf::st_read("data-raw/Part_1_shapefiles/RB5_StudyAreas.shp")

test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  group_by(masterid) |>
  slice_head(n = 1) |>
  ungroup()

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv")

## Figure 6 ####

test_data_scores_sf <- test_data_scores |>
  left_join(test_data_gis |> select(masterid, StudyArea, RefStatus, new_lat, new_long)) |>
  group_by(masterid, StudyArea, new_lat, new_long) |>
  summarize(
    CSCI_max = max(CSCI, na.rm = T),
    ASCI_D_max = max(ASCI_D, na.rm = T),
    ASCI_H_max = max(ASCI_H, na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    CSCI = case_when(
      CSCI_max >= 0.92 ~ "Class1",
      CSCI_max >= 0.79 ~ "Class2",
      CSCI_max >= 0.63 ~ "Class3",
      CSCI_max < 0.63 ~ "Class4",
      .default = NA_character_
    ),
    ASCI_D = case_when(
      ASCI_D_max >= 0.94 ~ "Class1",
      ASCI_D_max >= 0.86 ~ "Class2",
      ASCI_D_max >= 0.75 ~ "Class3",
      ASCI_D_max < 0.75 ~ "Class4",
      .default = NA_character_
    ),
    ASCI_H = case_when(
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

hi_scoring_stes_map <- ggplot() +
  geom_sf(data = psa_sf) +
  geom_sf(
    data = test_data_scores_sf |> 
      filter(Class %in% c("Class1", "Class2")), 
    aes(color = Class, shape = Class)
  ) +
  facet_wrap(vars(Index)) +
  scale_color_manual(values = c("#377eb8", "#4daf4a"), name = "Max Score", labels = c(">30th", ">10th")) +
  scale_shape_discrete(name = "Max Score", labels = c(">30th", ">10th")) +
  theme_bw() +
  theme(axis.text = element_blank(), legend.position = "bottom")

ggsave(hi_scoring_stes_map, filename = "figures/Part_1_Figure_06.jpg", dpi = 300, height = 4, width = 6.5)

## Figure 7 ####

test_data_scores_summary <- test_data_scores |>
  left_join(test_data_gis |> select(masterid, StudyArea)) |>
  select(masterid, StudyArea, CSCI, ASCI_D, ASCI_H) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index") |>
  na.omit() |>
  group_by(masterid, StudyArea, Index) |>
  summarize(
    index_n = length(value),
    index_mean = mean(value),
    index_max = max(value)
  ) |>
  ungroup() |>
  mutate(
    Class = case_when(
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
  group_by(StudyArea, Index, Class) |>
  tally() |>
  tidyr::pivot_wider(names_from = Class, values_from = n, values_fill = 0) 

high_scoring_sites_stack_data <- test_data_scores_summary |>
  tidyr::pivot_longer(cols = c(Class1, Class2, Class3, Class4)) |>
  mutate(StudyArea = if_else(StudyArea == "Modoc", "Modoc Pleateau", StudyArea)) |>
  group_by(Index, StudyArea) |>
  mutate(n_tot = sum(value)) |>
  ungroup() |>
  mutate(percent_sites = 100 * value / n_tot)

high_scoring_sites_stack <- ggplot(
    data = high_scoring_sites_stack_data, 
    aes(x = Index, y = percent_sites, fill = name)
  ) +
  geom_bar(position = position_stack(), stat = "identity", color = "gray20") +
  facet_wrap(vars(StudyArea), scales = "free_y") +
  scale_fill_brewer(palette = "Reds", name = "Max Score", labels = c(">30th", ">10th", ">1st","<1st")) +
  ylab("% sites") +
  theme_bw()

ggsave(high_scoring_sites_stack, filename = "figures/Part_1_Figure_07.jpg", dpi = 300, height = 4, width = 6)
