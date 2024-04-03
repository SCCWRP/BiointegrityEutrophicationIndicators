library(dplyr)
library(ggplot2)

psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp")
huc08_sf <- sf::st_read("data-raw/Part_1_shapefiles/calwater_SWAMP3Code.shp")
huc10_sf <- sf::st_read("data-raw/Part_1_shapefiles/HUC10.shp")

eval_sf_sa <- readr::read_csv('data-raw/Part_1_eval_df.csv') |>
  sf::st_as_sf(
    coords = c("longitude", "latitude"), 
    remove = F,
    crs = 4326
  ) |>
  sf::st_transform(crs = sf::st_crs(psa_sf)) |>
  sf::st_intersection(psa_sf) |>
  mutate(
    FlowStatus = case_when(
      flowstatuscode %in% c("NP", "NPF") ~ "Non-perennial",
      flowstatuscode %in% c("P") ~ "Perennial",
      .default = "Unknown"
    )
  )


eval_sf_over <- eval_sf_sa |> 
  sf::st_join(huc08_sf) |>
  rename(HUC08 = SWAMP3CODE) |>
  sf::st_join(huc10_sf |> select(HUC10) |> sf::st_transform(crs = sf::st_crs(eval_sf_sa)))

huc10_summary <- eval_sf_over |>
  tibble::as_tibble() |>
  select(-geometry) |>
  group_by(HUC10) |>
  summarize(
    TotalEvals = length(FlowStatus),
    TotalP = sum(FlowStatus == "Perennial"),
    TotalNP = sum(FlowStatus == "Non-Perennial")
  ) |>
  ungroup() |>
  mutate(
    PctP = TotalP / TotalEvals,
    PctNP = TotalNP / TotalEvals
  )



huc10_sf2 <- huc10_sf |>
  left_join(huc10_summary) |>
  sf::st_intersection(psa_sf |> sf::st_transform(crs = sf::st_crs(huc10_sf))) 

huc10_map <- ggplot() +
  geom_sf(
    data = huc10_sf2 , 
    aes(fill=PctP), 
    color = NA
  ) +
  scale_fill_viridis_c(option = "plasma", direction = -1, name = "% perennial") +
  geom_sf(data = psa_sf, fill = NA, color = "black", size = 1) +
  theme_bw()+
  theme(axis.text = element_blank())

flow_status_map2 <- ggplot() +
  geom_sf(data = psa_sf) +
  geom_sf(data = eval_sf_sa |> filter(FlowStatus != "Unknown"), aes(color = FlowStatus), size = 0.5) +
  scale_color_viridis_d(option = "plasma", direction = -1, name = "Flow Status", begin = 0.1, end = 0.9) +
  geom_sf(data = psa_sf, fill = NA, color = "black", size = 1) +
  facet_wrap(vars(FlowStatus)) +
  theme_bw() +
  theme(axis.text = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 1)))

ggsave(
  ggpubr::ggarrange(flow_status_map2, huc10_map, ncol = 1, nrow = 2), 
  filename = "figures/Part_1_Figure_02.jpg", 
  dpi = 300, 
  height = 6, 
  width = 6
)
