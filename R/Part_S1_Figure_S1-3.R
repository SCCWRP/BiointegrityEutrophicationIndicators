library(dplyr)
library(ggplot2)

rb_sf <- sf::st_read("data-raw/Part_1_shapefiles/rwqcbnda.shp")
psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp") |>
  mutate(
    PSA6 = factor(
      x = PSA6, 
      levels = c(
        "Chaparral", "Central Valley", "South Coast", 
        "Deserts Modoc", "North Coast", "Sierra Nevada"
      )
    )
  )

psa_pal <- c("#fec44f", "#fe9929", "#d95f0e", "#fee391", "#9ebcda", "#8c96c6")

base_map <- ggplot() +
  geom_sf(data = psa_sf, aes(fill = PSA6), color = NA) +
  scale_fill_manual(values = psa_pal, name = "Ecoregion") +
  geom_sf(data = rb_sf, fill = NA, color = "gray50") +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

chan_sf <- readr::read_csv("data-raw/Part_2_combined_df_withdata.csv") |>
  select(masterid, class_do, Watershed, CSCI, ASCI_H, ASCI_D, latitude, longitude) |>
  mutate(
    class = case_when(
      !is.na(Watershed) ~ "Constructed channel",
      .default = class_do
    )
  ) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

chan_map <- base_map +
  geom_sf(data = chan_sf |> filter(!is.na(class_do)), size = 1) +
  facet_wrap(~class_do, ncol = 2)

ggsave(chan_map, filename = "figures/Part_S1_Figure_S1-3.jpg", height = 8, width = 6)