library(dplyr)
library(ggplot2)

test_data_gis_sf <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  mutate(logwsa = log10(area_sqkm)) |>
  sf::st_as_sf(coords = c("new_long", "new_lat"), crs = 4326)

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

cvf_map <- base_map +
  geom_sf(data = test_data_gis_sf, size = 1) +
  facet_wrap(~StudyArea, nrow = 2)

ggsave(cvf_map, filename = "figures/Part_S1_Figure_S1-1.jpg", height = 6, width = 5)