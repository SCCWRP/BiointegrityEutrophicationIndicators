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

nonperen_sf <- readr::read_csv("data-raw/Part_S1_Andy_Nonperen_data_ASCI_CL_11_4_2022.csv") |>
  mutate(
    FlowStatus = case_when(
      Flow_SOP == "RFI_SFI" ~ "SFI",
      Flow_SOP == "RI" ~ "RFI",
      .default = Flow_SOP
    ),
    FlowStatus_label = case_when(
      FlowStatus == "P" ~ "Perennial",
      FlowStatus == "RFI" ~ "Regularly flowing intermittent",
      FlowStatus == "SFI" ~ "Seldom flowing intermittent",
      .default = "Other"
    )
  ) |>
  sf::st_as_sf(coords = c("New_Long", "New_Lat"), crs = 4326)

intermittent_map <- base_map +
  geom_sf(data = nonperen_sf, size = 1) +
  facet_wrap(~FlowStatus, nrow = 3)

ggsave(intermittent_map, filename ="figures/Part_S1_Figure_S1-2.jpg", height = 8, width = 5)
