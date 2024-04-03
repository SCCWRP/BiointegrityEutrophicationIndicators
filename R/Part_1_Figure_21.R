library(dplyr)
library(ggplot2)

rb_sf <- sf::st_read("data-raw/Part_1_shapefiles/rwqcbnda.shp")

nonperen_df <- readr::read_csv("data-raw/Part_1_Andy_Nonperen_data_ASCI.csv") |>
  mutate(
    masterid = StationCode,
    sampledate = lubridate::mdy(SampleDate)
  )

plot_dat <- nonperen_df |>
  mutate(Flow_SOP2 = if_else(Flow_SOP == "RFI_SFI", "SFI", Flow_SOP)) |>
  group_by(RB, StationCode, Flow_SOP, Flow_SOP2) |>
  summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T)) |>
  ungroup() |>
  mutate(Region = if_else(RB %in% c(4,7,8,9), "Southern", "Northern"))

plot_dat_sf <- plot_dat |>
  select(-CSCI) |>
  unique() |>
  inner_join(
    nonperen_df |>
      select(StationCode, New_Lat, New_Long) |>
      na.omit() |>
      group_by(StationCode) |>
      slice_head(n = 1) |>
      ungroup()
  ) |>
  sf::st_as_sf(coords = c("New_Long", "New_Lat"), remove = F, crs = 4326) |>
  sf::st_transform(crs = sf::st_crs(rb_sf))


map_int_refsites <- ggplot() +
  geom_sf(data = rb_sf) +
  geom_sf(data = rb_sf |> filter(RB %in% c(4, 8, 7, 9)), color = NA, fill = "gray50") +
  geom_sf(data = rb_sf |> filter(RB %in% c(1, 2, 3, 5, 6)), color = NA, fill = "gray80") +
  geom_sf(data = rb_sf, fill = NA, color = "black") +
  geom_sf(data = plot_dat_sf, aes(fill = Flow_SOP2, shape = Flow_SOP2)) +
  geom_sf(
    data = plot_dat_sf|> filter(Flow_SOP2 == "SFI"), 
    aes(fill = Flow_SOP2, shape = Flow_SOP2)
  ) +
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Flow status") +
  scale_shape_manual(values = c(21, 24, 25), name = "Flow status") +
  theme_bw() +
  theme(axis.text = element_blank())

ggsave(map_int_refsites, filename = "figures/Part_1_Figure_21.jpg", dpi = 300, height = 4, width = 5)
