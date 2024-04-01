library(dplyr)
library(ggplot2)

cv_ref <- readr::read_csv("data-raw/Part_1_cv_ibi_ref_tbl.csv")

psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp")

cv_ref_gis <- readr::read_csv("data-raw/Part_1_cv_ref_gis.csv") |>
  arrange(masterid) |>
  mutate(
    Source = case_when(
      masterid %in% cv_ref$stationcod ~ "CV-IBI",
      masterid %in% c("504PS0227", "504FC1115") ~ "CSCI",
      masterid %in% c("541OC0010", "541OSCHW5") ~ "USGS",
      .default = "Other"
    )
  )


cv_ref_scores <- readr::read_csv("data-raw/Part_1_cv_ref_scores.csv") |>
  arrange(masterid)

plot_dat <- cv_ref_scores |>
  left_join(cv_ref_gis |> select(masterid, psa6c, Source)) |>
  left_join(cv_ref |> select(masterid = stationcod, stream = waterbodyn)) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H))

plot_dat$stream[plot_dat$masterid == "541OC0010"] <- "Orestimba Creek"
plot_dat$stream[plot_dat$masterid == "541OSCHW5"] <- "Orestimba Creek"
plot_dat$stream[plot_dat$masterid == "504DRCH99"] <- "Deer Creek 1"
plot_dat$stream[plot_dat$masterid == "504PS0227"] <- "Deer Creek 2"
plot_dat$stream[plot_dat$masterid == "504FC1115"] <- "Deer Creek 3"
plot_dat$stream[plot_dat$masterid == "504DCFRxx"] <- "Dye Creek 1"
plot_dat$stream[plot_dat$masterid == "504DYCBSB"] <- "Dye Creek 2"
plot_dat$stream[plot_dat$masterid == "515DCBAFB"] <- "Dry Creek 1"
plot_dat$stream[plot_dat$masterid == "520DCDCRx"] <- "Dry Creek 2"
plot_dat$Source <- factor(plot_dat$Source, levels = c("CV-IBI", "CSCI", "USGS"))

plot_dat <- plot_dat |> arrange(psa6c, Source, stream)
plot_dat$stream <- factor(plot_dat$stream, levels = plot_dat$stream |> unique())

index_thresholds <- tidyr::crossing(
    name = c("CSCI", "ASCI_D", "ASCI_H"),
    lev = c("50th", "30th", "10th", "1st"),
    value = 1
  ) |>
  mutate(
    value = case_when(
      name == "CSCI" & lev == "30th" ~ 0.92,
      name == "CSCI" & lev == "10th" ~ 0.79,
      name == "CSCI" & lev == "1st" ~ 0.63,
      name != "CSCI" & lev == "30th" ~ 0.94,
      name != "CSCI" & lev == "10th" ~ 0.86,
      name != "CSCI" & lev == "1st" ~ 0.75,
      .default = value
    )
  )

## Figure 09 ####
cv_ref_gis_sf <- cv_ref_gis |>
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = F, crs = 4326)


cv_ref_location_map <- ggplot() +
  geom_sf(data = psa_sf) +
  geom_sf(data = cv_ref_gis_sf) +
  theme_bw() +
  theme(axis.text = element_blank())

ggsave(cv_ref_location_map, filename = "figures/Part_1_Figure_09.jpg", dpi = 300, height = 4, width = 4)


## Figure 10 ####
cv_ref_plot <- ggplot(data = plot_dat |> na.omit(), aes(y = value, x = stream)) +
  geom_point(aes(color = psa6c, shape = Source)) +
  geom_hline(data = index_thresholds, aes(yintercept = value), linetype = "dashed") +
  scale_y_continuous(limits = c(0, 1.25)) +
  facet_grid(cols = vars(name), drop = T, scales = "free_x", space = "free") +
  scale_color_brewer(name = "Ecoregion", palette = "Set1", labels = c("Chaparral", "Central Valley")) +
  theme_bw() +
  labs(x = "", y = "Score") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave(cv_ref_plot, filename ="figures/Part_1_Figure_10.jpg", dpi = 300, height = 4, width = 6.5)


## Figure 11 ####
env_vars<- tolower(
  c(
    "New_Lat", "New_Long", "SITE_ELEV", "logwsa", "ELEV_RANGE",
    "PPT_00_09", "SumAve_P", "MAXWD_WS", "MINP_WS", "TEMP_00_09",
    "TMAX_WS", "KFCT_AVE", "BDH_AVE", "LPREM_mean", "PRMH_AVE",
    "UCS_Mean", "P_MEAN", "S_Mean", "MgO_Mean", "CaO_Mean"
  )
)

plot_dat_gis_nat <- plot_dat |>
  select(masterid, Source, psa6c, stream, Index = name, IndexScore = value) |>
  unique() |>
  group_by(masterid, Source, psa6c, stream, Index) |>
  summarize(IndexScore = mean(IndexScore, na.rm=T)) |>
  ungroup() |>
  left_join(
    cv_ref_gis |>
      mutate(logwsa = log10(area_sqkm)) |>
      select(masterid, all_of(env_vars)
      )
  ) |>
  tidyr::pivot_longer(cols = all_of(env_vars), names_to = "EnvVar", values_to = "EnvResult") |>
  filter(Index == "CSCI")

nat_env_correlations <- plot_dat_gis_nat |>
  group_by(Index, EnvVar) |>
  summarize(Cor = cor(IndexScore, EnvResult, use = "pairwise.complete", method = "pearson")) |>
  ungroup() |>
  mutate(rsq = Cor^2)

top5_nat <- nat_env_correlations |>
  slice_max(rsq, n = 5) |> 
  ungroup() |>
  filter(Index == "CSCI") |>
  pull(EnvVar)


csci_cvref_nat_plot <- ggplot(data = plot_dat_gis_nat, aes(x = EnvResult, y = IndexScore)) +
  geom_point() +
  facet_wrap(vars(EnvVar), scales = "free_x", ncol = 5) +
  geom_smooth(method = lm) +
  geom_smooth(
    data = plot_dat_gis_nat |> 
      filter(EnvVar %in% top5_nat), 
    method = lm, 
    color = "red", 
    se = F
  ) +  
  labs(x = "", y = "CSCI score") +
  theme_bw()

ggsave(csci_cvref_nat_plot, filename = "figures/Part_1_Figure_11.jpg", dpi = 300, width = 10, height = 6)
