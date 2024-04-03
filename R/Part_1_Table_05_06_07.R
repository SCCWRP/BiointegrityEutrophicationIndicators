library(dplyr)

cv_ref <- readr::read_csv("data-raw/Part_1_cv_ibi_ref_tbl.csv")

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

## Table 5 ####
table_5 <- plot_dat |>
  group_by(masterid, name, psa6c, Source) |>
  summarize(value = mean(value, na.rm = T)) |>
  ungroup() |>
  group_by(Level = "All sites", name) |>
  summarize(MeanScore = mean(value, na.rm = T), n = n()) |>
  ungroup() |>
  bind_rows(
    plot_dat |>
      group_by(masterid, name, psa6c, Source) |>
      summarize(value = mean(value, na.rm = T)) |>
      ungroup() |>
      group_by(Level = psa6c, name) |>
      summarize(MeanScore = mean(value, na.rm = T), n = n()) |>
      ungroup()
  ) |>
  bind_rows(
    plot_dat |>
      group_by(masterid, name, psa6c, Source) |>
      summarize(value = mean(value, na.rm = T)) |>
      ungroup() |>
      group_by(Level = Source, name) |>
      summarize(MeanScore = mean(value, na.rm = T), n = n()) |>
      ungroup()
  ) |>
  tidyr::pivot_wider(names_from = name, values_from = MeanScore)

write.csv(table_5, file = "tables/Part_1_Table_05.csv", row.names = F)


## Table 6 ####
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
  tidyr::pivot_longer(cols = all_of(env_vars), names_to = "EnvVar", values_to = "EnvResult")

table_6 <- plot_dat_gis_nat |>
  group_by(Index, EnvVar) |>
  summarize(Cor = cor(IndexScore, EnvResult, use = "pairwise.complete", method = "pearson")) |>
  ungroup() |>
  mutate(rsq = Cor^2) |>
  filter(Index == "CSCI") |>
  select(Variable = EnvVar, `Pearson's r` = Cor)

write.csv(table_6, file = 'tables/Part_1_Table_06.csv', row.names = F)


## Table 7 ####

table_7 <- cv_ref_scores |> 
  group_by(masterid) |>
  summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T)
  ) |>
  ungroup() |>
  summarize(
    CSCI = sd(CSCI, na.rm = T),
    ASCI_H = sd(ASCI_H, na.rm = T),
    ASCI_D = sd(ASCI_D, na.rm = T)
  ) |>
  ungroup() |>
  mutate(Type = "CV_Ref") |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index", values_to = "SD_amongRef") |>
  bind_rows(
    tibble::tibble(
      Type = c("Ref"),
      Index = c("CSCI", "ASCI_D","ASCI_H"),
      SD_amongRef = c(0.16, 0.11, 0.11)
    )
  ) |>
  tidyr::pivot_wider(names_from = Type, values_from = SD_amongRef)

write.csv(table_7, file = "tables/Part_1_Table_07.csv", row.names = F)


## Mean within-site CSCI variability ####
mean_within_site_CSCI_var <- cv_ref_scores |>
  left_join(cv_ref_gis |> select(masterid, psa6c, Source)) |>
  left_join(cv_ref |> select(masterid = stationcod, stream = waterbodyn)) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index", values_to = "IndexScore", values_drop_na = T) |>
  group_by(masterid, Index) |>
  summarize(IndexScore_sd = sd(IndexScore, na.rm = T)) |>
  ungroup() |>
  na.omit() |>
  summarize(mean_within_site_CSCI_var = mean(IndexScore_sd))
