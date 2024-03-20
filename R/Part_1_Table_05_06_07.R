cv_ref <- readr::read_csv("data-raw/Part_1_cv_ibi_ref_tbl.csv")

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

cv_ref_scores <- readr::read_csv("data-raw/Part_1_cv_ref_scores.csv") |>
  dplyr::arrange(masterid)

plot_dat <- cv_ref_scores |>
  dplyr::left_join(cv_ref_gis |> dplyr::select(masterid, psa6c, Source)) |>
  dplyr::left_join(cv_ref |> dplyr::select(masterid = stationcod, stream = waterbodyn)) |>
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

plot_dat <- plot_dat |> dplyr::arrange(psa6c, Source, stream)
plot_dat$stream <- factor(plot_dat$stream, levels = plot_dat$stream |> unique())

## Table 5 ####
table_5 <- plot_dat |>
  dplyr::group_by(masterid, name, psa6c, Source) |>
  dplyr::summarize(value = mean(value, na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::group_by(Level = "All sites", name) |>
  dplyr::summarize(MeanScore = mean(value, na.rm = T), n = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::bind_rows(
    plot_dat |>
      dplyr::group_by(masterid, name, psa6c, Source) |>
      dplyr::summarize(value = mean(value, na.rm = T)) |>
      dplyr::ungroup() |>
      dplyr::group_by(Level = psa6c, name) |>
      dplyr::summarize(MeanScore = mean(value, na.rm = T), n = dplyr::n()) |>
      dplyr::ungroup()
  ) |>
  dplyr::bind_rows(
    plot_dat |>
      dplyr::group_by(masterid, name, psa6c, Source) |>
      dplyr::summarize(value = mean(value, na.rm = T)) |>
      dplyr::ungroup() |>
      dplyr::group_by(Level = Source, name) |>
      dplyr::summarize(MeanScore = mean(value, na.rm = T), n = dplyr::n()) |>
      dplyr::ungroup()
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
  dplyr::select(masterid, Source, psa6c, stream, Index = name, IndexScore = value) |>
  unique() |>
  dplyr::group_by(masterid, Source, psa6c, stream, Index) |>
  dplyr::summarize(IndexScore = mean(IndexScore, na.rm=T)) |>
  dplyr::ungroup() |>
  dplyr::left_join(
    cv_ref_gis |>
      dplyr::mutate(logwsa = log10(area_sqkm)) |>
      dplyr::select(masterid, dplyr::all_of(env_vars)
      )
  ) |>
  tidyr::pivot_longer(cols = dplyr::all_of(env_vars), names_to = "EnvVar", values_to = "EnvResult")

nat_env_correlations <- plot_dat_gis_nat |>
  dplyr::group_by(Index, EnvVar) |>
  dplyr::summarize(Cor = cor(IndexScore, EnvResult, use = "pairwise.complete", method = "pearson")) |>
  dplyr::ungroup() |>
  dplyr::mutate(rsq = Cor^2) |>
  dplyr::filter(Index == "CSCI") |>
  dplyr::select(Variable = EnvVar, `Pearson's r` = Cor)

write.csv(nat_env_correlations, file = 'tables/Part_1_Table_06.csv', row.names = F)


## Table 7 ####

among_var <- cv_ref_scores |> 
  dplyr::group_by(masterid) |>
  dplyr::summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T)
  ) |>
  dplyr::ungroup() |>
  dplyr::summarize(
    CSCI = sd(CSCI, na.rm = T),
    ASCI_H = sd(ASCI_H, na.rm = T),
    ASCI_D = sd(ASCI_D, na.rm = T)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(Type = "CV_Ref") |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index", values_to = "SD_amongRef") |>
  dplyr::bind_rows(
    tibble::tibble(
      Type = c("Ref"),
      Index = c("CSCI", "ASCI_D","ASCI_H"),
      SD_amongRef = c(0.16, 0.11, 0.11)
    )
  ) |>
  tidyr::pivot_wider(names_from = Type, values_from = SD_amongRef)

write.csv(among_var, file = "tables/Part_1_Table_07.csv", row.names = F)
