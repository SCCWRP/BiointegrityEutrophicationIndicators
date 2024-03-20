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

index_thresholds <- tidyr::crossing(
    name = c("CSCI", "ASCI_D", "ASCI_H"),
    lev = c("50th", "30th", "10th", "1st"),
    value = 1
  ) |>
  dplyr::mutate(
    value = dplyr::case_when(
      name == "CSCI" & lev == "30th" ~ 0.92,
      name == "CSCI" & lev == "10th" ~ 0.79,
      name == "CSCI" & lev == "1st" ~ 0.63,
      name != "CSCI" & lev == "30th" ~ 0.94,
      name != "CSCI" & lev == "10th" ~ 0.86,
      name != "CSCI" & lev == "1st" ~ 0.75,
      .default = value
    )
  )

## Figure 10 ####
cv_ref_plot <- ggplot2::ggplot(data = plot_dat |> na.omit(), ggplot2::aes(y = value, x = stream)) +
  ggplot2::geom_point(ggplot2::aes(color = psa6c, shape = Source)) +
  ggplot2::geom_hline(data = index_thresholds, ggplot2::aes(yintercept = value), linetype = "dashed") +
  ggplot2::scale_y_continuous(limits = c(0, 1.25)) +
  ggplot2::facet_grid(cols = ggplot2::vars(name), drop = T, scales = "free_x", space = "free") +
  ggplot2::scale_color_brewer(name = "Ecoregion", palette = "Set1", labels = c("Chaparral", "Central Valley")) +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "", y = "Score") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave(cv_ref_plot, filename ="figures/Part_1_Figure_10.jpg", dpi = 300, height = 4, width = 6.5)


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
  tidyr::pivot_longer(cols = dplyr::all_of(env_vars), names_to = "EnvVar", values_to = "EnvResult") |>
  dplyr::filter(Index == "CSCI")

nat_env_correlations <- plot_dat_gis_nat |>
  dplyr::group_by(Index, EnvVar) |>
  dplyr::summarize(Cor = cor(IndexScore, EnvResult, use = "pairwise.complete", method = "pearson")) |>
  dplyr::ungroup() |>
  dplyr::mutate(rsq = Cor^2)

top5_nat <- nat_env_correlations |>
  dplyr::slice_max(rsq, n = 5) |> 
  dplyr::ungroup() |>
  dplyr::filter(Index == "CSCI") |>
  dplyr::pull(EnvVar)


csci_cvref_nat_plot <- ggplot2::ggplot(data = plot_dat_gis_nat, ggplot2::aes(x = EnvResult, y = IndexScore)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(ggplot2::vars(EnvVar), scales = "free_x", ncol = 5) +
  ggplot2::geom_smooth(method = lm) +
  ggplot2::geom_smooth(
    data = plot_dat_gis_nat |> 
      dplyr::filter(EnvVar %in% top5_nat), 
    method = lm, 
    color = "red", 
    se = F
  ) +  
  ggplot2::labs(x = "", y = "CSCI score") +
  ggplot2::theme_bw()

ggplot2::ggsave(csci_cvref_nat_plot, filename = "figures/Part_1_Figure_11.jpg", dpi = 300, width = 10, height = 6)
