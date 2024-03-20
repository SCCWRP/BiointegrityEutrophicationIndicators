msid.lu <- readr::read_csv("data-raw/Part_1_lu_masterid.csv")

env_vars <- c(
  "new_lat", "new_long", "site_elev", "logwsa", "elev_range", "ppt_00_09", "sumave_p",
  "maxwd_ws", "meanp_ws", "minp_ws", "xwd_ws", "temp_00_09", "tmax_ws", "atmca", 
  "atmmg", "atmso4", "kfct_ave", "bdh_ave", "lprem_mean", "prmh_ave", "ucs_mean",
  "p_mean", "s_mean", "mgo_mean", "cao_mean"
)

univariate_env_vars <- c(
  "logwsa", "site_elev","elev_range",
  "ppt_00_09","maxwd_ws", "temp_00_09",
  "kfct_ave","prmh_ave","ucs_mean",
  "s_mean","cao_mean","p_mean"
)

test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  dplyr::rename_with(.fn = tolower) |>
  dplyr::mutate(logwsa = log10(area_sqkm))

csci_dev_gis <- readr::read_csv("data-raw/Part_1_stations.out.csv") |> 
  dplyr::rename_with(.fn = tolower)

asci_dev_gis <- readr::read_csv("data-raw/Part_1_asci_caldata.csv") |> 
  dplyr::rename_with(.fn = tolower)

biostim_dev_gis <- readr::read_csv("data-raw/Part_1_biostim_input_data.csv") |>
  dplyr::mutate(logwsa = log10(AREA_SQKM)) |>
  dplyr::rename_with(.fn = tolower)

all.gis <- readr::read_csv("data-raw/Part_1_all.gis.cleaned.csv") |>
  dplyr::mutate(logwsa = log10(area_sqkm)) |> # Get all GIS data imported in order to merge with ASCI data 
  dplyr::group_by(masterid) |> 
  dplyr::slice_head(n=1) |>
  dplyr::ungroup()


test_data_gis2 <- test_data_gis |>
  dplyr::select(masterid, dplyr::all_of(env_vars), sitestatus = refstatus, studyarea) |>
  unique() |>
  dplyr::mutate(Type = "Test", Type2 = "Test")

csci_dev_gis2 <- csci_dev_gis |>
  dplyr::select(masterid = stationcode, dplyr::all_of(env_vars), sitestatus) |> 
  unique() |>
  dplyr::mutate(
    studyarea = dplyr::case_when(sitestatus == "Reference" ~ "Reference", .default = "Non-reference"), 
    Type="CSCI cal", 
    Type2="Cal"
  )



asci_dev_gis2 <- asci_dev_gis |>
  dplyr::left_join(msid.lu) |>
  dplyr::mutate(masterid = dplyr::if_else(is.na(masterid), stationcode, masterid)) |>
  dplyr::inner_join(all.gis) |>
  dplyr::select(dplyr::all_of(env_vars), masterid, RefStatus) |>
  dplyr::rename(sitestatus = RefStatus) |>
  unique() |>
  dplyr::mutate(
    studyarea = dplyr::if_else(sitestatus == "Reference", "Reference", "Non-reference"), 
    Type = "ASCI cal", 
    Type2 = "Cal"
  )


biostim_dev_gis2 <- biostim_dev_gis |>
  dplyr::select(masterid = stationcode, dplyr::all_of(env_vars), sitestatus) |> 
  unique() |>
  dplyr::mutate(
    studyarea = dplyr::if_else(sitestatus == "Reference", "Reference", "Non-reference"), 
    Type = "Biostim cal", 
    Type2 = "Cal"
  )


csci_caltest <- csci_dev_gis2 |> 
  dplyr::bind_rows(test_data_gis2) 

asci_caltest <- asci_dev_gis2 |> 
  dplyr::bind_rows(test_data_gis2) |> 
  dplyr::filter(Type2 == "Cal")

biostim_caltest <- biostim_dev_gis2 |> 
  dplyr::bind_rows(test_data_gis2) |> 
  dplyr::filter(Type2 == "Cal")

all_cal_test <- dplyr::bind_rows(csci_caltest, asci_caltest, biostim_caltest) |>
  dplyr::mutate(
    Type.x = dplyr::case_when(
      Type == "CSCI cal" ~ "CSCI",
      Type == "ASCI cal" ~ "ASCI",
      Type == "Biostim cal" ~ "Biostim",
      .default = Type
    ),
    Type.x = factor(Type.x, levels = c("CSCI", "ASCI", "Biostim", "Test"))
  ) 

## Figure 3 ####

univariate_plot_data <- all_cal_test |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(env_vars), 
    names_to = "EnvVar", 
    values_to = "EnvResult"
  ) |>
  dplyr::filter(EnvVar %in% univariate_env_vars) |>
  dplyr::mutate(EnvVar = factor(EnvVar, levels = univariate_env_vars))



univariate_gradients_plot <- ggplot2::ggplot(
    data = univariate_plot_data, 
    ggplot2::aes(x = Type.x, y = EnvResult)
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = studyarea), 
    shape = 21, 
    position = ggplot2::position_jitterdodge(jitter.height = 0, jitter.width = 0.25), size = 1
  ) +
  ggplot2::facet_wrap(ggplot2::vars(EnvVar), scales = "free_y", ncol = 3) +
  ggplot2::scale_fill_viridis_d(name = "Site set") +
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_bw()+
  ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size=2)))+
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(
  univariate_gradients_plot, 
  filename = "figures/Part_1_Figure_03.jpg", 
  dpi = 300, 
  height = 8, 
  width = 6.5
)

## Figure 4 ####

all_data_slope_fines <- readr::read_csv("data-raw/Part_1_all_data_slope_fines.csv")

csci_cal_slope_fines <- all_data_slope_fines |> 
  dplyr::filter(masterid %in% csci_dev_gis$stationcode) |> 
  dplyr::mutate(Type.x = "CSCI") |>
  dplyr::inner_join(csci_dev_gis2 |> dplyr::select(masterid, studyarea))

asci_cal_slope_fines <- all_data_slope_fines |> 
  dplyr::filter(masterid %in% asci_dev_gis$stationcode) |> 
  dplyr::mutate(Type.x = "ASCI") |>
  dplyr::inner_join(asci_dev_gis2 |> dplyr::select(masterid, studyarea))

biostim_cal_slope_fines <- all_data_slope_fines |> 
  dplyr::filter(masterid %in% biostim_dev_gis$masterid) |> 
  dplyr::mutate(Type.x = "Biostim") |>
  dplyr::inner_join(biostim_dev_gis2 |> dplyr::select(masterid, studyarea))

test_data_slope_fines <- all_data_slope_fines |> 
  dplyr::filter(masterid %in% test_data_gis$masterid) |>  
  dplyr::mutate(Type.x = "Test") |>
  dplyr::inner_join(test_data_gis |> dplyr::select(masterid, studyarea))

slope_fines_plot_data <- univariate_plot_data |>
  dplyr::select(masterid, sitestatus, studyarea, Type.x) |>
  unique() |>
  dplyr::inner_join(csci_cal_slope_fines) |>
  dplyr::bind_rows(
    univariate_plot_data |>
      dplyr::select(masterid, sitestatus, studyarea, Type.x) |>
      unique() |>
      dplyr::inner_join(asci_cal_slope_fines) 
  ) |>
  dplyr::bind_rows(
    univariate_plot_data |>
      dplyr::select(masterid, sitestatus, studyarea, Type.x) |>
      unique() |>
      dplyr::inner_join(biostim_cal_slope_fines) 
  ) |>
  dplyr::bind_rows(
    univariate_plot_data |>
      dplyr::select(masterid, sitestatus, studyarea, Type.x) |>
      unique() |>
      dplyr::inner_join(test_data_slope_fines) 
  ) |>
  tidyr::pivot_longer(
    cols = c(XSLOPE, PCT_FN, PCT_HP), 
    names_to = "EnvVar", 
    values_to ="EnvResult", 
    values_drop_na = T
  ) |>
  dplyr::mutate(
    EnvVar2 = dplyr::case_when(
      EnvVar == "PCT_FN" ~ "% fines",
      EnvVar == "PCT_HP" ~ "% hardpan",
      EnvVar == "XSLOPE" ~ "slope",
      .default = "Other"
    ),
    Type.x = factor(Type.x, levels=c("CSCI", "ASCI", "Biostim", "Test"))
  )

slope_fines_plot <- ggplot2::ggplot(
  data = slope_fines_plot_data |>
    dplyr::filter(EnvVar != "PCT_HP"), 
    ggplot2::aes(x = Type.x, y = EnvResult)
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = studyarea), 
    shape = 21, 
    position = ggplot2::position_jitterdodge(jitter.height = 0, jitter.width = 0.25), 
    size = 1
  )+
  ggplot2::facet_wrap(ggplot2::vars(EnvVar2), scales = "free", ncol = 3) +
  ggplot2::scale_fill_viridis_d(name = "Site set")+
  ggplot2::labs(x = "", y = "") +
  ggplot2::theme_bw() +
  ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 2))) +
  ggplot2::theme(legend.position = "bottom")

ggplot2::ggsave(slope_fines_plot, filename ="figures/Part_1_Figure_04.jpg", height = 4, width = 6, dpi = 300)


## Figure 5 ####

nat_vars <- setdiff(env_vars, c("new_lat","new_long")) #Exclude "location" variables
pca_dat1 <- all_cal_test |>
  dplyr::filter(!is.na(minp_ws)) |>
  tidyr::pivot_longer(cols = dplyr::all_of(env_vars), names_to = "EnvVar", values_to = "EnvResult") |>
  dplyr::filter(EnvVar %in% nat_vars) |>
  tidyr::pivot_wider(names_from = EnvVar, values_from = EnvResult) |>
  dplyr::mutate(studyarea2 = dplyr::if_else(studyarea %in% c("Reference", "Non-reference"), "Calibration", studyarea))


pca_dat_csci <- pca_dat1 |> dplyr::filter(Type.x %in% c("CSCI", "Test"))
pca_dat_csci_matrix <- pca_dat_csci |> dplyr::select(dplyr::all_of(nat_vars))
nat_csci_pca <- prcomp(pca_dat_csci_matrix, scale = T, retx = T,center = T)
colnames(nat_csci_pca$x) <- paste("Nat", colnames(nat_csci_pca$x), sep = ".")
pca_dat_csci <- cbind(pca_dat_csci, nat_csci_pca$x)

nat_csci_pca_loadings <- data.frame(nat_csci_pca$rotation)
nat_csci_pca_loadings$Var <- nat_vars

pca_dat_asci <- pca_dat1 |> dplyr::filter(Type.x %in% c("ASCI", "Test"))
pca_dat_asci_matrix <- pca_dat_asci |> dplyr::select(dplyr::all_of(nat_vars))
nat_asci_pca <- prcomp(pca_dat_asci_matrix, scale = T, retx = T,center = T)
colnames(nat_asci_pca$x) <- paste("Nat", colnames(nat_asci_pca$x), sep = ".")
pca_dat_asci <- cbind(pca_dat_asci, nat_asci_pca$x)

nat_asci_pca_loadings <- data.frame(nat_asci_pca$rotation)
nat_asci_pca_loadings$Var <- nat_vars


pca_dat_biostim <- pca_dat1 |> dplyr::filter(Type.x %in% c("Biostim", "Test"))
pca_dat_biostim_matrix <- pca_dat_biostim |> dplyr::select(dplyr::all_of(nat_vars))
nat_biostim_pca <- prcomp(pca_dat_biostim_matrix, scale = T, retx = T,center = T)
colnames(nat_biostim_pca$x) <- paste("Nat", colnames(nat_biostim_pca$x), sep = ".")
pca_dat_biostim <- cbind(pca_dat_biostim, nat_biostim_pca$x)

nat_biostim_pca_loadings <- data.frame(nat_biostim_pca$rotation)
nat_biostim_pca_loadings$Var <- nat_vars



#Add min convex polygons around cal data, but don't plot those sites.
PC_hull_csci <- pca_dat_csci |> 
  dplyr::group_by(studyarea) |>
  dplyr::slice(chull(Nat.PC1, Nat.PC2))

PC_hull_asci <- pca_dat_asci |>
  dplyr::group_by(studyarea) |>
  dplyr::slice(chull(Nat.PC1, Nat.PC2))

PC_hull_biostim <- pca_dat_biostim |>
  dplyr::group_by(studyarea) |>
  dplyr::slice(chull(Nat.PC1, Nat.PC2))

pca_dat_csci$studyarea2 <- factor(pca_dat_csci$studyarea2, levels=c("Central Valley", "Modoc", "Calibration"))
pca_dat_asci$studyarea2 <- factor(pca_dat_asci$studyarea2, levels=c("Central Valley", "Modoc", "Calibration"))
pca_dat_biostim$studyarea2 <- factor(pca_dat_biostim$studyarea2, levels=c("Central Valley", "Modoc", "Calibration"))

csci_pca_plot <- ggplot2::ggplot(data = pca_dat_csci, ggplot2::aes(x = Nat.PC1, y = Nat.PC2))+
  ggplot2::geom_polygon(
    data = PC_hull_csci |> dplyr::filter(studyarea %in% c("Reference","Non-reference")), 
    ggplot2::aes(fill = studyarea), 
    alpha = 0.5
  ) +
  ggplot2::geom_point(size = 0.5, color = "gray")+
  ggplot2::geom_point(data = pca_dat_csci, ggplot2::aes(color = studyarea2, size = studyarea2))+
  ggplot2::geom_point(
    data = pca_dat_csci |> dplyr::filter(Type == "Test"), 
    ggplot2::aes(color = studyarea2, size = studyarea2)
  ) +
  ggplot2::geom_segment(data = nat_csci_pca_loadings, ggplot2::aes(xend = PC1 * 9, yend = PC2 * 9, x = 0, y = 0)) +
  ggplot2::geom_text(data = nat_csci_pca_loadings, ggplot2::aes(x = PC1 * 10, y = PC2 * 10, label = Var)) +
  ggplot2::scale_fill_brewer(palette = "Set1", name = "Site set") +
  ggplot2::scale_color_manual(values = c("#440154FF", "#FDE725FF", "gray"), name = "Study Area") +
  ggplot2::scale_size_manual(values = c(1.5, 1.5, 0.5), name = "Study Area") +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "PC 1", y = "PC 2")

asci_pca_plot <- ggplot2::ggplot(data = pca_dat_asci, ggplot2::aes(x = Nat.PC1, y = Nat.PC2))+
  ggplot2::geom_polygon(
    data = PC_hull_asci |> dplyr::filter(studyarea %in% c("Reference","Non-reference")), 
    ggplot2::aes(fill = studyarea), 
    alpha = 0.5
  ) +
  ggplot2::geom_point(size = 0.5, color = "gray")+
  ggplot2::geom_point(data = pca_dat_asci, ggplot2::aes(color = studyarea2, size = studyarea2))+
  ggplot2::geom_point(
    data = pca_dat_asci |> dplyr::filter(Type == "Test"), 
    ggplot2::aes(color = studyarea2, size = studyarea2)
  ) +
  ggplot2::geom_segment(data = nat_asci_pca_loadings, ggplot2::aes(xend = PC1 * 9, yend = PC2 * 9, x = 0, y = 0)) +
  ggplot2::geom_text(data = nat_asci_pca_loadings, ggplot2::aes(x = PC1 * 10, y = PC2 * 10, label = Var)) +
  ggplot2::scale_fill_brewer(palette = "Set1", name = "Site set") +
  ggplot2::scale_color_manual(values = c("#440154FF", "#FDE725FF", "gray"), name = "Study Area") +
  ggplot2::scale_size_manual(values = c(1.5, 1.5, 0.5), name = "Study Area") +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "PC 1", y = "PC 2")

biostim_pca_plot <- ggplot2::ggplot(data = pca_dat_biostim, ggplot2::aes(x = Nat.PC1, y = Nat.PC2))+
  ggplot2::geom_polygon(
    data = PC_hull_biostim |> dplyr::filter(studyarea %in% c("Reference","Non-reference")), 
    ggplot2::aes(fill = studyarea), 
    alpha = 0.5
  ) +
  ggplot2::geom_point(size = 0.5, color = "gray")+
  ggplot2::geom_point(data = pca_dat_biostim, ggplot2::aes(color = studyarea2, size = studyarea2))+
  ggplot2::geom_point(
    data = pca_dat_biostim |> dplyr::filter(Type == "Test"), 
    ggplot2::aes(color = studyarea2, size = studyarea2)
  ) +
  ggplot2::geom_segment(data = nat_biostim_pca_loadings, ggplot2::aes(xend = PC1 * 9, yend = PC2 * 9, x = 0, y = 0)) +
  ggplot2::geom_text(data = nat_biostim_pca_loadings, ggplot2::aes(x = PC1 * 10, y = PC2 * 10, label = Var)) +
  ggplot2::scale_fill_brewer(palette = "Set1", name = "Site set") +
  ggplot2::scale_color_manual(values = c("#440154FF", "#FDE725FF", "gray"), name = "Study Area") +
  ggplot2::scale_size_manual(values = c(1.5, 1.5, 0.5), name = "Study Area") +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "PC 1", y = "PC 2")


pca_plots <- ggpubr::ggarrange(
  csci_pca_plot, asci_pca_plot, biostim_pca_plot, 
  labels = c("A", "B", "C"),
  ncol = 1,
  common.legend = T,
  legend = "right"
)
ggplot2::ggsave(pca_plots, filename = "figures/Part_1_Figure_05.jpg", dpi = 300, height = 9, width = 6.5)
