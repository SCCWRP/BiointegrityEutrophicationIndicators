library(dplyr)
library(ggplot2)

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
  rename_with(.fn = tolower) |>
  mutate(logwsa = log10(area_sqkm))

csci_dev_gis <- readr::read_csv("data-raw/Part_1_stations.out.csv") |> 
  rename_with(.fn = tolower)

asci_dev_gis <- readr::read_csv("data-raw/Part_1_asci_caldata.csv") |> 
  rename_with(.fn = tolower)

biostim_dev_gis <- readr::read_csv("data-raw/Part_1_biostim_input_data.csv") |>
  mutate(logwsa = log10(AREA_SQKM)) |>
  rename_with(.fn = tolower)

all.gis <- readr::read_csv("data-raw/Part_1_all.gis.cleaned.csv") |>
  mutate(logwsa = log10(area_sqkm)) |> # Get all GIS data imported in order to merge with ASCI data 
  group_by(masterid) |> 
  slice_head(n=1) |>
  ungroup()


test_data_gis2 <- test_data_gis |>
  select(masterid, all_of(env_vars), sitestatus = refstatus, studyarea) |>
  unique() |>
  mutate(Type = "Test", Type2 = "Test")

csci_dev_gis2 <- csci_dev_gis |>
  select(masterid = stationcode, all_of(env_vars), sitestatus) |> 
  unique() |>
  mutate(
    studyarea = case_when(sitestatus == "Reference" ~ "Reference", .default = "Non-reference"), 
    Type = "CSCI cal", 
    Type2 = "Cal"
  )



asci_dev_gis2 <- asci_dev_gis |>
  left_join(msid.lu) |>
  mutate(masterid = if_else(is.na(masterid), stationcode, masterid)) |>
  inner_join(all.gis) |>
  select(all_of(env_vars), masterid, RefStatus) |>
  rename(sitestatus = RefStatus) |>
  unique() |>
  mutate(
    studyarea =  case_when(sitestatus == "Reference" ~ "Reference", .default = "Non-reference"), 
    Type = "ASCI cal", 
    Type2 = "Cal"
  )


biostim_dev_gis2 <- biostim_dev_gis |>
  select(masterid = stationcode, all_of(env_vars), sitestatus) |> 
  unique() |>
  mutate(
    studyarea =  case_when(sitestatus == "Reference" ~ "Reference", .default = "Non-reference"), 
    Type = "Biostim cal", 
    Type2 = "Cal"
  )


csci_caltest <- csci_dev_gis2 |> 
  bind_rows(test_data_gis2) 

asci_caltest <- asci_dev_gis2 |> 
  bind_rows(test_data_gis2) |> 
  filter(Type2 == "Cal")

biostim_caltest <- biostim_dev_gis2 |> 
  bind_rows(test_data_gis2) |> 
  filter(Type2 == "Cal")

all_cal_test <- bind_rows(csci_caltest, asci_caltest, biostim_caltest) |>
  mutate(
    Type.x = case_when(
      Type == "CSCI cal" ~ "CSCI",
      Type == "ASCI cal" ~ "ASCI",
      Type == "Biostim cal" ~ "ERM",
      .default = Type
    ),
    Type.x = factor(Type.x, levels = c("CSCI", "ASCI", "ERM", "Test"))
  ) 

## Figure 3 ####

univariate_plot_data <- all_cal_test |>
  tidyr::pivot_longer(
    cols = all_of(env_vars), 
    names_to = "EnvVar", 
    values_to = "EnvResult"
  ) |>
  filter(EnvVar %in% univariate_env_vars) |>
  mutate(EnvVar = factor(EnvVar, levels = univariate_env_vars))



univariate_gradients_plot <- ggplot(
    data = univariate_plot_data, 
    aes(x = Type.x, y = EnvResult)
  ) +
  geom_point(
    aes(fill = studyarea), 
    shape = 21, 
    position = position_jitterdodge(jitter.height = 0, jitter.width = 0.25), size = 1
  ) +
  facet_wrap(vars(EnvVar), scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(name = "Site set") +
  labs(x = "", y = "") +
  theme_bw()+
  guides(fill = guide_legend(override.aes = list(size=2)))+
  theme(legend.position = "bottom")

ggsave(
  univariate_gradients_plot, 
  filename = "figures/Part_1_Figure_03.jpg", 
  dpi = 300, 
  height = 8, 
  width = 6.5
)

## Figure 4 ####

all_data_slope_fines <- readr::read_csv("data-raw/Part_1_all_data_slope_fines.csv")

csci_cal_slope_fines <- all_data_slope_fines |> 
  filter(masterid %in% csci_dev_gis$stationcode) |> 
  mutate(Type.x = "CSCI") |>
  inner_join(csci_dev_gis2 |> select(masterid, studyarea))

asci_cal_slope_fines <- all_data_slope_fines |> 
  filter(masterid %in% asci_dev_gis$stationcode) |> 
  mutate(Type.x = "ASCI") |>
  inner_join(asci_dev_gis2 |> select(masterid, studyarea))

biostim_cal_slope_fines <- all_data_slope_fines |> 
  filter(masterid %in% biostim_dev_gis$masterid) |> 
  mutate(Type.x = "ERM") |>
  inner_join(biostim_dev_gis2 |> select(masterid, studyarea))

test_data_slope_fines <- all_data_slope_fines |> 
  filter(masterid %in% test_data_gis$masterid) |>  
  mutate(Type.x = "Test") |>
  inner_join(test_data_gis |> select(masterid, studyarea))

slope_fines_plot_data <- univariate_plot_data |>
  select(masterid, sitestatus, studyarea, Type.x) |>
  unique() |>
  inner_join(csci_cal_slope_fines) |>
  bind_rows(
    univariate_plot_data |>
      select(masterid, sitestatus, studyarea, Type.x) |>
      unique() |>
      inner_join(asci_cal_slope_fines) 
  ) |>
  bind_rows(
    univariate_plot_data |>
      select(masterid, sitestatus, studyarea, Type.x) |>
      unique() |>
      inner_join(biostim_cal_slope_fines) 
  ) |>
  bind_rows(
    univariate_plot_data |>
      select(masterid, sitestatus, studyarea, Type.x) |>
      unique() |>
      inner_join(test_data_slope_fines) 
  ) |>
  tidyr::pivot_longer(
    cols = c(XSLOPE, PCT_FN, PCT_HP), 
    names_to = "EnvVar", 
    values_to ="EnvResult", 
    values_drop_na = T
  ) |>
  mutate(
    EnvVar2 = case_when(
      EnvVar == "PCT_FN" ~ "% fines",
      EnvVar == "PCT_HP" ~ "% hardpan",
      EnvVar == "XSLOPE" ~ "slope",
      .default = "Other"
    ),
    Type.x = factor(Type.x, levels = c("CSCI", "ASCI", "ERM", "Test"))
  )

slope_fines_plot <- ggplot(
  data = slope_fines_plot_data |>
    filter(EnvVar != "PCT_HP"), 
    aes(x = Type.x, y = EnvResult)
  ) +
  geom_point(
    aes(fill = studyarea), 
    shape = 21, 
    position = position_jitterdodge(jitter.height = 0, jitter.width = 0.25), 
    size = 1
  )+
  facet_wrap(vars(EnvVar2), scales = "free", ncol = 3) +
  scale_fill_viridis_d(name = "Site set")+
  labs(x = "", y = "") +
  theme_bw() +
  guides(fill = guide_legend(override.aes = list(size = 2))) +
  theme(legend.position = "bottom")

ggsave(slope_fines_plot, filename = "figures/Part_1_Figure_04_new.jpg", height = 4, width = 6, dpi = 300)


## Figure 5 ####

nat_vars <- setdiff(env_vars, c("new_lat","new_long")) #Exclude "location" variables
pca_dat1 <- all_cal_test |>
  filter(!is.na(minp_ws)) |>
  tidyr::pivot_longer(cols = all_of(env_vars), names_to = "EnvVar", values_to = "EnvResult") |>
  filter(EnvVar %in% nat_vars) |>
  tidyr::pivot_wider(names_from = EnvVar, values_from = EnvResult) |>
  mutate(studyarea2 = if_else(studyarea %in% c("Reference", "Non-reference"), "Calibration", studyarea))


pca_dat_csci <- pca_dat1 |> filter(Type.x %in% c("CSCI", "Test"))
pca_dat_csci_matrix <- pca_dat_csci |> select(all_of(nat_vars))
nat_csci_pca <- prcomp(pca_dat_csci_matrix, scale = T, retx = T,center = T)
colnames(nat_csci_pca$x) <- paste("Nat", colnames(nat_csci_pca$x), sep = ".")
pca_dat_csci <- cbind(pca_dat_csci, nat_csci_pca$x)

nat_csci_pca_loadings <- data.frame(nat_csci_pca$rotation)
nat_csci_pca_loadings$Var <- nat_vars

pca_dat_asci <- pca_dat1 |> filter(Type.x %in% c("ASCI", "Test"))
pca_dat_asci_matrix <- pca_dat_asci |> select(all_of(nat_vars))
nat_asci_pca <- prcomp(pca_dat_asci_matrix, scale = T, retx = T,center = T)
colnames(nat_asci_pca$x) <- paste("Nat", colnames(nat_asci_pca$x), sep = ".")
pca_dat_asci <- cbind(pca_dat_asci, nat_asci_pca$x)

nat_asci_pca_loadings <- data.frame(nat_asci_pca$rotation)
nat_asci_pca_loadings$Var <- nat_vars


pca_dat_biostim <- pca_dat1 |> filter(Type.x %in% c("ERM", "Test"))
pca_dat_biostim_matrix <- pca_dat_biostim |> select(all_of(nat_vars))
nat_biostim_pca <- prcomp(pca_dat_biostim_matrix, scale = T, retx = T,center = T)
colnames(nat_biostim_pca$x) <- paste("Nat", colnames(nat_biostim_pca$x), sep = ".")
pca_dat_biostim <- cbind(pca_dat_biostim, nat_biostim_pca$x)

nat_biostim_pca_loadings <- data.frame(nat_biostim_pca$rotation)
nat_biostim_pca_loadings$Var <- nat_vars



#Add min convex polygons around cal data, but don't plot those sites.
PC_hull_csci <- pca_dat_csci |> 
  group_by(studyarea) |>
  slice(chull(Nat.PC1, Nat.PC2))

PC_hull_asci <- pca_dat_asci |>
  group_by(studyarea) |>
  slice(chull(Nat.PC1, Nat.PC2))

PC_hull_biostim <- pca_dat_biostim |>
  group_by(studyarea) |>
  slice(chull(Nat.PC1, Nat.PC2))

pca_dat_csci$studyarea2 <- factor(pca_dat_csci$studyarea2, levels=c("Central Valley", "Modoc", "Calibration"))
pca_dat_asci$studyarea2 <- factor(pca_dat_asci$studyarea2, levels=c("Central Valley", "Modoc", "Calibration"))
pca_dat_biostim$studyarea2 <- factor(pca_dat_biostim$studyarea2, levels=c("Central Valley", "Modoc", "Calibration"))

csci_pca_plot <- ggplot(data = pca_dat_csci, aes(x = Nat.PC1, y = Nat.PC2))+
  geom_polygon(
    data = PC_hull_csci |> filter(studyarea %in% c("Reference","Non-reference")), 
    aes(fill = studyarea), 
    alpha = 0.5
  ) +
  geom_point(size = 0.5, color = "gray")+
  geom_point(data = pca_dat_csci, aes(color = studyarea2, size = studyarea2))+
  geom_point(
    data = pca_dat_csci |> filter(Type == "Test"), 
    aes(color = studyarea2, size = studyarea2)
  ) +
  geom_segment(data = nat_csci_pca_loadings, aes(xend = PC1 * 9, yend = PC2 * 9, x = 0, y = 0)) +
  geom_text(data = nat_csci_pca_loadings, aes(x = PC1 * 10, y = PC2 * 10, label = Var)) +
  scale_fill_brewer(palette = "Set1", name = "Site set") +
  scale_color_manual(values = c("#440154FF", "#FDE725FF", "gray"), name = "Study Area") +
  scale_size_manual(values = c(1.5, 1.5, 0.5), name = "Study Area") +
  theme_classic() +
  labs(x = "PC 1", y = "PC 2")

asci_pca_plot <- ggplot(data = pca_dat_asci, aes(x = Nat.PC1, y = Nat.PC2))+
  geom_polygon(
    data = PC_hull_asci |> filter(studyarea %in% c("Reference","Non-reference")), 
    aes(fill = studyarea), 
    alpha = 0.5
  ) +
  geom_point(size = 0.5, color = "gray")+
  geom_point(data = pca_dat_asci, aes(color = studyarea2, size = studyarea2))+
  geom_point(
    data = pca_dat_asci |> filter(Type == "Test"), 
    aes(color = studyarea2, size = studyarea2)
  ) +
  geom_segment(data = nat_asci_pca_loadings, aes(xend = PC1 * 9, yend = PC2 * 9, x = 0, y = 0)) +
  geom_text(data = nat_asci_pca_loadings, aes(x = PC1 * 10, y = PC2 * 10, label = Var)) +
  scale_fill_brewer(palette = "Set1", name = "Site set") +
  scale_color_manual(values = c("#440154FF", "#FDE725FF", "gray"), name = "Study Area") +
  scale_size_manual(values = c(1.5, 1.5, 0.5), name = "Study Area") +
  theme_classic() +
  labs(x = "PC 1", y = "PC 2")

biostim_pca_plot <- ggplot(data = pca_dat_biostim, aes(x = Nat.PC1, y = Nat.PC2))+
  geom_polygon(
    data = PC_hull_biostim |> filter(studyarea %in% c("Reference","Non-reference")), 
    aes(fill = studyarea), 
    alpha = 0.5
  ) +
  geom_point(size = 0.5, color = "gray")+
  geom_point(data = pca_dat_biostim, aes(color = studyarea2, size = studyarea2))+
  geom_point(
    data = pca_dat_biostim |> filter(Type == "Test"), 
    aes(color = studyarea2, size = studyarea2)
  ) +
  geom_segment(data = nat_biostim_pca_loadings, aes(xend = PC1 * 9, yend = PC2 * 9, x = 0, y = 0)) +
  geom_text(data = nat_biostim_pca_loadings, aes(x = PC1 * 10, y = PC2 * 10, label = Var)) +
  scale_fill_brewer(palette = "Set1", name = "Site set") +
  scale_color_manual(values = c("#440154FF", "#FDE725FF", "gray"), name = "Study Area") +
  scale_size_manual(values = c(1.5, 1.5, 0.5), name = "Study Area") +
  theme_classic() +
  labs(x = "PC 1", y = "PC 2")


pca_plots <- ggpubr::ggarrange(
  csci_pca_plot, asci_pca_plot, biostim_pca_plot, 
  labels = c("A", "B", "C"),
  ncol = 1,
  common.legend = T,
  legend = "right"
)
ggsave(pca_plots, filename = "figures/Part_1_Figure_05.jpg", dpi = 300, height = 9, width = 6.5)
