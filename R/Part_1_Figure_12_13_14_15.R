library(dplyr)
library(ggplot2)

#Responsiveness vs. stress
test_data_gis_vars <- readr::read_csv('data-raw/Part_1_test_data_gis.csv') |> 
  group_by(masterid) |> 
  slice_head(n = 1) |>
  ungroup() |>
  select(
    masterid, psa6c, StudyArea, RefStatus,
    starts_with("urban"), 
    starts_with("ag"), 
    starts_with("code_21"),
    starts_with("roaddens"),
    W1_HALL = MaxW1_HALL
  )

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv")


## Figure 12 ####
drop_disturbances <- c(
  "urban_2011_1k", "urban_2011_ws",
  "ag_2011_1k", "ag_2011_ws",
  "code_21_2011_1k", "code_21_2011_ws",
  "developed_2011_1k", "developed_2011_ws",
  "roaddens_1k", "roaddens_ws"
)

plot_dat_gis_stress <- test_data_scores |>
  select(masterid,CSCI, ASCI_D, ASCI_H) |>
  unique() |>
  group_by(masterid) |>
  summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T)
  ) |>
  ungroup() |>
  left_join(test_data_gis_vars) |>
  mutate(
    developed_2011_1k = urban_2011_1k + ag_2011_1k + code_21_2011_1k,
    developed_2011_5k = urban_2011_5k + ag_2011_5k + code_21_2011_5k,
    developed_2011_ws = urban_2011_ws + ag_2011_ws + code_21_2011_ws
  ) |>
  tidyr::pivot_longer(
    cols = c(
      starts_with("urban"), 
      starts_with("ag"), 
      starts_with("code_21"), 
      starts_with("developed"),
      starts_with("roaddens"),
      W1_HALL
    ), 
    names_to = "Disturbance", 
    values_to = "DisturbanceResult", 
    values_drop_na = T
  ) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index", values_to = "IndexScore", values_drop_na = T) |>
  filter(DisturbanceResult != -Inf) |> 
  filter(!(Disturbance %in% drop_disturbances)) |>
  mutate(
    Disturbance = factor(
      Disturbance, 
      levels = c("ag_2011_5k", "urban_2011_5k", "code_21_2011_5k", "developed_2011_5k", "roaddens_5k","W1_HALL"),
      labels = c("% Agriculture", "% Urban", "% Code 21", "% Developed", "Road density", "Riparian\ndisturbance")
    ) 
  )
  
responsiveness_plot_gis_gam <- ggplot(
    data = plot_dat_gis_stress , 
    aes(x = DisturbanceResult, y = IndexScore, color = StudyArea)
  ) +
  geom_point(size = 0.5, alpha = 0.5) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 1), linewidth = 1) +
  scale_color_brewer(palette = "Set1", name = "Study area") +
  facet_grid(rows = vars(Index), cols = vars(Disturbance), scales = "free_x") +
  labs(x = "Human activity level", y = "Index score") +
  theme_bw()

ggsave(responsiveness_plot_gis_gam, filename = "figures/Part_1_Figure_12.jpg", dpi = 300, width = 10, height = 6)

## Figure 13 ####
# note that the calculated variable importance plot depends on random number generation that may differ
# slightly depending on computer hardware, operating system, and version of R
rf_dat <- test_data_scores |>
  select(masterid,CSCI, ASCI_D, ASCI_H) |>
  unique() |>
  group_by(masterid) |>
  summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T)
  ) |>
  ungroup() |>
  left_join(test_data_gis_vars)

rf_dat_csci <- rf_dat |>
  select(-ASCI_D, -ASCI_H) |>
  na.omit()

rf_dat_asci<-rf_dat |>
  select(-CSCI) |>
  na.omit()

set.seed(111)
rf_csci <- randomForest::randomForest(
  CSCI ~ urban_2011_ws + urban_2011_5k + urban_2011_1k +
         ag_2011_ws + ag_2011_5k+ag_2011_1k +
         code_21_2011_ws + code_21_2011_5k + code_21_2011_1k +
         roaddens_ws + roaddens_5k + roaddens_1k,
  data = rf_dat_csci, 
  importance = T
)

set.seed(118)
rf_asci_d <- randomForest::randomForest(
  ASCI_D ~ urban_2011_ws + urban_2011_5k + urban_2011_1k +
           ag_2011_ws + ag_2011_5k + ag_2011_1k +
           code_21_2011_ws + code_21_2011_5k + code_21_2011_1k +
           roaddens_ws + roaddens_5k + roaddens_1k,
  data = rf_dat_asci, 
  importance = T
)

set.seed(125)
rf_asci_h <- randomForest::randomForest(
  ASCI_H ~ urban_2011_ws + urban_2011_5k + urban_2011_1k +
           ag_2011_ws + ag_2011_5k + ag_2011_1k +
           code_21_2011_ws + code_21_2011_5k + code_21_2011_1k +
           roaddens_ws + roaddens_5k + roaddens_1k,
  data = rf_dat_asci, 
  importance = T
)

rf_importance_df <- tibble(
    EnvVar = row.names(rf_csci$importance),
    CSCI = rf_csci$importance[, 1],
    ASCI_D = rf_asci_d$importance[, 1],
    ASCI_H = rf_asci_h$importance[, 1]
  ) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index", values_to = "IncMSE")

rf_stress_importance_plot <- ggplot(
    data = rf_importance_df, 
    aes(x = EnvVar, y = IncMSE, color = Index)
  ) +
  geom_point() +
  labs(x = "", y = "Importance (% increase mean-square error)") +
  coord_flip() +
  scale_color_viridis_d() +
  theme_bw()

ggsave(rf_stress_importance_plot, filename = "figures/Part_1_Figure_13.jpg", dpi = 300, height = 4, width = 4)


## Figure 14 ####
test_data_chem_phab <- readr::read_csv("data-raw/Part_1_test_data_chem_phab.csv") 

#Clean up outliers
test_data_chem_phab$Oxygen_Dissolved[test_data_chem_phab$Oxygen_Dissolved > 20] <- NA
test_data_chem_phab$pH[test_data_chem_phab$pH > 10] <- NA
test_data_chem_phab$Temperature[test_data_chem_phab$Temperature < 4] <- NA

wq_var <- c("Oxygen_Dissolved", "pH", "SpecificConductivity", "Temperature", "Turbidity", "TN", "TP")
phab_var <- c("PCT_SAFN", "XCMG", "XFC_NAT_SWAMP", "PCT_FAST")

test_data_scores2 <- test_data_scores |>
  select(masterid, sampledate, CSCI, ASCI_D, ASCI_H) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H)) |>
  na.omit() |>
  group_by(masterid, sampledate, name) |>
  summarize(value = max(value)) |>
  ungroup() |>
  tidyr::pivot_wider(names_from = name, values_from = value) |>
  left_join(
    test_data_chem_phab |>
      tidyr::pivot_longer(cols = setdiff(names(test_data_chem_phab), c("masterid", "sampledate")), values_drop_na = T) |>
      group_by(masterid, sampledate, name) |>
      summarize(value = mean(value)) |>
      ungroup() |>
      tidyr::pivot_wider(names_from = name, values_from = value)  ) |>
  inner_join(test_data_gis_vars |> select(masterid, StudyArea)) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index", values_to = "IndexScore", values_drop_na = T) |>
  select(-c("Alkalinity_as_CaCO3", "Salinity", "AFDM_mg_m2", "Chl-a_ug_cm2", "PCT_MAP")) |>
  tidyr::pivot_longer(
    cols = all_of(c(wq_var, phab_var)), 
    names_to = "Stressor", 
    values_to = "StressorResult", 
    values_drop_na = T
  ) |>
  mutate(
    Stressor2 = factor(
      Stressor, 
      levels = c(wq_var, phab_var),
      labels = c("DO","pH","Sp. Cond", "Temp", "Turb","TN","TP","PCT_SAFN","XCMG","XFC_NAT","PCT_FAST")
    ),
    Stressor = factor(Stressor, levels = c(wq_var, phab_var)),
  )



wq_stress_plot <- ggplot(
    data = test_data_scores2 |> filter(Stressor %in% wq_var), 
    aes(x = StressorResult, y = IndexScore, color = StudyArea)
  ) +
  geom_point(size = 0.5, alpha = 0.5) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 1), linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(Index ~ Stressor2, scales = "free") +
  theme_bw() +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

ggsave(wq_stress_plot, filename = "figures/Part_1_Figure_14.jpg", dpi = 300, width = 10, height = 6)


## Figure 15 ####

phab_stress_plot <- ggplot(
    data = test_data_scores2 |> filter(Stressor %in% phab_var), 
    aes(x = StressorResult, y = IndexScore, color = StudyArea)
  ) +
  geom_point(size = 0.5, alpha = 0.5) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 1), linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(Index ~ Stressor2, scales = "free") +
  theme_bw()+
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

ggsave(phab_stress_plot, filename = "figures/Part_1_Figure_15.jpg", dpi = 300, width = 10, height = 6)
