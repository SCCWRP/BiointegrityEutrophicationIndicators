library(dplyr)

test_data_gis <- readr::read_csv('data-raw/Part_1_test_data_gis.csv') |> 
  group_by(masterid) |> 
  slice_head(n = 1) |>
  ungroup()

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv")

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
  inner_join(test_data_gis |> select(masterid, StudyArea)) |>
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
  left_join(
    test_data_gis |>
      select(
        masterid, psa6c, StudyArea, RefStatus,
        starts_with("urban"), 
        starts_with("ag"), 
        starts_with("code_21"),
        starts_with("roaddens"),
        W1_HALL = MaxW1_HALL
      )
  ) |>
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


table_9 <- test_data_scores2 |>
  mutate(
    StressType = case_when(
      Stressor %in% wq_var ~ "WQ",
      Stressor %in% phab_var ~ "Hab",
      .default = "Other"
    )
  ) |>
  group_by(StressType, Stressor2, Index) |>
  summarize(Rho = cor(IndexScore, StressorResult, method = "spearman", use = "pairwise.complete"))|>
  ungroup() |>
  tidyr::pivot_wider(names_from = Index, values_from = Rho) |>
  bind_rows(
    plot_dat_gis_stress |>
      mutate(StressType = "Landscape", Stressor2 = Disturbance) |>
      group_by(StressType, Stressor2, Index) |>
      summarize(Rho = cor(IndexScore, DisturbanceResult, method = "spearman", use = "pairwise.complete"))|>
      ungroup() |>
      tidyr::pivot_wider(names_from = Index, values_from = Rho) 
  ) |>
  select(StressType, Stressor = Stressor2, CSCI, ASCI_D, ASCI_H) |>
  mutate(
    StressType = factor(
      StressType, 
      levels = c("Landscape", "Hab", "WQ"), 
      labels = c("Landscape", "Habitat", "Water quality")
    )
  ) |>
  arrange(StressType)

write.csv(table_9, file = 'tables/Part_1_Table_09.csv', row.names = F)
