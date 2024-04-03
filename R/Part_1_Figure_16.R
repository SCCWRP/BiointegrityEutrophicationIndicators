library(dplyr)
library(ggplot2)

test_data_gis <- readr::read_csv('data-raw/Part_1_test_data_gis.csv') |> 
  group_by(masterid) |> 
  slice_head(n = 1) |>
  ungroup()

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv")

test_data_chem_phab <- readr::read_csv("data-raw/Part_1_test_data_chem_phab.csv") |>
  mutate(
    Nitrogen_Total_mgPerL = TN,
    Phosphorus_as_P_mgPerL = TP,
    Chlorophyll_a_mgPerm2 = `Chl-a_ug_cm2`,
    Ash_Free_Dry_Mass_mgPercm2 = AFDM_mg_m2 / 100
  )

model.summary <- readr::read_csv("data-raw/Part_1_model.summary.csv")
bs_models <- readRDS("data-raw/Part_1_my.models.Rdata")


summarized_chem_phab <- test_data_chem_phab |>
  select(
    masterid, 
    Nitrogen_Total_mgPerL, 
    Phosphorus_as_P_mgPerL, 
    Chlorophyll_a_mgPerm2, 
    Ash_Free_Dry_Mass_mgPercm2, 
    PCT_MAP
  ) |>
  #One value per site, not per sample
  group_by(masterid) |>
  summarize(
    Nitrogen_Total_mgPerL = mean(Nitrogen_Total_mgPerL, na.rm = T),
    Phosphorus_as_P_mgPerL = mean(Phosphorus_as_P_mgPerL, na.rm = T),
    Chlorophyll_a_mgPerm2 = mean(Chlorophyll_a_mgPerm2, na.rm = T),
    Ash_Free_Dry_Mass_mgPercm2 = mean(Ash_Free_Dry_Mass_mgPercm2, na.rm = T),
    PCT_MAP = mean(PCT_MAP, na.rm = T)
  ) |>
  ungroup()


test_data_scores2 <- test_data_scores |>
  group_by(masterid, sampledate) |>
  reframe(
    CSCI = pmax(CSCI, na.rm = T),
    ASCI_D = pmax(ASCI_D, na.rm = T),
    ASCI_H = pmax(ASCI_H, na.rm = T)
  ) |>
  ungroup() |>
  #Make it one value per site, rather than per sample
  group_by(masterid) |>
  summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T)
  ) |>
  ungroup() |>
  inner_join(summarized_chem_phab) |>
  inner_join(test_data_gis |> select(masterid, StudyArea)) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index", values_to = "IndexScore") |>
  tidyr::pivot_longer(
    cols = c(Nitrogen_Total_mgPerL, Phosphorus_as_P_mgPerL, Chlorophyll_a_mgPerm2, Ash_Free_Dry_Mass_mgPercm2, PCT_MAP), 
    names_to = "Stressor", 
    values_to = "Biostim", 
    values_drop_na = T
  ) |>
  filter(!is.na(IndexScore))


get_model <- function(bs_models, model.summary, BSPretty_, Response_, BIgoal_, Stratum_) {
  list_index <- model.summary |>
    mutate(row = row_number()) |>
    filter(
      BSPretty == BSPretty_,
      Response == Response_,
      BIgoal == BIgoal_,
      Stratum == Stratum_
    ) |>
    pull(row)
  
  bs_models[[list_index]]
}


calc_ProbRef10 <- function(Index_, Stressor_, Biostim_, bs_models, model.summary) {
  stressor_map <- c(
    "Nitrogen_Total_mgPerL" = "Total N", 
    "Phosphorus_as_P_mgPerL" = "Total P", 
    "Chlorophyll_a_mgPerm2" = "Chl-a", 
    "Ash_Free_Dry_Mass_mgPercm2" = "AFDM", 
    "PCT_MAP" = "% cover"
  )
  model <- get_model(bs_models, model.summary, stressor_map[Stressor_], Index_, "Ref10", "California")
  
  predict(model, newdata = data.frame(Biostim = Biostim_), type = "response") / 
    predict(model, newdata = data.frame(Biostim = 0),  type = "response")
}

test_data_scores2 <- test_data_scores2 |>
  mutate(
    ProbRef10 = purrr::pmap_dbl(
      .l = list(Index, Stressor, Biostim), 
      .f = function(Ind, Str, Bio) calc_ProbRef10(Ind, Str, Bio, bs_models, model.summary)
    ),
    Ref10_pass = case_when(
      Index == "CSCI" &  IndexScore >= 0.79 ~ "Pass",
      Index %in% c("ASCI_H", "ASCI_D") & IndexScore >= 0.86 ~ "Pass",
      .default = "Fail"
    ),
    BSPretty = case_when(
      Stressor == "Nitrogen_Total_mgPerL" ~ "TN",
      Stressor == "Phosphorus_as_P_mgPerL" ~ "TP",
      Stressor == "Ash_Free_Dry_Mass_mgPercm2" ~ "AFDM",
      Stressor == "Chlorophyll_a_mgPerm2" ~ "Chl-a",
      Stressor == "PCT_MAP" ~ "% cover",
      .default = NA_character_
    ),
    BSPretty = factor(BSPretty, levels = c("TN", "TP", "Chl-a", "AFDM", "% cover"))
  )

thresh_df <- data.frame(
  Index = c("CSCI", "ASCI_D", "ASCI_H"),
  Thresh = c(0.79, 0.86, 0.86)
)

biostim_probs_scatterplot <- ggplot(
    data = test_data_scores2, 
    aes(x = ProbRef10, y = IndexScore)
  ) +
  geom_point(aes(color = StudyArea), size = 1, shape = 16) +
  scale_color_brewer(palette = "Set1", name = "Study Area") +
  facet_grid(Index ~ BSPretty) +
  theme_bw() +
  geom_hline(data = thresh_df, aes(yintercept = Thresh, linetype = "Threshold")) +
  geom_quantile(method = "rq", quantiles = 0.9, color = "black", formula = y ~ x) +
  labs(y = "Index score", x = "Probability of score > 10th percentile of reference") +
  scale_linetype_manual(values = "dashed", name = "Threshold", labels = "10th percentile", guide = "none")+
  guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 1))) +
  theme(legend.box.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(0, units = "pt"))


ggsave(biostim_probs_scatterplot, filename = "figures/Part_1_Figure_16.jpg", dpi = 300, width = 10.5, height = 6)
