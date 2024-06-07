library(dplyr)
library(ggplot2)

response.varz <- c("CSCI", "ASCI_D", "ASCI_H")

bs.varz.df <- data.frame(
  BiostimVar = c(
    "Nitrogen_Total_mgPerL", 
    "Phosphorus_as_P_mgPerL", 
    "Chlorophyll_a_mgPerm2", 
    "Ash_Free_Dry_Mass_mgPercm2", 
    "PCT_MAP"
  ), 
  BSPretty = c("Total N", "Total P", "Chl-a", "AFDM", "% cover")
)

thresholds.df <- readr::read_csv('data-raw/Part_1_thresholds.df.csv')

thresholds.df.ref10 <- thresholds.df |>
  select(Index, Ref10)

mydf.c3.m <- readr::read_csv("data-raw/Part_1_mydf.c3.m.csv") |>
  mutate(
    DevSet = case_when(
      PSA6c == "CV" ~ "CV",
      PSA6c == "DM" & New_Lat > 39 ~ "MP",
      .default = "Other"
    )
  ) |>
  filter(DevSet %in% c("CV","MP")) |>
  tidyr::pivot_longer(cols = all_of(response.varz), names_to = "Index", values_to = "IndexResult") |>
  inner_join(bs.varz.df) |>
  mutate(BSPretty = factor(BSPretty, levels = bs.varz.df$BSPretty))



summary_df <- readr::read_csv("data-raw/Part_1_model.summary2.m3.csv") |>
  filter(Stratum == "California", BIgoal == "Ref10", Prob == "p80") |>
  mutate(DevSet = if_else(DevSet == "Cal", "CV", "MP")) |>
  inner_join(
    mydf.c3.m |> filter(SelectedSample == "Selected"), 
    by = join_by(BiostimVar, BIgoal, BIgoal2, Response == Index, DevSet, BSPretty),
    keep = F
  ) |>
  na.omit() |>
  inner_join(thresholds.df.ref10, by = join_by(Response == Index)) |>
  select(MasterID, New_Long, New_Lat, BSPretty, Index = Response, DevSet, IndexResult, threshBI = Ref10, BiostimResult, threshBS = Est) |>
  mutate(
    Response = if_else(IndexResult < threshBI, "Poor", "Good"),
    Stressor = if_else(BiostimResult > threshBS, "Poor", "Good"),
    siteID = MasterID,
    weight = 1
  )

calc_rel_risk <- function(df) {
  if (
    sum(df$Response == "Good") == nrow(df) | 
    sum(df$Stressor == "Good") == nrow(df) 
  ) {
    myrisk <- data.frame(Type = "All_Sites", Subpopulation = "All Sites", Response = "Response", Stressor = "Stressor") |>
      mutate(
        nResp = nrow(df),
        Estimate = NA, Estimate_num = NA, Estimate_denom = NA,
        StdError_log = NA, MarginofError_log = NA, LCB95Pct = NA, UCB95Pct = NA,
        WeightTotal = nrow(df),
        Count_RespPoor_StressPoor = sum(df$IndexResult < df$threshBI & df$BiostimResult > df$threshBS),
        Count_RespPoor_StressGood = sum(df$IndexResult >= df$threshBI & df$BiostimResult > df$threshBS),
        Count_RespGood_StressPoor = sum(df$IndexResult < df$threshBI & df$BiostimResult <= df$threshBS),
        Count_RespGood_StressGood = sum(df$IndexResult >= df$threshBI & df$BiostimResult <= df$threshBS),
        Prop_RespPoor_StressPoor = NA, Prop_RespPoor_StressGood = NA, Prop_RespGood_StressPoor = NA, Prop_RespGood_StressGood = NA
      )
  } else {
    myrisk <- spsurvey::relrisk_analysis(
      dframe = df, 
      vars_response = "Response", vars_stressor = "Stressor", 
      xcoord = "New_Long", ycoord = "New_Lat"
    )
  }
  myrisk
}


model_summary <- summary_df |>
  group_by(BSPretty, Index, DevSet) |>
  summarize(
    n_total = n(),
    n_BIPass = sum(IndexResult >= threshBI),
    n_BSPass = sum(BiostimResult <= threshBS),
    n_AgreePass = sum(IndexResult >= threshBI & BiostimResult <= threshBS),
    n_AgreeFail = sum(IndexResult < threshBI & BiostimResult > threshBS),
    n_Disagree_FailBI = sum(IndexResult < threshBI & BiostimResult <= threshBS),
    n_Disagree_FailBS = sum(IndexResult >= threshBI & BiostimResult > threshBS),
    risk = calc_rel_risk(pick(IndexResult, threshBI, BiostimResult, threshBS, Response, Stressor, New_Long, New_Lat, weight))
  ) |>
  tidyr::unnest(risk) |>
  mutate(
    BSPretty = factor(BSPretty, levels = c("Total N", "Total P", "Chl-a", "AFDM", "% cover")),
    StudyArea = case_when(
      DevSet == "CV" ~ "Central\nValley",
      DevSet == "MP" ~ "Modoc\nPlateau"
    )
  ) |>
  arrange(BSPretty) |>
  select(
    Indicator = BSPretty, Index, StudyArea, `Pass both` = n_AgreePass, `Fail both` = n_AgreeFail, 
    `Fail BI` = n_Disagree_FailBI, `Fail ET` = n_Disagree_FailBS, RR = Estimate, L95 = LCB95Pct, U95 = UCB95Pct
  )


cv_mp_rr_plot <- ggplot(data = model_summary, aes(x = StudyArea, y = RR)) +
  geom_pointrange(
    aes(color = Index, ymin = L95, ymax = U95),
    position = position_dodge(width = 0.25)
  ) +
  scale_color_brewer(palette = "Set1", name = "Index") +
  facet_wrap(~Indicator, scales = "free") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_cartesian(ylim = c(0, 5)) +
  labs(x = "", y = "Relative Risk") +
  theme_bw()

ggsave(cv_mp_rr_plot, filename = "figures/Part_1_Figure_19.jpg", height = 5, width = 6, dpi = 300)
