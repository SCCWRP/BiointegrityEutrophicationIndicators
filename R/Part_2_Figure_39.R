library(dplyr)
library(ggplot2)

chan_df <- readr::read_csv("data-raw/Part_2_combined_df_withdata.csv") 

site_info_varz <- c("masterid", "latitude", "longitude", "comid", "huc", "county", "psa_lu", "RB")
class_varz <- c("ISWP2_name", "Watershed", "class_do")
bi_varz <- c("CSCI", "ASCI_D", "ASCI_H") |> sort()
bs_varz <- c("TN_mgL", "TP_mgL", "Chla_ugcm2", "AFDM_gm2", "PCT_MAP", "SpCond_uScm", "Temp_C", "DO_mgL", "PCT_SAFN")
bs_pretty <- c("TN", "TP", "Chla", "AFDM", "PCT_MAP", "SpCond", "Temp", "DO", "PCT_SAFN")

thresholds_df <- tibble(
  Index = c("CSCI", "ASCI_D", "ASCI_H"),
  Ref10 = c(0.79, 0.86, 0.86)
)

chan_bi_bs_df <- chan_df |>
  select(
    all_of(site_info_varz),
    all_of(class_varz),
    all_of(bi_varz),
    all_of(bs_varz)
  ) |>
  tidyr::pivot_longer(
    cols = all_of(class_varz), 
    names_to = "Classification", values_to = "Class", values_drop_na = T
  ) |>
  tidyr::pivot_longer(
    cols = all_of(bi_varz), 
    names_to = "Index", values_to = "Score", values_drop_na = T
  ) |>
  tidyr::pivot_longer(
    cols = all_of(bs_varz), 
    names_to = "BSVar", values_to = "BSResult", values_drop_na = T
  ) |>
  mutate(
    BSResult = if_else(BSVar == "Chla_ugcm2", BSResult * 10, BSResult), # convert to mg/m2
    BSVar = stringr::str_replace(BSVar, "Chla_ugcm2", "Chla_mgm2")
  ) |>
  group_by(
    masterid, latitude, longitude, comid, huc, county, 
    psa_lu, RB, Classification, Class, Index, BSVar
  ) |>
  summarize(
    Score = max(Score),
    BSResult = mean(BSResult)
  ) |>
  ungroup() |>
  inner_join(thresholds_df) |>
  mutate(
    PF = if_else(Score >= Ref10, "Pass", "Fail"),
    Classification = factor(
      Classification, 
      levels = c("ISWP2_name", "Watershed", "class_do"),
      labels = c("ISWP", "Watershed", "Bed and Bank")
    ),
    Class = gsub("Soft bottom-", "Soft bottom\n", Class),
    Stressor = factor(BSVar, levels = c("TN_mgL", "TP_mgL", "Chla_mgm2", "AFDM_gm2", "PCT_MAP", "SpCond_uScm", "Temp_C", "DO_mgL", "PCT_SAFN"))
  )

lin_model_summary <- chan_bi_bs_df |>
  filter(stringr::str_detect(Class, "bottom") | Class %in% c("Constructed","Ambiguous")) |>
  group_by(Classification, Class, Index, Stressor) |>
  summarize(
    lms = list(lm(Score ~ BSResult, data = pick(Score, BSResult))),
    n = n()
  ) |>
  filter(n >= 20) |>
  ungroup() |>
  mutate(
    Slope = purrr::map_dbl(lms, function(model) coef(model)[2]),
    L90 = purrr::map_dbl(lms, function(model) confint(model, level = 0.9)[2, 1]),
    U90 = purrr::map_dbl(lms, function(model) confint(model, level = 0.9)[2, 2]),
    Power = purrr::map_dbl(lms, function(model) {
      model_sum <- summary(model)
      f2_ <- model_sum$fstatistic["value"]
      u_ <- model_sum$fstatistic["numdf"]
      v_ <- model_sum$fstatistic["dendf"]
      pwr::pwr.f2.test(u = u_, v = v_, f2 = f2_, sig.level = 0.1)$power
    }),
    Evidence_Response = case_when(
      Stressor == "DO_mgL" & L90 > 0 ~ "Yes",
      Stressor == "DO_mgL" & L90 <= 0 & Power >= 0.8 ~ "No",
      U90 < 0 ~ "Yes",
      U90 >= 0 & Power >= 0.8 ~ "No",
      U90 >=0 & Power < 0.8 ~ "Insufficient data",
      .default = "Insufficient data"
    ),
    Evidence_Response = factor(Evidence_Response, levels = c("No", "Yes", "Insufficient data"))
  )

chan_bi_bs_df2 <- chan_bi_bs_df |>
  left_join(lin_model_summary, by = join_by(Class, Index, Stressor), keep = F) |>
  mutate(
    Class = gsub("\n", ", ", Class, fixed = T),
    Class = gsub(" 0 ", " no ", Class, fixed = T)
  )

make_plot <- function(data_, Class_) {
  ggplot(
    data = data_ |> filter(Class == Class_),
    aes(x = BSResult, y = Score)
  ) +
    geom_point(aes(color = Index), size = 0.5, alpha = 0.5, shape = 16) +
    facet_wrap(~ Stressor, scales = "free_x", nrow = 1) +
    geom_smooth(
      aes(color = Index, linetype = Evidence_Response, linewidth = Evidence_Response), 
      method = "lm", se = F, show.legend = TRUE
    ) +
    scale_linewidth_manual(
      values = c(0.5, 1, 0), name = "Evidence of responsiveness?", 
      labels = c("No", "Yes", ""), drop = FALSE
    ) +
    scale_linetype_manual(
      values = c("dashed", "solid", NA), name = "Evidence of responsiveness?", 
      labels = c("No", "Yes", ""), drop = FALSE
    ) +
    scale_color_brewer(palette = "Set1", drop = FALSE) +
    theme_bw() +
    labs(title = Class_, x = "", y = "Index Score") +
    guides(
      linetype = guide_legend(order = 1),
      linewidth = guide_legend(order = 1),
      color = guide_legend(order = 2)
    )
}

hard_bottom_plot <- make_plot(chan_bi_bs_df2, "Hard bottom")
soft_bottom_2_plot <- make_plot(chan_bi_bs_df2, "Soft bottom, 2 hard sides")
soft_bottom_1_plot <- make_plot(chan_bi_bs_df2, "Soft bottom, 1 hard side")
soft_bottom_0_plot <- make_plot(chan_bi_bs_df2, "Soft bottom, no hard sides")

chan_response_plot_alt <- ggpubr::ggarrange(
  hard_bottom_plot,
  soft_bottom_2_plot,
  soft_bottom_1_plot,
  soft_bottom_0_plot,
  ncol = 1, common.legend = T, legend = "bottom"
)

ggsave(chan_response_plot_alt, filename = "figures/Part_2_Figure_39.jpg", dpi = 300, width = 11, height = 8)
