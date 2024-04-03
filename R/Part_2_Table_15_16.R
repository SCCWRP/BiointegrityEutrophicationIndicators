library(dplyr)

chan_df <- readr::read_csv("data-raw/Part_2_combined_df_withdata.csv") 

site_info_varz <- c("masterid", "latitude", "longitude", "comid", "huc", "county", "psa_lu", "RB")
class_varz <- c("ISWP2_name", "Watershed", "class_do")
bi_varz <- c("CSCI", "ASCI_D", "ASCI_H") |> sort()
bs_varz <- c("TN_mgL", "TP_mgL", "Chla_ugcm2", "AFDM_gm2", "PCT_MAP", "SpCond_uScm", "Temp_C", "DO_mgL", "PCT_SAFN")
bs_pretty <- c("TN", "TP", "Chla", "AFDM", "PCT_MAP", "SpCond", "Temp", "DO", "PCT_SAFN")

thresholds_df <- tibble::tibble(
  Index = c("CSCI", "ASCI_D", "ASCI_H"),
  Ref10 = c(0.79, 0.86, 0.86)
)

## Table 15 ####
chan_bi_df <- chan_df |>
  select(
    all_of(site_info_varz),
    all_of(class_varz),
    all_of(bi_varz)
  ) |>
  tidyr::pivot_longer(
    cols = all_of(class_varz), 
    names_to = "Classification", values_to = "Class", values_drop_na = T
  ) |>
  tidyr::pivot_longer(
    cols = all_of(bi_varz), 
    names_to = "Index", values_to = "Score", values_drop_na = T
  ) |>
  group_by(
    masterid, latitude, longitude, comid, huc, 
    county, psa_lu, RB, Classification, Class, Index 
  ) |>
  summarize(Score = max(Score)) |>
  ungroup() |>
  inner_join(thresholds_df) |>
  mutate(
    PF = if_else(Score >= Ref10, "Pass", "Fail"),
    Classification = factor(
      Classification, 
      levels = c("ISWP2_name", "Watershed", "class_do"),
      labels = c("ISWP", "Watershed", "Bed and Bank")
    ),
    Class = gsub("Soft bottom-", "Soft bottom\n", Class)
  )



high_score_table <- chan_bi_df |> 
  inner_join(thresholds_df) |>
  group_by(Classification, Class, Index) |>
  summarize(
    n = sum(!is.na(Score)),
    n_pass = sum(Score > Ref10)
  ) |>
  ungroup() |>
  mutate(PctPass = 100 * (n_pass / n)) |>
  filter(!(Class %in% c("Natural", "Agricultural"))) |>
  bind_rows(
    tibble::tibble(
      Classification = factor(c("Watershed")), 
      Class = "Ambiguous",
      Index = c("ASCI_D", "ASCI_H"),
      n=0
    )
  ) |>
  arrange(Classification, Class, Index) |>
  mutate(
    Evidence = case_when(
      n < 20 ~ "Need more data",
      PctPass < 10 ~ "No", 
      .default = "Yes"
    ),
    Class = gsub("Soft bottom\n", "Soft bottom-", Class)
  )

write.csv(high_score_table, 'tables/Part_2_Table_15.csv', row.names = F)

## Table 16 ####

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
    Stressor = factor(BSVar, levels = bs_varz, labels = bs_pretty),
    Class = gsub("\n", "-", Class)
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
      Stressor == "DO" & L90 > 0 ~ "Yes",
      Stressor == "DO" & L90 <= 0 & Power >= 0.8 ~ "No",
      U90 < 0 ~ "Yes",
      U90 >= 0 & Power >= 0.8 ~ "No",
      U90 >=0 & Power < 0.8 ~ "Insufficient data",
      .default = "Insufficient data"
    )
  ) |>
  select(-c(lms, Classification), `Do linear models provide evidence of index responsiveness to stress?` = Evidence_Response)

write.csv(lin_model_summary, "tables/Part_2_Table_16.csv", row.names = F)
