library(dplyr)

nonperen_df <- readr::read_csv("data-raw/Part_1_Andy_Nonperen_data_ASCI.csv") |>
  mutate(
    masterid = StationCode,
    sampledate = lubridate::mdy(SampleDate)
  )

sample_exclusion <- readr::read_csv('data-raw/Part_1_sample_metadata_review_07152024.csv') |>
  filter(!Include) |>
  mutate(sampledate = lubridate::mdy(SampleDate))

nonperen_df <- nonperen_df |>
  anti_join(sample_exclusion, by = c("StationCode", "sampledate"))

plot_dat <- nonperen_df |>
  mutate(Flow_SOP2 = if_else(Flow_SOP == "RFI_SFI", "SFI", Flow_SOP)) |>
  group_by(RB, StationCode, Flow_SOP, Flow_SOP2) |>
  summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T)) |>
  ungroup() |>
  mutate(Region = if_else(RB %in% c(4,7,8,9), "Southern", "Northern"))

stat_dat <- plot_dat |> 
  mutate(RBf = as.character(RB)) |>
  filter(Flow_SOP2 != "SFI")


## Table 11 ####
anova_df <- bind_rows(
  lm(CSCI ~ Flow_SOP2 * Region, data = stat_dat) |>  
    anova() |> 
    broom::tidy() |>
    mutate(Index = "CSCI"),
  lm(ASCI_D ~ Flow_SOP2 * Region, data = stat_dat) |>  
    anova() |> 
    broom::tidy() |>
    mutate(Index = "ASCI_D"),
  lm(ASCI_H ~ Flow_SOP2 * Region, data = stat_dat) |> 
    anova() |> 
    broom::tidy() |>
    mutate(Index = "ASCI_H")
) |>
  select(Index, Term = term, DF = df, SS = sumsq, MS = meansq, `F` = statistic, p = p.value) |>
  mutate(
    SS = round(SS, 3),
    MS = round(MS, 4),
    `F` = round(`F`, 1),
    p = round(p, 3),
    Term = stringr::str_replace_all(Term, "Flow_SOP2", "Flow status")
  )

write.csv(anova_df, "tables/Part_1_Table_11.csv", row.names = F, na = "")


## Table 12 ####

anova_df_tukey <- bind_rows(
    lm(CSCI ~ Flow_SOP2 * Region, data = stat_dat) |> 
      aov() |> 
      TukeyHSD() |>
      broom::tidy() |>
      mutate(Index = "CSCI"),
    lm(ASCI_D~Flow_SOP2*Region, data=stat_dat) |>  
      aov() |> 
      TukeyHSD() |> 
      broom::tidy() |>
      mutate(Index = "ASCI_D"),
    lm(ASCI_H ~ Flow_SOP2 * Region, data = stat_dat) |>  
      aov() |> 
      TukeyHSD() |> 
      broom::tidy() |>
      mutate(Index = "ASCI_H")
  ) |>
  filter(!(contrast %in% c("RFI:Southern-P:Northern", "P:Southern-RFI:Northern"))) |>
  select(Index, Contrast = contrast, Difference = estimate, L95 = conf.low, U95 = conf.high, `p-value` = adj.p.value) |>
  mutate(
    Difference = round(Difference, 2),
    L95 = round(L95, 2),
    U95 = round(U95, 2),
    `p-value` = round(`p-value`, 2)
  )

write.csv(anova_df_tukey, "tables/Part_1_Table_12.csv", row.names = F)


## Table 13 ####

plot_dat_pivot <- plot_dat |>
  tidyr::pivot_longer(
    cols = c("CSCI", "ASCI_D", "ASCI_H"), 
    names_to = "Index", values_to = "IndexScore", values_drop_na = T
  )


int_ref_distributions_empirical <- plot_dat_pivot |>
  filter(Flow_SOP2 != "P") |>
  group_by(Region, RB, Flow_SOP2, Index) |>
  summarize(
    n = length(IndexScore),
    Mean = mean(IndexScore),
    SD = sd(IndexScore),
    q30 = quantile(IndexScore, 0.3),
    q10 = quantile(IndexScore, 0.1),
    q01 = quantile(IndexScore, 0.01),
    RB = list(unique(RB))
  ) |>
  ungroup() |>
  na.omit() |>
  bind_rows(
    plot_dat_pivot |> 
      group_by(Region,Flow_SOP2, Index) |>
      summarize(
        n = length(IndexScore),
        Mean = mean(IndexScore),
        SD = sd(IndexScore),
        q30 = quantile(IndexScore, 0.3),
        q10 = quantile(IndexScore, 0.1),
        q01 = quantile(IndexScore, 0.01),
        RB = list(unique(RB))
      ) |>
      ungroup() ,
    plot_dat_pivot |> 
      group_by(Flow_SOP2, Index) |>
      summarize(
        n = length(IndexScore),
        Mean = mean(IndexScore),
        SD = sd(IndexScore),
        q30 = quantile(IndexScore, 0.3),
        q10 = quantile(IndexScore, 0.1),
        q01 = quantile(IndexScore, 0.01),
        RB = list(unique(RB))
      ) |>
      ungroup() |>
      mutate(Region = "All regions")
  ) |>
  filter(Flow_SOP2 != "P") 

int_ref_distributions_normal <- plot_dat_pivot |>
  filter(Flow_SOP2 != "P") |>
  group_by(Region, RB, Flow_SOP2, Index) |>
  summarize(
    n = length(IndexScore),
    Mean = mean(IndexScore),
    SD = sd(IndexScore),
    q30 = qnorm(p = 0.3, mean = Mean, sd = SD),
    q10 = qnorm(p = 0.1, mean = Mean, sd = SD),
    q01 = qnorm(p = 0.01, mean = Mean, sd = SD),
    RB = list(unique(RB))
  ) |>
  ungroup() |>
  na.omit() |>
  bind_rows(
    plot_dat_pivot |> 
      group_by(Region, Flow_SOP2, Index) |>
      summarize(
        n = length(IndexScore),
        Mean = mean(IndexScore),
        SD = sd(IndexScore),
        q30 = qnorm(p = 0.3, mean = Mean, sd = SD),
        q10 = qnorm(p = 0.1, mean = Mean, sd = SD),
        q01 = qnorm(p = 0.01, mean = Mean, sd = SD),
        RB = list(unique(RB))
      ) |>
      ungroup(),
    plot_dat_pivot |> 
      group_by(Flow_SOP2, Index) |>
      summarize(
        n = length(IndexScore),
        Mean = mean(IndexScore),
        SD = sd(IndexScore),
        q30 = qnorm(p = 0.3, mean = Mean, sd = SD),
        q10 = qnorm(p = 0.1, mean = Mean, sd = SD),
        q01 = qnorm(p = 0.01, mean = Mean, sd = SD),
        RB = list(unique(RB))
      ) |>
      ungroup() |>
      mutate(Region = "All regions")
  ) |>
  filter(Flow_SOP2 != "P")

int_ref_distributions <- int_ref_distributions_empirical |>
  rename(q30_e = q30, q10_e = q10, q01_e = q01) |>
  inner_join(
    int_ref_distributions_normal |>
      rename(q30_n = q30, q10_n = q10, q01_n = q01)
  ) |>
  mutate(
    RB = RB
  ) |>
  filter(n != 1) |> 
  mutate(RB = purrr::map_chr(RB, function(x) paste("Regional Board", toString(x)))) |>
  rename(`Flow status` = Flow_SOP2) |>
  mutate(across(Mean:q01_n, .fns = function(x) round(x, 2)))

write.csv(int_ref_distributions, "tables/Part_1_Table_13.csv", row.names = F)
