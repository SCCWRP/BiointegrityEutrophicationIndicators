library(dplyr)
library(ggplot2)

mydf.c3 <- readr::read_csv('data-raw/Part_1_mydf.c3.csv')
thresholds.df <- readr::read_csv('data-raw/Part_1_thresholds.df.csv')
thresholds.bs.df <- readr::read_csv('data-raw/Part_1_thresholds.bs.df.csv')

get_lowest_validated_threshold <- function(thresholds.bs.df, BSPretty_) {
  thresholds.bs.df |>
    filter(
      BSPretty == BSPretty_,
      BIgoal == "Ref10",
      Prob == "p80",
      Stratum == "California"
    ) |>
    pull(LowestValidatedThreshold)
}

Ref10p80_Conc <- mydf.c3 |>
  filter(SelectedSample == "Selected") |>
  na.omit() |>
  mutate(
    ASCI_D_pass = ASCI_D >= thresholds.df$Ref10[which(thresholds.df$Index == "ASCI_D")],
    ASCI_H_pass = ASCI_H >= thresholds.df$Ref10[which(thresholds.df$Index == "ASCI_H")],
    CSCI_pass = CSCI >= thresholds.df$Ref10[which(thresholds.df$Index == "CSCI")],
    
    tn_pass = Nitrogen_Total_mgPerL <= get_lowest_validated_threshold(thresholds.bs.df, "Total N"),
    tp_pass = Phosphorus_as_P_mgPerL <= get_lowest_validated_threshold(thresholds.bs.df, "Total P"),
    chla_pass = Chlorophyll_a_mgPerm2 <= get_lowest_validated_threshold(thresholds.bs.df, "Chl-a"),
    afdm_pass = Ash_Free_Dry_Mass_mgPercm2 <= get_lowest_validated_threshold(thresholds.bs.df, "AFDM"),
    map_pass = PCT_MAP <= get_lowest_validated_threshold(thresholds.bs.df, "% cover"),
         
    nBIPass = ASCI_D_pass + ASCI_H_pass + CSCI_pass,
    nBSPass = tn_pass + tp_pass + chla_pass + afdm_pass + map_pass,
    BIPassAll = nBIPass == 3,
    BSPassAll = nBSPass == 5
  )

Ref10p80_Conc.sum <- Ref10p80_Conc |>
  group_by(DevSet, nBSPass) |>
  summarize(
    n = length(nBIPass),
    None = sum(nBIPass == 0),
    One = sum(nBIPass == 1),
    Two = sum(nBIPass == 2),
    Three = sum(nBIPass == 3)
  ) |>
  tidyr::pivot_longer(cols = c(None, One, Two, Three), names_to = "BIPassed") |>
  mutate(BIPassed = factor(BIPassed, levels = c("None", "One", "Two", "Three"))) |>
  filter(DevSet == "Cal")

## Figure 17 ####

targets_vs_indices_stacked_bar <- ggplot(
    data = Ref10p80_Conc.sum, 
    aes(x = as.factor(nBSPass), y = 100 * value / n, fill = BIPassed)
  ) +
  geom_bar(position = position_stack(), stat = "identity") +
  scale_fill_brewer(palette = "Blues", name = "Biointegrity\nindices\npassed") +
  ylab("% sites") +
  theme_bw() +
  scale_x_discrete(
    labels = c("None", "One", "Two", "Three", "Four", "Five"), 
    name = "Eutrophication targets met"
  ) +
  theme(panel.grid = element_blank())
ggsave(targets_vs_indices_stacked_bar, filename = "figures/Part_1_Figure_17.jpg", dpi = 300, height = 4, width = 5)


## Figure 18 ####
mydf.c3_fig_18 <- mydf.c3 |>
  mutate(
    StudyArea = case_when(
      PSA6c == "CV" ~ "Central Valley",
      PSA6c == "DM" & New_Lat > 39 ~ "Modoc Plateau",
      .default = "Other"
    )
  ) |>
  filter(StudyArea %in% c("Central Valley", "Modoc Plateau"))

Ref10p80_Conc_fig_18 <- mydf.c3_fig_18 |>
  filter(SelectedSample == "Selected") |>
  na.omit() |>
  mutate(
    ASCI_D_pass = ASCI_D >= thresholds.df$Ref10[which(thresholds.df$Index == "ASCI_D")],
    ASCI_H_pass = ASCI_H >= thresholds.df$Ref10[which(thresholds.df$Index == "ASCI_H")],
    CSCI_pass = CSCI >= thresholds.df$Ref10[which(thresholds.df$Index == "CSCI")],
    
    tn_pass = Nitrogen_Total_mgPerL <= get_lowest_validated_threshold(thresholds.bs.df, "Total N"),
    tp_pass = Phosphorus_as_P_mgPerL <= get_lowest_validated_threshold(thresholds.bs.df, "Total P"),
    chla_pass = Chlorophyll_a_mgPerm2 <= get_lowest_validated_threshold(thresholds.bs.df, "Chl-a"),
    afdm_pass = Ash_Free_Dry_Mass_mgPercm2 <= get_lowest_validated_threshold(thresholds.bs.df, "AFDM"),
    map_pass = PCT_MAP <= get_lowest_validated_threshold(thresholds.bs.df, "% cover"),
    
    nBIPass = ASCI_D_pass + ASCI_H_pass + CSCI_pass,
    nBSPass = tn_pass + tp_pass + chla_pass + afdm_pass + map_pass,
    BIPassAll = nBIPass == 3,
    BSPassAll = nBSPass == 5
  )


Ref10p80_Conc_fig_18.sum <- Ref10p80_Conc_fig_18 |>
  group_by(StudyArea, nBSPass) |>
  summarize(
    n = length(nBIPass),
    None = sum(nBIPass == 0),
    One = sum(nBIPass == 1),
    Two = sum(nBIPass == 2),
    Three = sum(nBIPass == 3)
  ) |>
  ungroup() |>
  group_by(StudyArea) |>
  mutate(n_tot = sum(n)) |>
  ungroup() |>
  tidyr::pivot_longer(cols = c(None, One, Two, Three), names_to = "BIPassed") |>
  mutate(
    BIPassed = factor(BIPassed, levels = c("None", "One", "Two", "Three")),
    nBSPass = factor(nBSPass),
    nscl = scales::rescale(n, to = c(0.2, 0.95))
  )
  
targets_vs_indices_stacked_bar_fig_18 <- ggplot(
    data = Ref10p80_Conc_fig_18.sum, 
    aes(x = nBSPass, y = 100 * value / n, fill = BIPassed, width = nscl)
  ) +
  geom_bar(position = position_stack(), stat = "identity", color = "gray") +
  scale_fill_brewer(palette = "Blues", name = "Biointegrity\nindices\npassed") +
  ylab("% sites") +
  theme_bw() +
  scale_x_discrete(
    labels = c("None", "One", "Two", "Three", "Four", "Five"), 
    name = "Eutrophication targets met"
  ) +
  facet_wrap(~StudyArea, ncol = 1) +
  theme(panel.grid = element_blank())

ggsave(targets_vs_indices_stacked_bar_fig_18, filename = "figures/Part_1_Figure_18.jpg", height = 6, width = 5)
