library(dplyr)
library(ggplot2)

# note that due to the random jitter in the box plot, the x-direction placement of each data point 
# may not be consistent if the plot is regenerated. the y-direction will always be consistent

chan_df <- readr::read_csv("data-raw/Part_2_combined_df_withdata.csv") 

site_info_varz <- c("masterid", "latitude", "longitude", "comid", "huc", "county", "psa_lu", "RB")
class_varz <- c("ISWP2_name", "Watershed", "class_do")
bi_varz <- c("CSCI", "ASCI_D", "ASCI_H") |> sort()

thresholds_df <- tibble(
  Index = c("CSCI", "ASCI_D", "ASCI_H"),
  Ref10 = c(0.79, 0.86, 0.86)
)

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
      levels = c("Watershed", "ISWP2_name", "class_do"),
      labels = c("Watershed", "ISWP", "Bed and Bank")
    ),
    Class = gsub("Soft bottom-", "Soft bottom\n", Class)
  ) |>
  filter(!(Class %in% c("Natural","A","B","Agricultural")))

high_score_boxplot <- ggplot(
    data = chan_bi_df, 
    aes(x = Class, y = Score)
  ) +
  geom_boxplot(outlier.colour = NA, fill = "gray75") +
  geom_point(
    aes(fill = PF, size = PF), 
    position = position_jitter(width = 0.15, height = 0), 
    shape = 21
  ) +
  scale_size_manual(values = c(0.8, 1.5), name = "High score?", labels = c("No", "Yes")) +
  scale_fill_brewer(palette = "Set1", name = "High score?", labels = c("No", "Yes")) +
  geom_hline(data = thresholds_df, aes(yintercept = Ref10), linetype = "dashed", color = "red") +
  facet_grid(Index ~ Classification, drop = T, scales = "free", space = "free") +
  labs(x = "", y = "Index score") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(high_score_boxplot, filename = "figures/Part_2_Figure_38.jpg", dpi = 300, width = 6.5, height = 6.5)
