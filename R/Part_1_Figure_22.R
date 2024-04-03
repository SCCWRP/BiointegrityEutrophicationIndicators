library(dplyr)
library(ggplot2)

nonperen_df <- readr::read_csv("data-raw/Part_1_Andy_Nonperen_data_ASCI.csv") |>
  mutate(
    masterid = StationCode,
    sampledate = lubridate::mdy(SampleDate)
  )

plot_dat <- nonperen_df |>
  mutate(Flow_SOP2 = if_else(Flow_SOP == "RFI_SFI", "SFI", Flow_SOP)) |>
  group_by(RB, StationCode, Flow_SOP, Flow_SOP2) |>
  summarize(
    CSCI = mean(CSCI, na.rm = T),
    ASCI_D = mean(ASCI_D, na.rm = T),
    ASCI_H = mean(ASCI_H, na.rm = T)) |>
  ungroup() |>
  mutate(Region = if_else(RB %in% c(4,7,8,9), "Southern", "Northern"))

plot_dat_pivot <- plot_dat |>
  tidyr::pivot_longer(
    cols = c("CSCI", "ASCI_D", "ASCI_H"), 
    names_to = "Index", values_to = "IndexScore", values_drop_na = T
  )

plot_dat_pivot_thresholds <- tibble::tibble(
    Index = c("CSCI", "ASCI_D", "ASCI_H"),
    q50 = 1,
    q10 = c(.79, .84, .84)
  ) |>
  tidyr::pivot_longer(cols = c(q50, q10), values_to = "Threshold")


# note that the random jitter in position_jitterdodge will lead to random
# horizontal positions for each data point, but the vertical positions will 
# be the same
intermittent_v_perennial_scores_2regions <- ggplot(
    data = plot_dat_pivot, 
    aes(x = Region, y = IndexScore, fill = Flow_SOP2)
  ) +
  geom_boxplot(outlier.color = NA) +
  geom_point(shape = 21, position = position_jitterdodge(jitter.width = 0.125, jitter.height = 0)) +
  geom_hline(data = plot_dat_pivot_thresholds, aes(yintercept = Threshold), linetype = "dashed") +
  scale_fill_brewer(palette = "Blues", direction = -1, name = "Flow status") +
  ylab("") +
  scale_x_discrete(name = "", labels = c("Northern\n(Regions 1, 2, 3, and 5)", "Southern\n(Regions 7 and 9)")) +
  facet_wrap(~Index, scales = "free_y", ncol = 1) +
  theme_bw()

ggsave(
  intermittent_v_perennial_scores_2regions, 
  filename = "figures/Part_1_Figure_22.jpg", 
  dpi = 300, height = 6, width = 6.5
)
