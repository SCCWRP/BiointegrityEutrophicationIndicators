library(dplyr)
library(ggplot2)

thresholds.df <- data.frame(
  Index = c("CSCI", "ASCI_D", "ASCI_H"),
  BCG2 = c(1.025, 1.31, 1.23),
  BCG3 = c(0.825, 0.95, 0.97),
  BCG4 = c(0.625, 0.54, 0.67),
  BCG5 = c(0.325, 0, 0.30),
  BCG6 = c(0, NA_real_, 0)
)

thresholds.df2 <- thresholds.df |>
  tidyr::pivot_longer(cols = starts_with("BCG"))

mydf <- readr::read_csv("data-raw/Part_3_thresholds_df_formatted.csv") |>
  rename(Thresh_Hi = High, Thresh_Int = Medium, Thresh_Low = Low) |>
  filter(
    Population %in% c(
      "Reference", "CVFloor", "Hard bottom", "Soft bottom-2 hard sides", 
      "Soft bottom-1 hard side", "Soft bottom-0 hard sides", "Ambiguous"
    )
  ) |>
  mutate(
    Population = case_when(
      Population == "Reference" ~ "Wadeable (standard)",
      Population == "CVFloor" ~ "CVF",
      Population == "Hard bottom" ~ "HB",
      Population == "Soft bottom-2 hard sides" ~ "SB2",
      Population == "Soft bottom-1 hard side" ~ "SB1",
      Population == "Soft bottom-0 hard sides" ~ "SB0",
      Population == "Ambiguous" ~ "CC",
      .default = "OTHER"
    )
  )

mydf2 <- mydf |> 
  select(Population, Index, starts_with("Thresh_")) |>
  tidyr::pivot_longer(cols = starts_with("Thresh_")) |>
  filter(!Population %in% c("RFI-N", "RFI-S")) |>
  mutate(
    Stringency = case_when(
      name == "Thresh_Hi" ~ "High",
      name=="Thresh_Int"~"Intermediate",
      name=="Thresh_Low"~"Low",T~"Other"
    ),
    Population = factor(
      Population, 
      levels = c("CC", "SB0", "SB1", "SB2", "HB", "CVF", "Wadeable (standard)"),
      labels = c("CC", "SB0", "SB1", "SB2", "HB", "CVF", "Standard")
    )
  )


trad_thresholds_df <- tidyr::crossing(
    Index = c("ASCI_D", "ASCI_H", "CSCI"),
    Stringency = c("High","Intermediate","Low")
  ) |>
  mutate(
    value = case_when(
      Index %in% c("ASCI_D", "ASCI_H") & Stringency == "High" ~ 0.94,
      Index %in% c("ASCI_D", "ASCI_H") & Stringency == "Intermediate" ~ 0.86,
      Index %in% c("ASCI_D", "ASCI_H") & Stringency == "Low" ~ 0.75,
      Index %in% c("CSCI") & Stringency == "High" ~ 0.92,
      Index %in% c("CSCI") & Stringency == "Intermediate" ~ 0.79,
      Index %in% c("CSCI") & Stringency == "Low" ~ 0.63,
      .default = -Inf
    )
  )

bcg_comparison_plot_lines <- ggplot() +
  geom_rect(
    data = thresholds.df, fill = "#bdbdbd50",
    aes(xmin = -Inf, xmax = Inf, ymin = BCG5, ymax = BCG4)
  ) +
  geom_rect(
    data = thresholds.df, fill = "#bdbdbd50",
    aes(xmin = -Inf, xmax = Inf, ymin = BCG3, ymax = BCG2)
  ) +
  geom_point(
    data = mydf2 |> filter(Population != "Standard"), size = 2,
    aes(color = Stringency, x = Population, y = value)
  ) +
  facet_wrap(~ Index, ncol = 1) +
  geom_hline(data = trad_thresholds_df, aes(yintercept = value), linetype = "dashed") +
  coord_flip() +
  geom_text(
    data = thresholds.df2, vjust = -0.1, hjust = -0.1, size = 3,
    aes(y = value, x = "Standard", label = name)
  ) +
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_color_viridis_d(end = 0.9, guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(drop = F, labels = c("CC", "SB0", "SB1", "SB2", "HB", "CVF", " ")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

ggsave(bcg_comparison_plot_lines, filename = "figures/Part_3_Figure_51.jpg", dpi = 300, height = 7, width = 6.5)  
