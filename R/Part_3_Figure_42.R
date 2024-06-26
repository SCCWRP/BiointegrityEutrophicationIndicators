library(dplyr)
library(ggplot2)

set.seed(42)

mydf <- tibble(value = rnorm(1000, mean = 1, sd = 0.1)) |>
  mutate(value = if_else(value < 0, 0, value))

myquants <- mydf |>
  summarize(
    q01 = quantile(value, 0.01),
    q10 = quantile(value, 0.1),
    q30 = quantile(value, 0.3),
    q70 = quantile(value, 0.7),
    q90 = quantile(value, 0.9),
    q99 = quantile(value, 0.99)
  ) |>
  tidyr::pivot_longer(cols = everything(), names_to = "quant", values_to = "x") |>
  tidyr::crossing(
    data.frame(
      group = c(
        "Reference biointegrity thresholds", 
        "Reference eutrophication thresholds", 
        "Best observed biointegrity thresholds",
        "Best observed eutrophication thresholds"
      )
    )
  ) |>
  mutate(
    label = case_when(
      group == "Reference biointegrity thresholds" & quant == "q01" ~ "Low\nStringency",
      group == "Reference biointegrity thresholds" & quant == "q10" ~ "Intermediate\nStringency",
      group == "Reference biointegrity thresholds" & quant == "q30" ~ "High\nStringency",
      group == "Reference eutrophication thresholds" & quant == "q70" ~ "High\nStringency",
      group == "Reference eutrophication thresholds" & quant == "q90" ~ "Intermediate\nStringency",
      group == "Reference eutrophication thresholds" & quant == "q99" ~ "Low\nStringency",
      group == "Best observed biointegrity thresholds" & quant == "q99" ~ "High\nStringency",
      group == "Best observed biointegrity thresholds" & quant == "q90" ~ "Intermediate\nStringency",
      group == "Best observed biointegrity thresholds" & quant == "q70" ~ "Low\nStringency",
      group == "Best observed eutrophication thresholds" & quant == "q30" ~ "Low\nStringency",
      group == "Best observed eutrophication thresholds" & quant == "q10" ~ "Intermediate\nStringency",
      group == "Best observed eutrophication thresholds" & quant == "q01" ~ "High\nStringency",
      .default = NA_character_
    ),
    y = case_when(
      quant %in% c("q01", "q99") ~ 70,
      quant %in% c("q10", "q90") ~ 85,
      quant %in% c("q30", "q70") ~ 100
    ),
    group = factor(group, levels = c("Reference biointegrity thresholds", "Reference eutrophication thresholds", "Best observed biointegrity thresholds", "Best observed eutrophication thresholds"))
  ) |>
  filter(!is.na(label))

make_plot <- function(mydf, myquants, group_) {
  plot_data <- myquants |> filter(group == group_)
  ggplot() +
    geom_histogram(data = mydf, aes(x = value)) +
    geom_vline(data = plot_data, aes(xintercept = x)) +
    geom_label(data = plot_data, aes(x = x, label = label, y = y), size.unit = "pt", size = 7, vjust = 0) +
    scale_x_continuous(breaks = plot_data$x) +
    labs(x = "", y = "", title = group_) +
    ylim(0, 110) +
    theme_classic() +
    theme(
      title = element_text(size = 10),
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line = element_blank()
    )
}

ref_best_distributions_1 <- make_plot(mydf, myquants, "Reference biointegrity thresholds")
ref_best_distributions_2 <- make_plot(mydf, myquants, "Best observed biointegrity thresholds")
ref_best_distributions_3 <- make_plot(mydf, myquants, "Reference eutrophication thresholds")
ref_best_distributions_4 <- make_plot(mydf, myquants, "Best observed eutrophication thresholds")

combined_plots <- ggpubr::ggarrange(
  ref_best_distributions_1, ref_best_distributions_2, 
  ref_best_distributions_3, ref_best_distributions_4, 
  nrow = 2, ncol = 2
)

ggsave(combined_plots, filename = "figures/Part_3_Figure_42.jpg", height = 6, width = 6.5, dpi = 300)
