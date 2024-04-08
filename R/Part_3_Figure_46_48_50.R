library(dplyr)
library(ggplot2)

assessment_summary_plot <- function(thresh_df, stringency, applicable_classes, obs_df) {
  my_thresh_df <- thresh_df |>
    filter(Class %in% c("Wadeable streams", applicable_classes)) |>
    filter(Stringency %in% stringency) |>
    inner_join(obs_df) |>
    mutate(
      Indicator_Type = stringr::str_replace(Indicator_Type, "Biostimulatory", "Eutrophication"),
      Threshold_pass = case_when(
        is.na(Observed_value) ~ "No data",
        is.na(Threshold_value) ~ "No threshold identified",
        Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & is.na(Flag) ~ "Fails",
        Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & is.na(Flag) ~ "Passes",
        Indicator_Type == "Biointegrity" & Threshold_value > Observed_value & !is.na(Flag) ~ "Fails but flagged",
        Indicator_Type == "Biointegrity" & Threshold_value <= Observed_value & !is.na(Flag) ~ "Passes flagged",
        Indicator_Type == "Eutrophication" & Threshold_value < Observed_value & is.na(Flag) ~ "Fails",
        Indicator_Type == "Eutrophication" & Threshold_value >= Observed_value & is.na(Flag) ~ "Passes",
        Indicator_Type == "Eutrophication" & Threshold_value < Observed_value & !is.na(Flag) ~ "Fails but flagged",
        Indicator_Type == "Eutrophication" & Threshold_value >= Observed_value & !is.na(Flag) ~ "Passes flagged",
        .default = "Other"
      ),
      obs_label = paste0(Indicator, "\n(", Observed_value, ")"),
      Threshold_pass = factor(
        Threshold_pass, 
        levels = c("Passes", "Passes flagged", "Fails", "Fails but flagged", "No threshold identified", "No data")
      ),
      Approach4 = factor(Approach4, levels = rev(unique(Approach4)))
    )

  threshold_colors <- c("#1f78b4", "#a6cee3", "#e31a1c", "#cab2d6", "#ff7f00", "#fdbf6f")
  assessment_plot <- ggplot(data = my_thresh_df, aes(x = obs_label, y = Approach4)) +
    geom_tile(aes(fill = Threshold_pass), color = "white") +
    geom_text(aes(label = Threshold_value)) +
    facet_wrap(~ Indicator_Type, ncol = 1, scales = "free", drop = T) +
    scale_fill_manual(values = threshold_colors, name = "Threshold", drop = F) +
    labs(y = "", x = "Indicator\n(Observed value)") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.title.x = element_text(color = "gray25"),
      legend.location = "plot",
      legend.key.spacing.y = unit(0, "cm")
    )
  assessment_plot
}

assessment_detail_plot <- function(thresh_df, stringency, applicable_classes, obs_data) {
  thresh_dat <- thresh_df |>
    filter(
      Stringency == stringency,
      Indicator %in% obs_data$Indicator,
      Class %in% c("Wadeable streams", applicable_classes)
    ) |>
    mutate(
      Class = as.character(Class),
      Index = case_when(
        Indicator_Type == "Biointegrity" ~ Indicator,
        Approach == "Response" ~ Response_model_index,
        .default = "Not applicable"
      )
    )
  
  observed_and_grand_mean <- thresh_dat |>
    filter(!Flagged) |>
    group_by(Indicator) |>
    summarize(Value = mean(Threshold_value, na.rm = T), DataType = "Mean of thresholds") |>
    bind_rows(
      obs_data |>
        mutate(DataType = "Observed value") |>
        rename(Value = Observed_value)
    )

  unflagged_thresholds <- thresh_dat |> 
    filter(!Flagged) |>
    group_by(Class, Indicator) |>
    mutate(n = n()) |>
    filter(n > 1)
  
  ggplot(data = thresh_dat, aes(x = Class, y = Threshold_value)) +
    geom_point(aes(fill = Approach5, shape = Index, size = Flagged)) +
    stat_summary(data = unflagged_thresholds, fun = "mean", geom = "crossbar", linewidth = 0.25) +
    geom_hline(data = observed_and_grand_mean, aes(yintercept = Value, color = DataType)) +
    scale_color_manual(
      name = "", values = c("black", "violet"), 
      labels = c("Mean of unflagged thresholds\n(Within or across classes)", "Observed value")
    ) +
    facet_wrap(~ Indicator, scales = "free", ncol = 2) +
    scale_shape_manual(values = c(24, 25, 22, 21), name = "Response model index") +
    scale_fill_manual(values = c("#e41a1c", "#377eb8", "#33a02c", "#b2df8a"), name = "Approach") +
    scale_size_manual(values = c(2, 1), name = "Flagged?", labels = c("No", "Yes")) +
    theme_bw() +
    coord_flip() +
    guides(
      fill = guide_legend(override.aes = list(shape = 21, size = 2), order = 1),
      shape = guide_legend(override.aes = list(fill = "gray", size = 2), order = 2),
      size = guide_legend(order = 3),
      color = guide_legend(order = 4)
    ) +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    labs(x = "", y = "")
}

thresh_df <- readr::read_csv("data-raw/Part_3_mod_channel_thresholds.csv") |>
  mutate(
    Indicator = factor(Indicator, levels = unique(Indicator)),
    Class = factor(Class, levels = c("Wadeable streams", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC")),
    Flagged = !is.na(Flag),
    Approach = factor(Approach, levels = c("Reference", "Best observed", "Response")),
    Response_model_form = factor(Response_model_form, levels = c("LR", "SCAM")),
    Response_model_index = factor(Response_model_index, levels = c("ASCI_D", "ASCI_H", "CSCI")),
    Approach4 = case_when(
      Approach != "Response" ~ paste0(Class, " - ", Approach),
      Approach == "Response" ~ paste0(Class, " - ", Approach, " (", Response_model_form, ", ", Response_model_index, ")"),
      .default = "OTHER"
    ),
    Approach5 = if_else(Approach == "Response", paste0("Response (", Response_model_form, ")"), Approach)
  ) |>
  arrange(Class, Approach, Response_model_form, Response_model_index) 


# Example from Elder Creek
elder_creek_obs_df <- tibble(
  Indicator = factor(c("CSCI", "ASCI_D", "ASCI_H", "TN", "TP", "Chl-a", "AFDM", "% cover")),
  Observed_value = c(0.39, 0.97, 1.02, 0.79, 1.03, 13.26, 9.2, NA_real_)
)

# Example from Magpie Creek
magpie_creek_obs_df<- tibble(
  Indicator = factor(c("CSCI", "ASCI_D", "ASCI_H", "TN", "TP", "Chl-a", "AFDM", "% cover")),
  Observed_value = c(0.21, 0.92, 0.82, 0.64, 0.20, 154, 83, 41)
)

# Example from Pine Creek
pine_creek_obs_df <- tibble(
  Indicator = factor(c("CSCI", "ASCI_D", "ASCI_H", "TN", "TP", "Chl-a", "AFDM", "% cover")),
  Observed_value = c(0.83, 0.9, 0.94, 0.07, 0.013, 33.3, 11, NA_real_)
)


## Figure 46 ####
elder_creek_summary <- assessment_summary_plot(thresh_df, "Intermediate", c("CVF", "SB0"), elder_creek_obs_df)
elder_creek_detail <- assessment_detail_plot(thresh_df, "Intermediate", c("CVF", "SB0"), elder_creek_obs_df)

ggsave(elder_creek_summary, filename = "figures/Part_3_Figure_46_summary.jpg", dpi = 300, height = 7.5, width = 6.5)
ggsave(elder_creek_detail, filename = "figures/Part_3_Figure_46_detail.jpg", dpi = 300, height = 8, width = 7.75)

## Figure 48 ####
magpie_creek_summary <- assessment_summary_plot(thresh_df, "Intermediate", c("CVF", "HB"), magpie_creek_obs_df)
magpie_creek_detail <- assessment_detail_plot(thresh_df, "Intermediate", c("CVF", "HB"), magpie_creek_obs_df)

ggsave(magpie_creek_summary, filename = "figures/Part_3_Figure_48_summary.jpg", dpi = 300, height = 7.5, width = 6.5)
ggsave(magpie_creek_detail, filename = "figures/Part_3_Figure_48_detail.jpg", dpi = 300, height = 8, width = 7.75)

## Figure 50 ####
pine_creek_summary <- assessment_summary_plot(thresh_df, "High", c("CVF", "RFI-N"), pine_creek_obs_df)
pine_creek_detail <- assessment_detail_plot(thresh_df, "High", c("CVF", "RFI-N"), pine_creek_obs_df)

ggsave(pine_creek_summary, filename = "figures/Part_3_Figure_50_summary.jpg", dpi = 300, height = 7.5, width = 6.5)
ggsave(pine_creek_detail, filename = "figures/Part_3_Figure_50_detail.jpg", dpi = 300, height = 8, width = 7.75)

