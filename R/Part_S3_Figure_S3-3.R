library(dplyr)
library(ggplot2)

mydf.c <- readr::read_csv("data-raw/Part_3_mydf.c.csv") |>
  mutate(
    PSA2 = if_else(PSA6c %in% c("NC", "SN"), "Wet", "Arid"),
    Ash_Free_Dry_Mass_gPerm2 = Ash_Free_Dry_Mass_mgPercm2 * 10, # convert from mg/cm2 to g/m2
  )

asci.df.c <- readr::read_csv("data-raw/Part_3_asci.df.c.csv") |>
  mutate(
    PSA2 = if_else(PSA6c %in% c("NC", "SN"), "Wet", "Arid"),
    Ash_Free_Dry_Mass_gPerm2 = Ash_Free_Dry_Mass_mgPercm2 * 10, # convert from mg/cm2 to g/m2
  )

#Biostiulatory variables
chem.varz <- c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL", NULL)
om.varz <- c("Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_gPerm2", "PCT_MAP", NULL)
bs.varz <- c(chem.varz, om.varz)
bs.varz_pretty <- c("Total N", "Total P", "Chl-a", "AFDM", "% cover") #This is for graphs and plots
bs.varz.df <- data.frame(BiostimVar = bs.varz, BSPretty = factor(bs.varz_pretty, levels = bs.varz_pretty))
mod.id.varz <- c("MasterID", "MasterDate", "MasterDate_rep", "DevSet", "SelectedSample", "New_Lat", "New_Long")

mod.dat.csci <- mydf.c |>
  select(all_of(mod.id.varz), CSCI, all_of(chem.varz), all_of(om.varz), Stratum = PSA6c) |>
  rename(IndexScore = CSCI) |>
  mutate(Index = "CSCI")
mod.dat.asci_d <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_D, all_of(chem.varz), all_of(om.varz), Stratum = PSA6c) |>
  rename(IndexScore = ASCI_D) |>
  mutate(Index = "ASCI_D")
mod.dat.asci_h <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_H, all_of(chem.varz), all_of(om.varz), Stratum = PSA6c) |>
  rename(IndexScore = ASCI_H) |>
  mutate(Index = "ASCI_H")

mod.dat <- bind_rows(mod.dat.csci, mod.dat.asci_d, mod.dat.asci_h)

#######
#CREATE MODELS
#######
length.out.x <- 1000

max_TN <- 3
max_TP <- 1.5
max_chl <- 300
max_afdm <- 400
max_cov <- 100

my.newdfs <- data.frame(
  Nitrogen_Total_mgPerL = seq(from = 0, to = max_TN, length.out = length.out.x),
  Phosphorus_as_P_mgPerL = seq(from = 0, to = max_TP, length.out = length.out.x),
  Chlorophyll_a_mgPerm2 = seq(from = 0, to = max_chl, length.out = length.out.x),
  Ash_Free_Dry_Mass_gPerm2 = seq(from = 0, to = max_afdm, length.out = length.out.x),
  PCT_MAP = seq(from = 0, to = max_cov, length.out = length.out.x)
) |>
  tidyr::pivot_longer(cols = everything(), names_to = "BiostimVar", values_to = "Biostim") |>
  tidyr::nest(.by = "BiostimVar")

model_df <- mod.dat |>
  filter(DevSet == "Cal", SelectedSample == "Selected") |>
  tidyr::pivot_longer(cols = Nitrogen_Total_mgPerL:PCT_MAP, names_to = "BiostimVar", values_to = "Biostim") |>
  na.omit() |>
  group_by(Stratum, Index, BiostimVar) |>
  mutate(
    model = list(scam::scam(IndexScore ~ s(Biostim, bs = "mpd")))
  )

my.models.predictions <- model_df |>
  inner_join(my.newdfs) |>
  select(model, data) |>
  distinct() |>
  mutate(
    Fit = list(predict(first(model), newdata = first(data), type = "response", se = T)$fit),
    SE = list(predict(first(model), newdata = first(data), type = "response", se = T)$se.fit)
  ) |>
  tidyr::unnest(cols = c(data, Fit, SE)) |>
  inner_join(bs.varz.df) |>
  select(-model)

obs_points_df <- mod.dat |>
  tidyr::pivot_longer(cols = all_of(bs.varz), names_to = "BiostimVar", values_to = "BiostimValue", values_drop_na = T) |>
  filter(!(BiostimVar == "Nitrogen_Total_mgPerL" & BiostimValue > max_TN)) |>
  filter(!(BiostimVar == "Phosphorus_as_P_mgPerL" & BiostimValue > max_TP)) |>
  filter(!(BiostimVar == "Chlorophyll_a_mgPerm2" & BiostimValue > max_chl)) |>
  filter(!(BiostimVar == "Ash_Free_Dry_Mass_gPerm2" & BiostimValue > max_afdm)) |>
  inner_join(bs.varz.df)

scam_plots <- ggplot(data = my.models.predictions, aes(x = Biostim, y = Fit, color = Stratum)) +
  geom_ribbon(
    mapping = aes(ymin = Fit - 1.96 * SE, ymax = Fit + 1.96 * SE, group = Stratum), 
    color = NA, alpha = 0.2, fill = "#39568cff", show.legend = FALSE
  ) +
  scale_color_manual(values = RColorBrewer::brewer.pal(6, "Set1")) +
  geom_path(linewidth = 1) +
  facet_grid(Index ~ BSPretty, scales = "free_x") +
  coord_cartesian(ylim = c(0, 1.2)) +
  labs(x = "", y = "Index score") +
  theme_bw()

ggsave("figures/Part_S3_Figure_S3-3.jpg", plot = scam_plots, height = 6, width = 10)
