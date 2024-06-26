library(dplyr)

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

mod.dat.csci_CA <- mydf.c |>
  mutate(Stratum = "California") |>
  select(all_of(mod.id.varz), CSCI, all_of(chem.varz), all_of(om.varz), Stratum) |>
  rename(IndexScore = CSCI) |>
  mutate(Index = "CSCI")
mod.dat.asci_d_CA<- asci.df.c |>
  mutate(Stratum = "California") |>
  select(all_of(mod.id.varz), ASCI_D, all_of(chem.varz), all_of(om.varz), Stratum) |>
  rename(IndexScore = ASCI_D) |>
  mutate(Index = "ASCI_D")
mod.dat.asci_h_CA <- asci.df.c |>
  mutate(Stratum = "California") |>
  select(all_of(mod.id.varz), ASCI_H, all_of(chem.varz), all_of(om.varz), Stratum) |>
  rename(IndexScore = ASCI_H) |>
  mutate(Index = "ASCI_H")

mod.dat.csci_aw <- mydf.c |>
  select(all_of(mod.id.varz), CSCI, all_of(chem.varz), all_of(om.varz), Stratum = PSA2) |>
  rename(IndexScore = CSCI) |>
  mutate(Index = "CSCI")
mod.dat.asci_d_aw <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_D, all_of(chem.varz), all_of(om.varz), Stratum = PSA2) |>
  rename(IndexScore = ASCI_D) |>
  mutate(Index = "ASCI_D")
mod.dat.asci_h_aw <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_H, all_of(chem.varz), all_of(om.varz), Stratum = PSA2) |>
  rename(IndexScore = ASCI_H) |>
  mutate(Index = "ASCI_H")

mod.dat.csci_eco <- mydf.c |>
  select(all_of(mod.id.varz), CSCI, all_of(chem.varz), all_of(om.varz), Stratum = PSA6c) |>
  rename(IndexScore = CSCI) |>
  mutate(Index = "CSCI")
mod.dat.asci_d_eco <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_D, all_of(chem.varz), all_of(om.varz), Stratum = PSA6c) |>
  rename(IndexScore = ASCI_D) |>
  mutate(Index = "ASCI_D")
mod.dat.asci_h_eco <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_H, all_of(chem.varz), all_of(om.varz), Stratum = PSA6c) |>
  rename(IndexScore = ASCI_H) |>
  mutate(Index = "ASCI_H")


mod.dat <- bind_rows(
  mod.dat.csci_CA, mod.dat.asci_d_CA, mod.dat.asci_h_CA,
  mod.dat.csci_aw, mod.dat.asci_d_aw, mod.dat.asci_h_aw,
  mod.dat.csci_eco, mod.dat.asci_d_eco, mod.dat.asci_h_eco,
)

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

model.summary <- mod.dat |>
  filter(DevSet == "Cal", SelectedSample == "Selected") |>
  tidyr::pivot_longer(cols = Nitrogen_Total_mgPerL:PCT_MAP, names_to = "BiostimVar", values_to = "Biostim") |>
  na.omit() |>
  group_by(Stratum, Index, BiostimVar) |>
  mutate(
    model = list(scam::scam(IndexScore ~ s(Biostim, bs = "mpd"))),
    null_model = list(scam::scam(IndexScore ~ 1)),
    AIC = purrr::map_dbl(model, function(mod) mod$aic),
    AIC_null = purrr::map_dbl(null_model, function(mod) mod$aic),
    summary = purrr::map(model, function(mod) summary(mod)),
    summ_perf = purrr::map(summary, function(summ) {
      list(rsq = summ$r.sq, dev = summ$dev.expl, biostim_F = summ$s.table[3], biostim_p = summ$s.table[4], n = summ$n)
    })
  ) |>
  tidyr::unnest_wider(summ_perf) |>
  ungroup() |>
  inner_join(bs.varz.df, by = "BiostimVar") |>
  distinct(BSPretty, Index, Stratum, AIC, AIC_null, dev, rsq, biostim_F, biostim_p, n) |>
  mutate(
    Stratum_set = case_when(
      Stratum == "California" ~ 1,
      Stratum %in% c("Arid", "Wet") ~ 2,
      .default = 3
    ),
    AIC = round(AIC, 1),
    AIC_null = round(AIC_null, 1),
    dev = round(dev * 100, 1),
    rsq = round(rsq, 3),
    biostim_F = round(biostim_F, 1)
  ) |>
  arrange(Stratum_set, BSPretty, Index, Stratum) |>
  select(
    `Eutrophication indicator` = BSPretty, Index, Stratum, AIC, AIC_null,
    `Deviance explained` = dev, `R^2` = rsq, `F` = biostim_F, p = biostim_p, n
  )

write.csv(model.summary, file = "tables/Part_S3_Table_S3.csv", row.names = FALSE)