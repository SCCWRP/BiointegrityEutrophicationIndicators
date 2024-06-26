library(dplyr)

# depends on tables 22, 23 being generated first, see R/Part_3_Table_22_23_24.R
# necessary for Part_3_Figure_46_48_50.R

class_names <- data.frame(
  Class = factor(
    c("Wadeable streams", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC", "SB0_CVF", "HB_CVF", "CC_CVF"),
    levels = c("Wadeable streams", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC", "SB0_CVF", "HB_CVF", "CC_CVF")
  ),
  Class_fullname = c(
    "Wadeable streams", "Intermittent northern California", "Intermittent southern California", 
    "Central Valley Floor", "Hard bottom", "Soft bottom engineered channel, two hardened sides",
    "Soft bottom engineered channel, one hardened side", "Soft bottom engineered channel, no hardened sides",
    "Constructed channel", "Soft bottom engineered channel, no hardened sides (Central Valley Floor)",
    "Hard bottom (Central Valley Floor)", "Constructed channel (Central Valley Floor)"
  )
)

index_thresholds <- readr::read_csv(
    "data-raw/Part_1_thresholds.df.csv", 
    col_select = c(Index, High = Ref30, Intermediate = Ref10, Low = Ref01)
  ) |>
  tidyr::pivot_longer(cols = High:Low, names_to = "Stringency", values_to = "Response_model_goal")

# biointegrity thresholds ####

bi_ref_best <- readr::read_csv('tables/Part_3_Table_22.csv') |>
  tidyr::pivot_longer(cols = High:Low, names_to = "Stringency", values_to = "Threshold") |>
  transmute(
    Class = factor(
      Population, 
      levels = c("Wadeable (standard)", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC", "SB0_CVF", "HB_CVF", "CC_CVF"),
      labels = c("Wadeable streams", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC", "SB0_CVF", "HB_CVF", "CC_CVF")
    ),
    Approach = factor(`Threshold type`, levels = c("Reference", "Best observed")),
    Indicator = factor(Index, levels = c("CSCI", "ASCI_D", "ASCI_H")),
    Stringency,
    `Indicator type` = "Biointegrity",
    Threshold,
    Details = case_when(
      Approach == "Reference" & Stringency == "High" ~ "30th percentile of reference values",
      Approach == "Reference" & Stringency == "Intermediate" ~ "10th percentile of reference values",
      Approach == "Reference" & Stringency == "Low" ~ "1st percentile of reference values",
      Approach == "Best observed" & Stringency == "High" ~ "99th percentile of observed values",
      Approach == "Best observed" & Stringency == "Intermediate" ~ "90th percentile of observed values",
      Approach == "Best observed" & Stringency == "Low" ~ "70th percentile of observed values",
      .default = ""
    )
  ) |>
  group_by(Indicator, Stringency) |>
  mutate(
    Flag = case_when(
      Threshold > Threshold[Class == "Wadeable streams"] ~ "Biointegrity goal is higher than reference.", 
      is.na(Threshold) ~ "Insufficient data",
      .default = ""
    ),
    Response_model_form = "",
    Response_model_index = "",
    Response_model_goal = NA_real_,
    Response_model_detail = ""
  ) |>
  ungroup() |>
  inner_join(class_names) |>
  arrange(Indicator, Approach, Class) |>
  select(
    Class, Class_fullname, Approach, Response_model_form, Response_model_index, 
    Response_model_goal, Response_model_detail, Details, Stringency, `Indicator type`, 
    Indicator, Threshold, Flag
  )

# Reference + best observed Eutrophication thresholds ####

# table 23 does not include CC Class because of insufficient data, so add in NA placeholders
CC_class <- readr::read_csv('tables/Part_3_Table_23.csv') |>
  distinct(Indicator) |>
  mutate(
    `Threshold type` = "Best observed",
    `Stream class` = "CC",
    n = NA_real_,
    High = NA_real_,
    Intermediate = NA_real_,
    Low = NA_real_
  )


eu_ref_best <- readr::read_csv('tables/Part_3_Table_23.csv') |>
  bind_rows(CC_class) |>
  tidyr::pivot_longer(cols = High:Low, names_to = "Stringency", values_to = "Threshold") |>
  transmute(
    Class = factor(
      `Stream class`, 
      levels = c("Wadeable (standard)", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC"),
      labels = c("Wadeable streams", "RFI-N", "RFI-S", "CVF", "HB", "SB2", "SB1", "SB0", "CC")
    ),
    Indicator = stringr::str_split_i(Indicator, " \\(", 1),
    Indicator = factor(Indicator, levels = unique(Indicator)),
    Approach = factor(`Threshold type`, levels = c("Reference", "Best observed")),
    Stringency,
    `Indicator type` = "Eutrophication",
    Threshold,
    Details = case_when(
      Approach == "Reference" & Stringency == "High" ~ "70th percentile of reference values",
      Approach == "Reference" & Stringency == "Intermediate" ~ "90th percentile of reference values",
      Approach == "Reference" & Stringency == "Low" ~ "99th percentile of reference values",
      Approach == "Best observed" & Stringency == "High" ~ "1st percentile of observed values",
      Approach == "Best observed" & Stringency == "Intermediate" ~ "10th percentile of observed values",
      Approach == "Best observed" & Stringency == "Low" ~ "30th percentile of observed values",
      .default = ""
    ),
    Flag = if_else(is.na(Threshold), "Insufficient data", ""),
    Response_model_form = "",
    Response_model_index = "",
    Response_model_goal = NA_real_,
    Response_model_detail = ""
  ) |>
  inner_join(class_names) |>
  arrange(Indicator, Approach, Class) |>
  select(
    Class, Class_fullname, Approach, Response_model_form, Response_model_index, 
    Response_model_goal, Response_model_detail, Details, Stringency, `Indicator type`, 
    Indicator, Threshold, Flag
  )
  
# Eutrophication Logistic regression thresholds ####
mazor2022_thresholds <- readr::read_csv("data-raw/Part_3_model.summary_thresholds_woInorganics.csv") |>
  filter(BIgoal %in% c("Ref01", "Ref10", "Ref30"), Stratum == "California") |>
  transmute(
    Class = "Wadeable streams",
    Response_model_index = Response,
    `Indicator type` = "Eutrophication", 
    Approach = "Response",
    Indicator = case_when(
      BSPretty == "Total N" ~ "TN",
      BSPretty == "Total P" ~ "TP",
      .default = BSPretty
    ),
    Indicator = factor(Indicator, levels = unique(Indicator)),
    Stringency = case_when(
      BIgoal == "Ref30" ~ "High",
      BIgoal == "Ref10" ~ "Intermediate",
      BIgoal == "Ref01" ~ "Low",
      .default = "x"
    ),
    Threshold = if_else(is.infinite(p80), NA_real_, p80),
    Threshold = if_else(Indicator == "AFDM", Threshold * 10, Threshold), # convert from mg/cm2 to g/m2
    Flag = if_else(is.na(Threshold), "No threshold identified", ""),
    Response_model_form = "LR",
  ) |>
  inner_join(index_thresholds, by = join_by(Stringency == Stringency, Response_model_index == Index)) |>
  mutate(
    Response_model_detail = glue::glue(
      "Logistic model, 80% probability of attaining {Response_model_index} score above {Response_model_goal}."
    ),
    Details = Response_model_detail
  ) |>
  inner_join(class_names, by = join_by(Class == Class)) |>
  arrange(Indicator, Approach, Class, Response_model_index, Stringency) |>
  select(
    Class, Class_fullname, Approach, Response_model_form, Response_model_index, 
    Response_model_goal, Response_model_detail, Details, Stringency, `Indicator type`, 
    Indicator, Threshold, Flag
  )


# Eutrophication Scam model thresholds ####
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

#Create vectors of modeling variables
#Biostiulatory variables
chem.varz <- c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL")
om.varz <- c("Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_gPerm2", "PCT_MAP")
bs.varz <- c(chem.varz, om.varz)
bs.varz_pretty <- c("Total N", "Total P", "Chl-a", "AFDM", "% cover") #This is for graphs and plots
bs.varz.df <- data.frame(BiostimVar = bs.varz, BSPretty = factor(bs.varz_pretty, levels = bs.varz_pretty))
mod.id.varz <- c("MasterID", "MasterDate", "MasterDate_rep", "DevSet", "SelectedSample", "PSA2", "New_Lat", "New_Long")

mod.dat.csci <- mydf.c |>
  select(all_of(mod.id.varz), CSCI, all_of(chem.varz), all_of(om.varz)) |>
  rename(IndexScore = CSCI) |>
  mutate(Index = "CSCI")
mod.dat.asci_d<- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_D, all_of(chem.varz), all_of(om.varz)) |>
  rename(IndexScore = ASCI_D) |>
  mutate(Index = "ASCI_D")
mod.dat.asci_h <- asci.df.c |>
  select(all_of(mod.id.varz), ASCI_H, all_of(chem.varz), all_of(om.varz)) |>
  rename(IndexScore = ASCI_H) |>
  mutate(Index = "ASCI_H")

mod.dat <- bind_rows(mod.dat.csci, mod.dat.asci_d, mod.dat.asci_h)

#CREATE MODELS
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
  group_by(Index, BiostimVar) |>
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
  tidyr::unnest(cols = c(data, Fit, SE))


scam_thresholds <- bi_ref_best |>
  filter(!(Class %in% c("SB0_CVF", "HB_CVF", "CC_CVF"))) |>
  unique() |>
  mutate(Threshold = round(Threshold, 2)) |>
  tidyr::crossing(BiostimVar = bs.varz) |> 
  inner_join(bs.varz.df, by = join_by(BiostimVar)) |>
  left_join(my.models.predictions, by = join_by(Indicator == Index, BiostimVar == BiostimVar, closest(Threshold < Fit))) |>
  select(-c(Fit, SE)) |>
  mutate(
    Response_model_index = Indicator,
    Response_model_goal = Threshold,
    Response_model_form = "SCAM",
    `Indicator type` = "Eutrophication",
    Indicator = case_when(
      BSPretty == "Total N" ~ "TN",
      BSPretty == "Total P" ~ "TP",
      .default = BSPretty
    ),
    Approach = "Response",
    Indicator = factor(Indicator, levels = c("TN", "TP", "Chl-a", "AFDM", "% cover")),
    Threshold = case_when(
      Indicator == "TN" & Biostim == max_TN ~ NA_real_,
      Indicator == "TP" & Biostim == max_TP ~ NA_real_,
      Indicator == "Chl-a" & Biostim == max_chl ~ NA_real_,
      Indicator == "AFDM" & Biostim == max_afdm ~ NA_real_,
      Indicator == "% cover" & Biostim == max_cov ~ NA_real_,
      .default = Biostim
    ),
    Response_model_detail = glue::glue(
      "Additive model, 50% probability of attaining {Response_model_index} score above {Response_model_goal}."
    ),
    Details = Response_model_detail,
    Flag = case_when(
      Flag == "Biointegrity goal is higher than reference." ~ "Eutrophication threshold is set for a biointegrity goal that is above reference.",
      Flag == "" & is.na(Threshold) ~ "No threshold identified",
      .default = Flag
    )
  ) |>
  arrange(Indicator, Approach, Class, Response_model_index, Stringency) |>
  select(
    Class, Class_fullname, Approach, Response_model_form, Response_model_index, 
    Response_model_goal, Response_model_detail, Details, Stringency, `Indicator type`, 
    Indicator, Threshold, Flag
  ) 

mod_channel_thresholds <- bind_rows(
  bi_ref_best,
  eu_ref_best,
  mazor2022_thresholds,
  scam_thresholds
) |>
  mutate(
    # rounding for clarity
    Threshold = case_when(
      `Indicator type` == "Biointegrity" ~ round(Threshold, 2),
      Indicator == "% cover" ~ round(Threshold),
      Indicator %in% c("AFDM", "Chl-a") ~ round(Threshold, 1),
      Indicator %in% c("TN", "TP") ~ round(Threshold, 3),
      .default = Threshold
    )
  )

write.csv(
  mod_channel_thresholds |> rename(Indicator_Type = `Indicator type`, Threshold_value = Threshold), 
  "tables/Part_3_Table_S4_for_plots.csv", 
  na = "",
  row.names = FALSE
)
