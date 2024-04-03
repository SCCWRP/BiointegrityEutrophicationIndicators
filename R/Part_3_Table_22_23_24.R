library(dplyr)

# install CSCI package if needed
# devtools::install_github('SCCWRP/CSCI')

## Traditional reference
csci_df <- CSCI::refsamples
asci_df <- readr::read_csv("data-raw/Part_1_asci_caldata.csv") |>
  rename_with(.fn = tolower) |>
  select(-...1)


## Intermittent reference
nonperen_df <- readr::read_csv("data-raw/Part_1_Andy_Nonperen_data_ASCI.csv") |>
  mutate(
    masterid = StationCode,
    sampledate = lubridate::mdy(SampleDate),
    Region = if_else(RB %in% c(4, 7, 8, 9), "Southern", "Northern")
  )

## Valley floor
test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  mutate(logwsa = log10(area_sqkm)) |>
  group_by(masterid) |>
  slice_head(n = 1) |>
  ungroup() |>
  rename_with(.fn = tolower)

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv") |>
  rename_with(.fn = tolower) |>
  inner_join(test_data_gis |> select(masterid, studyarea))

## Constructed streams and modified classes
chan_df <- readr::read_csv("data-raw/Part_2_combined_df_withdata.csv") |>
  select(masterid, class_do, Watershed, CSCI, ASCI_H, ASCI_D)

mydf <-
  ##Ref sites
  #CSCI
  csci_df |>
  transmute(
    masterid = StationCode, 
    CSCI,
    Type = "Reference"
  ) |>
  #ASCI
  full_join(
    asci_df |>
      transmute(masterid = stationcode, ASCI_D = d_asci, ASCI_H = h_asci, Type = "Reference")
  ) |>
  #Intermittent
  bind_rows(
    nonperen_df |> 
      filter(Flow_SOP == "RFI", Region == "Northern") |>
      transmute(masterid = StationCode, CSCI, ASCI_D, ASCI_H, Type = "Intermittent_NorCal"),
    nonperen_df |> 
      filter(Flow_SOP == "RFI", Region == "Southern") |>
      transmute(masterid = StationCode, CSCI, ASCI_D, ASCI_H, Type = "Intermittent_SoCal"),
    #General
    test_data_scores |> 
      inner_join(
        test_data_gis |> 
          select(masterid, studyarea) |>
          filter(studyarea == "Central Valley") |>
          group_by(masterid) |>
          slice_head(n=1) |>
          ungroup()
      ) |>
      transmute(masterid, CSCI = csci, ASCI_D = asci_d, ASCI_H = asci_h, Type = "CVFloor"),
    #Ambiguous 
    chan_df |>
      filter(Watershed == "Ambiguous") |>
      select(-class_do, -Watershed) |>
      mutate(Type = "Ambiguous"),
    #Modified
    chan_df |>
      filter(!is.na(class_do)) |>
      filter(class_do != "Natural") |>
      transmute(masterid, CSCI, ASCI_H, ASCI_D, Type = class_do),
    #Modified AND CVF 
    chan_df |>
      filter(
        !is.na(class_do),
        class_do!="Natural",
        masterid %in% test_data_gis$masterid[test_data_gis$studyarea == "Central Valley"]
      ) |>
      transmute(masterid, CSCI, ASCI_H, ASCI_D, Type = paste0(class_do, "_CVF")),
    chan_df |>
      filter(Watershed == "Ambiguous") |>
      mutate(RB = stringr::str_sub(masterid, 1, 1)) |>
      filter(RB == 5) |>
      transmute(masterid, CSCI, ASCI_H, ASCI_D, Type = ("CC_CVF"))
  ) |>
  #Reformat
  tidyr::pivot_longer(
    cols = c("CSCI","ASCI_D","ASCI_H"), values_drop_na = T, 
    names_to = "Index", values_to = "Score"
  ) |>
  mutate(
    Type2 = if_else(
      Type %in% c("Reference", "Intermittent_SoCal", "Intermittent_NorCal"),
      "Reference",
      "Best observed"
    ),
    Type = factor(
      Type,
      levels = c(
        "Ambiguous", "CC_CVF", "CVFloor", "Hard bottom", "Hard bottom_CVF", 
        "Intermittent_NorCal", "Intermittent_SoCal", "Reference", 
        "Soft bottom-0 hard sides", "Soft bottom-0 hard sides_CVF", 
        "Soft bottom-1 hard side", "Soft bottom-2 hard sides"
      ),
      labels = c(
        "CC", "CC_CVF","CVF", "HB", "HB_CVF",
        "RFI-N","RFI-S", "Reference",
        "SB0","SB0_CVF", "SB1","SB2"
      )
    ),
    Type = factor(
      Type, 
      levels = c(
        "Reference", "RFI-N", "RFI-S", "CVF", "SB0", "SB0_CVF", 
        "SB1", "SB2", "HB", "HB_CVF", "CC", "CC_CVF"
      )
    )
  ) |>
  arrange(Type2, Type, Index, Score) 


## Table 22 ####

bi_thresholds_table <- mydf |>
  group_by(masterid, Type2, Type, Index) |>
  summarize(value = mean(Score, na.rm = T)) |>
  ungroup() |>
  group_by(Type2, Type, Index) |>
  summarize(
    n_sites = sum(!is.na(value)),
    qe01 = quantile(value, probs = c(0.01), na.rm = T),
    qe10 = quantile(value, probs = c(0.1), na.rm = T),
    qe30 = quantile(value, probs = c(0.3), na.rm = T),
    qe70 = quantile(value, probs = c(0.7), na.rm = T),
    qe90 = quantile(value, probs = c(0.9), na.rm = T),
    qe99 = quantile(value, probs = c(0.99), na.rm = T),
    MeanScore = mean(value),
    SDScore = sd(value, na.rm = T),
    qn01 = qnorm(p = 0.01, mean = MeanScore, sd = SDScore, lower.tail = T),
    qn10 = qnorm(p = 0.10, mean = MeanScore, sd = SDScore, lower.tail = T),
    qn30 = qnorm(p = 0.30, mean = MeanScore, sd = SDScore, lower.tail = T),
    qn70 = qnorm(p = 0.70, mean = MeanScore, sd = SDScore, lower.tail = T),
    qn90 = qnorm(p = 0.90, mean = MeanScore, sd = SDScore, lower.tail = T),
    qn99 = qnorm(p = 0.99, mean = MeanScore, sd = SDScore, lower.tail = T)
  ) |>
  ungroup() |>
  arrange(Type)  |>
  transmute(
    `Threshold type` = Type2,
    Population = Type, 
    Index = Index, 
    StandardUsageSupported = case_when(
      Type == "Reference" ~ "Yes",
      Type == "RFI-N" & Index %in% c("ASCI_D", "ASCI_H") ~ "Yes",
      Type == "RFI-N" & Index %in% c("CSCI") ~ "No",
      Type == "RFI-S" ~ "Yes",
      Type == "CVF" & Index %in% c("ASCI_D", "ASCI_H") ~ "Yes",
      Type == "CVF" & Index %in% c("CSCI") ~ "No",
      Type == "SB0" & Index %in% c("ASCI_D", "ASCI_H") ~ "No",
      Type == "SB0" & Index %in% c("CSCI") ~ "Yes",
      Type == "SB1" ~ "Yes",
      Type == "SB2" & Index %in% c("ASCI_D", "ASCI_H") ~ "No",
      Type == "SB2" & Index %in% c("CSCI") ~ "Yes",
      Type == "HB" & Index %in% c("ASCI_D", "ASCI_H") ~ "Yes",
      Type == "HB" & Index %in% c("CSCI") ~ "No",
      Type == "CC" & Index %in% c("ASCI_D", "ASCI_H") ~ "ND",
      Type == "CC" & Index %in% c("CSCI") ~ "No",
      .default = "ND"
    ),
    n_sites = n_sites,
    High = case_when(
      Type2 == "Reference" & Type %in% c("RFI-N", "RFI-S") ~ qn30,
      Type2 == "Reference" & Type %in% c("Reference") & Index == "CSCI" ~ 0.92,
      Type2 == "Reference" & Type %in% c("Reference") & Index %in% c("ASCI_D", "ASCI_H") ~ 0.95,
      Type2 == "Best observed" ~ qe99,
      .default = -999
    ),
    Intermediate = case_when(
      Type2 == "Reference" & Type %in% c("RFI-N","RFI-S") ~ qn10,
      Type2 == "Reference" & Type %in% c("Reference") & Index == "CSCI" ~ 0.79,
      Type2 == "Reference" & Type %in% c("Reference") & Index %in% c("ASCI_D", "ASCI_H") ~ 0.86,
      Type2 == "Best observed" ~ qe90,
      .default = -999
    ),
    Low = case_when(
      Type2 == "Reference" & Type %in% c("RFI-N", "RFI-S") ~ qn01,
      Type2 == "Reference" & Type %in% c("Reference") & Index == "CSCI" ~ 0.63,
      Type2 == "Reference" & Type %in% c("Reference") & Index %in% c("ASCI_D", "ASCI_H") ~ 0.75,
      Type2 == "Best observed" ~ qe70,
      .default = -999
    )
  )

#Numbers from Theroux et al. 2022 or Mazor et al. 2016
bi_thresholds_table$n_sites[bi_thresholds_table$Population == "Reference"] <- c(418, 418, 473) 

write.csv(bi_thresholds_table, "tables/Part_3_Table_22.csv", row.names = F)

## Table 23 ####
all_data_chem_phab <- readr::read_csv("data-raw/Part_3_all_data_chem_phab.csv")
mydf_bs <- mydf |>
  select(masterid, Type, Type2) |>
  unique() |>
  left_join(
    all_data_chem_phab |>
      select(masterid, sampledate, TN, TP, AFDM_mg_m2, `Chl-a_ug_cm2`, PCT_MAP),
    relationship = "many-to-many"
  ) |>
  tidyr::pivot_longer(cols = c(TN, TP, AFDM_mg_m2, `Chl-a_ug_cm2`, PCT_MAP), values_drop_na = T) |>
  group_by(masterid, Type, Type2, name) |>
  summarize(value = mean(value)) |>
  ungroup() |>
  group_by(Type2, Type, name) |>
  summarize(
    n_sites = sum(!is.na(value)),
    qe01 = quantile(value, probs = c(0.01), na.rm = T),
    qe10 = quantile(value, probs = c(0.1), na.rm = T),
    qe30 = quantile(value, probs = c(0.3), na.rm = T),
    qe70 = quantile(value, probs = c(0.7), na.rm = T),
    qe90 = quantile(value, probs = c(0.9), na.rm = T),
    qe99 = quantile(value, probs = c(0.99), na.rm = T)
  ) |>
  ungroup() |>
  mutate(
    name = factor(name, levels = c("TN", "TP", "Chl-a_ug_cm2", "AFDM_mg_m2", "PCT_MAP"))
  ) |>
  arrange(name, desc(Type2))

table_23 <- mydf_bs |>
  transmute(
    `Threshold type` = Type2,
    Population = Type,
    Indicator = case_when(
      name == "AFDM_mg_m2" ~ "AFDM (mg/m2)",
      name == "Chl-a_ug_cm2" ~ "Chl-a (ug/cm2)",
      name == "PCT_MAP" ~ "% cover",
      name == "TN" ~ "TN (mg/L)",
      name == "TP" ~ "TP (mg/L)",
      .default = name
    ),
    Indicator = factor(Indicator, levels = c("TN (mg/L)", "TP (mg/L)", "Chl-a (ug/cm2)", "AFDM (mg/m2)", "% cover")),
    n = n_sites,
    High = case_when(
      Type2 == "Reference" ~ qe70,
      Type2 == "Best observed" ~ qe01,
      .default = -999
    ),
    Intermediate = case_when(
      Type2 == "Reference" ~ qe90,
      Type2 == "Best observed" ~ qe10,
      .default = -999
    ),
    Low = case_when(
      Type2 == "Reference" ~ qe99,
      Type2 == "Best observed" ~ qe30,
      .default = -999
    )
  ) |>
  select(Indicator, `Threshold type`, `Stream class` = Population, n, High, Intermediate, Low) |>
  arrange(Indicator, desc(`Threshold type`)) 

write.csv(table_23, "tables/Part_3_Table_23.csv", row.names = F)

## Table 24 ####

# results from Mazor et al. (2022)
mazor2022_thresholds <- readr::read_csv("data-raw/Part_3_model.summary_thresholds_woInorganics.csv") |>
  filter(BIgoal %in% c("Ref01", "Ref10", "Ref30"), Stratum == "California") |>
  transmute(
    Class = "Wadeable streams",
    Index = Response,
    BSPretty,
    Stringency = case_when(
      BIgoal == "Ref30" ~ "High",
      BIgoal=="Ref10"~"Intermediate",
      BIgoal=="Ref01"~"Low",
      .default = "x"
    ),
    Threshold = p80
  ) |>
  tidyr::pivot_wider(names_from = BSPretty, values_from = Threshold) |>
  filter(Stringency == 'Intermediate') |>
  transmute(
    Population = Class, Index, StandardUsageSupported = "Yes", 
    value = c(0.86, 0.86, 0.79), AFDM, `Chl-a`, `Total N`, `% cover`, `Total P`
  )

mydf.c <- readr::read_csv("data-raw/Part_3_mydf.c.csv") |>
  mutate(PSA2 = if_else(PSA6c %in% c("NC", "SN"), "Wet", "Arid"))

asci.df.c <- readr::read_csv("data-raw/Part_3_asci.df.c.csv") |>
  mutate(PSA2 = if_else(PSA6c %in% c("NC", "SN"), "Wet", "Arid"))

#Create vectors of modeling variables
#Biostiulatory variables
chem.varz <- c("Nitrogen_Total_mgPerL", "Phosphorus_as_P_mgPerL")
om.varz <- c("Chlorophyll_a_mgPerm2", "Ash_Free_Dry_Mass_mgPercm2", "PCT_MAP")
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
my.newdfs <- data.frame(
    Nitrogen_Total_mgPerL = seq(from = 0, to = 3, length.out = length.out.x),
    Phosphorus_as_P_mgPerL = seq(from = 0, to = 1.5, length.out = length.out.x),
    Chlorophyll_a_mgPerm2 = seq(from = 0, to = 300, length.out = length.out.x),
    Ash_Free_Dry_Mass_mgPercm2 = seq(from = 0, to = 40, length.out = length.out.x),
    PCT_MAP = seq(from = 0, to = 100, length.out = length.out.x)
  ) |>
  tidyr::pivot_longer(cols = everything(), names_to = "BiostimVar", values_to = "Biostim") |>
  tidyr::nest(.by = "BiostimVar")

model.summary <- mod.dat |>
  filter(DevSet == "Cal", SelectedSample == "Selected") |>
  tidyr::pivot_longer(cols = Nitrogen_Total_mgPerL:PCT_MAP, names_to = "BiostimVar", values_to = "Biostim") |>
  group_by(Index, BiostimVar) |>
  mutate(
    model = list(scam::scam(IndexScore ~ s(Biostim, bs = "mpd"))),
    performance = purrr::map(model, function(mod) {
      list(AIC = mod$aic, gcv.ubre = mod$gcv.ubre, dgcv.ubre = mod$dgcv.ubre)
    }),
    AIC_null = scam::scam(IndexScore ~ 1)$aic,
    summary = purrr::map(model, function(mod) {
      summ <- summary(mod)
      list(rsq = summ$r.sq, biostim_F = summ$s.table[3], biostim_p = summ$s.table[4], n = summ$n)
    }),
  ) |>
  tidyr::unnest_wider(c(performance, summary))

my.models.predictions <- model.summary |>
  inner_join(my.newdfs) |>
  select(model, data) |>
  distinct() |>
  mutate(
    Fit = list(predict(first(model), newdata = first(data), type = "response", se = T)$fit),
    SE = list(predict(first(model), newdata = first(data), type = "response", se = T)$se.fit)
  ) |>
  tidyr::unnest(cols = c(data, Fit, SE))

table_24 <- bi_thresholds_table |>
  filter(!(Population %in% c("SB0_CVF", "HB_CVF", "CC_CVF"))) |>
  select(Index, Population, StandardUsageSupported, High, Intermediate, Low) |>
  tidyr::pivot_longer(cols = c(High, Intermediate, Low)) |>
  unique() |>
  mutate(value = round(value, 2)) |>
  tidyr::crossing(BiostimVar = bs.varz) |> 
  left_join(my.models.predictions, by = join_by(Index == Index, BiostimVar == BiostimVar, closest(value <= Fit))) |>
  filter(name == "Intermediate") |>
  select(-c(Fit, SE)) |>
  inner_join(bs.varz.df) |>
  tidyr::pivot_wider(
    names_from = BSPretty, values_from = Biostim, 
    id_cols = c(Population, Index, StandardUsageSupported, value)
  ) |>
  bind_rows(mazor2022_thresholds) |>
  mutate(
    Population = factor(
      Population,
      levels = c("Wadeable streams", "Reference", "CVF", "SB0", "SB1", "SB2", "HB", "CC"),
      labels = c(
        "Wadeable streams (logistic regression, Mazor et al. 2022)", 
        "Wadeable streams (SCAM, present study)", "CVF", "SB0", "SB1", "SB2", "HB", "CC"
      )
    )
  ) |>
  arrange(Population, Index) |>
  select(
    `Stream class` = Population, Index, StandardUsageSupported, `Biointegrity goal` = value, 
    `Total N`, `Total P`, `Chl-a`, AFDM, `% cover`
  )

write.csv(table_24, "tables/Part_3_Table_24.csv", row.names = F)
