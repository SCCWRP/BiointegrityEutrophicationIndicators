library(dplyr)
library(ggplot2)

# install CSCI package if needed
# devtools::install_github('SCCWRP/CSCI')

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

# source: https://knb.ecoinformatics.org/view/urn:uuid:75411f50-32ed-42a5-bbfd-26833c7a441f
scape_df <- readr::read_csv('data-raw/Part_3_strm_constraints.csv')

csci_df <- CSCI::refsamples
asci_df <- readr::read_csv("data-raw/Part_1_asci_caldata.csv") |>
  rename_with(.fn = tolower)

lustations_df <- readr::read_csv('data-raw/Part_3_lu_stations.csv')

####Intermittent reference####
nonperen_df <- readr::read_csv("data-raw/Part_1_Andy_Nonperen_data_ASCI.csv") |>
  mutate(
    masterid = StationCode,
    sampledate = lubridate::mdy(SampleDate),
    Region = if_else(RB %in% c(4, 7, 8, 9), "Southern", "Northern")
  )

sample_exclusion <- readr::read_csv('data-raw/Part_1_sample_metadata_review_07152024.csv') |>
  filter(!Include) |>
  mutate(sampledate = lubridate::mdy(SampleDate))

nonperen_df <- nonperen_df |>
  anti_join(sample_exclusion, by = c("StationCode", "sampledate"))

test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  mutate(logwsa = log10(area_sqkm)) |>
  rename_with(.fn = tolower)

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv") |>
  rename_with(.fn = tolower)

test_data_scores <- test_data_scores |>
  inner_join(test_data_gis |> select(masterid, studyarea))

####Constructed streams and modified classes####
chan_df <- readr::read_csv("data-raw/Part_2_combined_df_withdata.csv") |>
  select(masterid, class_do, Watershed, CSCI, ASCI_H, ASCI_D)


assembled_df<-
  ##Ref sites
  #CSCI
  csci_df |>
    transmute(masterid = StationCode, CSCI, Type = "Reference") |>
    #ASCI
    full_join(asci_df |> transmute(masterid = stationcode, ASCI_D = d_asci, ASCI_H = h_asci, Type = "Reference")) |>
    bind_rows(
      #Intermittent
      nonperen_df |>
        filter(Flow_SOP == "RFI" & Region == "Northern") |>
        transmute(masterid = StationCode, CSCI, ASCI_D, ASCI_H, Type = "Intermittent_NorCal"),
      nonperen_df |>
        filter(Flow_SOP == "RFI" & Region == "Southern") |>
        transmute(masterid = StationCode, CSCI, ASCI_D, ASCI_H, Type = "Intermittent_SoCal"),
      #General
      test_data_scores |> 
        inner_join(
          test_data_gis |> 
            select(masterid, studyarea) |>
            filter(studyarea == "Central Valley")
        ) |>
        transmute(masterid, CSCI = csci, ASCI_D = asci_d, ASCI_H = asci_h, Type = "CVFloor"),
      #Ambiguous 
      chan_df |>
        filter(Watershed == "Ambiguous") |>
        select(-c(class_do, Watershed)) |>
        mutate(Type = "Ambiguous"),
      #Modified
      chan_df |>
        filter(!is.na(class_do)) |>
        filter(class_do != "Natural") |>
        transmute(masterid, CSCI, ASCI_H, ASCI_D, Type = class_do)
    ) |>
    #Reformat
    tidyr::pivot_longer(
      cols = c("CSCI", "ASCI_D", "ASCI_H"), values_drop_na = T, 
      names_to = "Index", values_to = "Score"
    ) |>
    mutate(
      Type2 = if_else(
        Type %in% c("Reference", "Intermittent_SoCal", "Intermittent_NorCal"), 
        "Reference", 
        "Best observed"
      )
    ) |>
    arrange(Type2, Type, Index, Score) |>
    left_join(
      lustations_df |>
        transmute(masterid, stationcode, COMID = as.integer(comid)) |>
        group_by(masterid) |>
        slice_head(n = 1) |>
        ungroup()
    )

scape_plot_dat <- assembled_df |>
  inner_join(scape_df) |>
  filter(Index == "CSCI", qt70 > 0) |>
  select(masterid, Type, Index, `Low Stringency` = qt70, `Intermediate Stringency` = qt90) |>
  tidyr::pivot_longer(cols = c(`Low Stringency`, `Intermediate Stringency`)) |>
  mutate(
    Type = factor(
      Type, 
      levels = c(
        "Ambiguous", "Soft bottom-0 hard sides", "Soft bottom-1 hard side",
        "Soft bottom-2 hard sides", "Hard bottom", "CVFloor", 
        "Intermittent_SoCal", "Intermittent_NorCal", "Reference"
      ),
      labels = c("CC","SB0","SB1","SB2","HB","CVF","RFI-S","RFI-N","Reference")
    )
  ) |>
  filter(name == "Intermediate Stringency", Type %in% c("CVF", "HB", "SB2", "SB1", "SB0", "CC"))



thresh_scape_overlay_dat <- mydf |>
  filter(Index == "CSCI") |>
  mutate(Type = if_else(Population == "Wadeable (standard)", "Reference", Population)) |>
  transmute(Type, `Low Stringency` = Thresh_Low, `Intermediate Stringency` = Thresh_Int) |>
  tidyr::pivot_longer(cols = c(`Low Stringency`, `Intermediate Stringency`)) |>
  filter(name == "Intermediate Stringency", Type %in% c("CVF", "HB", "SB2", "SB1", "SB0", "CC"))

stringency_lines <- tibble(
  name = c("Low Stringency", "Intermediate Stringency"),
  value = c(0.63, 0.79)
)

scape_comparison_plot_alt <- ggplot(data = scape_plot_dat, aes(x = Type, y = value)) +
  geom_boxplot(fill = "gray") +
  geom_point(
    data = thresh_scape_overlay_dat, 
    position = position_dodge(width = 0.75),
    shape = 22, size = 3, fill = "red"
  ) +
  geom_hline(data = stringency_lines, aes(yintercept = value), linetype = "dashed") +
  coord_flip() +
  theme_bw() +
  labs(x = "", y = "CSCI score")

ggsave(scape_comparison_plot_alt, filename = "figures/Part_3_Figure_52.jpg", dpi = 300, width = 6.5, height = 5)
