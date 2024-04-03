library(dplyr)
library(ggplot2)

# install CSCI package if needed
# devtools::install_github('SCCWRP/CSCI')

msid.lu <- readr::read_csv("data-raw/Part_3_lu_stations.csv") |>
  mutate(RB = stringr::str_sub(huc, 1, 1))

## Traditional reference ####
csci_df <- CSCI::refsamples
asci_df <- readr::read_csv("data-raw/Part_1_asci_caldata.csv") |>
  rename_with(.fn = tolower) |>
  select(-...1)


## Intermittent reference ####
nonperen_df <- readr::read_csv("data-raw/Part_1_Andy_Nonperen_data_ASCI.csv") |>
  mutate(
    masterid = StationCode,
    sampledate = lubridate::mdy(SampleDate),
    Region = if_else(RB %in% c(4, 7, 8, 9), "Southern", "Northern")
  )

## Valley floor ####
test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  mutate(logwsa = log10(area_sqkm)) |>
  group_by(masterid) |>
  slice_head(n = 1) |>
  ungroup() |>
  rename_with(.fn = tolower)

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv") |>
  rename_with(.fn = tolower) |>
  inner_join(test_data_gis |> select(masterid, studyarea))

## Constructed streams and modified classes ####
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
        transmute(masterid = StationCode, CSCI, ASCI_D, ASCI_H, Type = "Intermittent_NorCal")
    ) |>
    bind_rows(
      nonperen_df |> 
        filter(Flow_SOP == "RFI", Region == "Southern") |>
        transmute(masterid = StationCode, CSCI, ASCI_D, ASCI_H, Type = "Intermittent_SoCal")
    ) |>
    #General
    bind_rows(
      test_data_scores |> 
        inner_join(
          test_data_gis |> 
            select(masterid, studyarea) |>
            filter(studyarea == "Central Valley") |>
            group_by(masterid) |>
            slice_head(n=1) |>
            ungroup()
        ) |>
        transmute(masterid, CSCI = csci, ASCI_D = asci_d, ASCI_H = asci_h, Type = "CVFloor")
    ) |>
    #Ambiguous 
    bind_rows(
      chan_df |>
        filter(Watershed == "Ambiguous") |>
        select(-class_do, -Watershed) |>
        mutate(Type = "Ambiguous")
    ) |>
    #Modified
    bind_rows(
      chan_df |>
        filter(!is.na(class_do)) |>
        filter(class_do != "Natural") |>
        transmute(masterid, CSCI, ASCI_H, ASCI_D, Type = class_do)
    ) |>
    #Modified AND CVF 
    bind_rows(
      chan_df |>
        filter(
          !is.na(class_do),
          class_do!="Natural",
          masterid %in% test_data_gis$masterid[test_data_gis$studyarea == "Central Valley"]
        ) |>
        transmute(masterid, CSCI, ASCI_H, ASCI_D, Type = paste0(class_do, "_CVF")) 
    ) |>
    bind_rows(
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
          "Ambiguous", "CC_CVF", "CVFloor", 
          "Hard bottom", "Hard bottom_CVF", 
          "Intermittent_NorCal", "Intermittent_SoCal", 
          "Reference", 
          "Soft bottom-0 hard sides", "Soft bottom-0 hard sides_CVF", 
          "Soft bottom-1 hard side", "Soft bottom-2 hard sides"
        ),
        labels = c(
          "CC", "CC_CVF","CVF",
          "HB", "HB_CVF",
          "RFI-N","RFI-S",
          "Reference",
          "SB0","SB0_CVF",
          "SB1","SB2"
        )
      ),
      Type = factor(
        Type, 
        levels = c(
          "Reference", "RFI-N", "RFI-S",
          "CVF",
          "SB0", "SB0_CVF",
          "SB1", "SB2",
          "HB", "HB_CVF",
          "CC", "CC_CVF"
        )
      )
    ) |>
    arrange(Type2, Type, Index, Score) 
  

lines_df <- tidyr::crossing(
    Type2 = c("Reference","Best observed"),
    Level = c("High","Medium","Low")
  ) |>
  mutate(
    Ptile = case_when(
      Type2 == "Reference" & Level == "High" ~ 0.3,
      Type2 == "Reference" & Level == "Medium" ~ 0.1,
      Type2 == "Reference" & Level == "Low" ~ 0.01,
      Type2 == "Best observed" & Level == "High" ~ 0.99,
      Type2 == "Best observed" & Level == "Medium" ~ 0.9,
      Type2 == "Best observed" & Level == "Low" ~ 0.7
    )
  )

biointegrity_distributions_plot <- ggplot(mydf, aes(y = Score)) +
  stat_ecdf(aes(color = Type), geom = "path") +
  facet_grid(Index ~ Type2, drop = T) +
  geom_vline(data = lines_df, aes(xintercept = Ptile), linetype = "dashed") +
  scale_color_manual(
    name = "Stream class",
    values = c(
      "#481567ff", "#238a8dff", "#55c667ff",
      "darkgreen",
      "#fed976", "gold3", "#fd8d3c", "#f03b20",
      "#c51b8a", "purple", "gray", "black"
    )
  )+
  theme_bw() +
  labs(x = "Cumulative percent of sites")

ggsave(biointegrity_distributions_plot, filename = "figures/Part_3_Figure_43.jpg", dpi = 300, height = 7, width = 6.5)
