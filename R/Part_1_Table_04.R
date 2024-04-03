library(dplyr)

test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  group_by(masterid) |>
  slice_head(n = 1) |>
  ungroup()

test_data_scores <- readr::read_csv("data-raw/Part_1_test_data_scores.csv")

table4 <- test_data_scores |>
  left_join(test_data_gis |> select(masterid, StudyArea)) |>
  select(masterid, StudyArea, CSCI, ASCI_D, ASCI_H) |>
  tidyr::pivot_longer(cols = c(CSCI, ASCI_D, ASCI_H), names_to = "Index") |>
  na.omit() |>
  group_by(masterid, StudyArea, Index) |>
  summarize(
    index_n = length(value),
    index_mean = mean(value),
    index_max = max(value)
  ) |>
  ungroup() |>
  mutate(
    Class = case_when(
      Index == "CSCI" & index_max >= 0.92 ~ "Class1",
      Index == "CSCI" & index_max >= 0.79 ~ "Class2",
      Index == "CSCI" & index_max >= 0.63 ~ "Class3",
      Index == "CSCI" & index_max < 0.63 ~ "Class4",
      Index %in% c("ASCI_D", "ASCI_H") & index_max >= 0.94 ~ "Class1",
      Index %in% c("ASCI_D", "ASCI_H") & index_max >= 0.86 ~ "Class2",
      Index %in% c("ASCI_D", "ASCI_H") & index_max >= 0.75 ~ "Class3",
      Index %in% c("ASCI_D", "ASCI_H") & index_max < 0.75 ~ "Class4"
    )
  ) |>
  group_by(StudyArea, Index, Class) |>
  tally() |>
  tidyr::pivot_wider(names_from = Class, values_from = n, values_fill = 0) |>
  rename(`>=30th` = Class1, `>=10th` = Class2, `>=1st` = Class3, `<1st` = Class4) 

write.csv(table4, file = 'tables/Part_1_Table_04.csv', row.names = F)
