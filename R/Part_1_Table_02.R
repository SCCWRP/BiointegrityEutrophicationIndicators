library(dplyr)

table_2_test_data <- readr::read_csv('data-raw/Part_1_test_data_gis.csv') |> 
  select(masterid, RefStatus, StudyArea) |>
  unique() |>
  group_by(StudyArea, RefStatus) |>
  tally() |>
  tidyr::pivot_wider(names_from = RefStatus, values_from = n) |>
  mutate(`Data set` = "Test") |>
  select(`Data set`, StudyArea, `Non-reference`, Reference) |>
  rowwise() |>
  mutate(Total = Reference + `Non-reference`) |>
  ungroup()

write.csv(table_2_test_data, file = 'tables/Part_1_Table_02_test_data.csv', row.names = F)
