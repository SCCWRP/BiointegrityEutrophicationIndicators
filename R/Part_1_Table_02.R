Part_1_Table_2_test_data <- readr::read_csv('data-raw/Part_1_test_data_gis.csv') |> 
  dplyr::select(masterid, RefStatus, StudyArea) |>
  unique() |>
  dplyr::group_by(StudyArea, RefStatus) |>
  dplyr::tally() |>
  tidyr::pivot_wider(names_from = RefStatus, values_from = n) |>
  dplyr::mutate(`Data set` = "Test") |>
  dplyr::select(`Data set`, StudyArea, `Non-reference`, Reference) |>
  dplyr::rowwise() |>
  dplyr::mutate(Total = Reference + `Non-reference`) |>
  dplyr::ungroup()

write.csv(Part_1_Table_2_test_data, file = 'tables/Part_1_Table_02_test_data.csv', row.names = F)
