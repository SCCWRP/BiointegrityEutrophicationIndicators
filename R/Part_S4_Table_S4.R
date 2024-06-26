library(dplyr)
# depends on Part_3_Table_S4.R

mod_channel_thresholds <- readr::read_csv('tables/Part_3_Table_S4_for_plots.csv')

supplement_S4 <- mod_channel_thresholds |>
  mutate(Flag = tidyr::replace_na(Flag, "")) |>
  rename(`Indicator type` = Indicator_Type, Threshold = Threshold_value) |>
  select(-c(Class_fullname, Response_model_form, Response_model_index, Response_model_goal, Response_model_detail))

write.csv(supplement_S4, "tables/Part_S4_Table_S4.csv", row.names = FALSE)
