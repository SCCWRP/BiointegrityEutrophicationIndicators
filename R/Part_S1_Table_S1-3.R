library(dplyr)
library(ggplot2)
library(sf)

psa_sf <- st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp") |>
  mutate(
    PSA6 = factor(
      x = PSA6, 
      levels = c(
        "Chaparral", "Central Valley", "South Coast", 
        "Deserts Modoc", "North Coast", "Sierra Nevada"
      )
    )
  )

chan_sf <- readr::read_csv("data-raw/Part_2_combined_df_withdata.csv") |>
  select(masterid, class_do, Watershed, CSCI, ASCI_H, ASCI_D, latitude, longitude) |>
  mutate(
    class = case_when(
      !is.na(Watershed) ~ "Constructed channel",
      .default = class_do
    )
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

table_S1_3 <- chan_sf |>  
  filter(!is.na(class_do)) |>
  st_transform(crs = st_crs(psa_sf)) |>
  st_join(psa_sf) |>
  st_drop_geometry() |>
  select(masterid, class_do, PSA6) |>
  distinct() |>
  # one site gets incorrectly joined results in NA value for PSA6, 
  # when it should be Sierra Nevada
  mutate(PSA6 = tidyr::replace_na(PSA6, "Sierra Nevada")) |> 
  group_by(PSA6, class_do) |>
  tally(name = "# sites") |>
  rename(Ecoregion = PSA6, Class = class_do)

write.csv(table_S1_3, "tables/Part_S1_Table_S1-3.csv", row.names = FALSE)