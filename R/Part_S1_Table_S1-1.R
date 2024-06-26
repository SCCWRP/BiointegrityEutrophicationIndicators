library(dplyr)
library(ggplot2)
library(sf)

rb_sf <- st_read("data-raw/Part_1_shapefiles/rwqcbnda.shp")

test_data_gis_sf <- readr::read_csv("data-raw/Part_1_test_data_gis.csv") |>
  mutate(logwsa = log10(area_sqkm)) |>
  st_as_sf(coords = c("new_long", "new_lat"), crs = 4326)

table_S1_1 <- test_data_gis_sf |>
  st_transform(crs = st_crs(rb_sf)) |>
  st_join(rb_sf) |>
  st_drop_geometry() |>
  select(masterid, StudyArea, RBNAME) |>
  distinct() |>
  group_by(RBNAME, StudyArea) |>
  tally(name = "# sites") |>
  rename(`Regional board` = RBNAME, `Study area` = StudyArea)

write.csv(table_S1_1, "tables/Part_S1_Table_S1-1.csv", row.names = FALSE)