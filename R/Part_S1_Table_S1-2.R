library(dplyr)
library(ggplot2)
library(sf)

psa_sf <- sf::st_read("data-raw/Part_1_shapefiles/PSA6_2011.shp") |>
  mutate(
    PSA6 = factor(
      x = PSA6, 
      levels = c(
        "Chaparral", "Central Valley", "South Coast", 
        "Deserts Modoc", "North Coast", "Sierra Nevada"
      )
    )
  )

nonperen_sf <- readr::read_csv("data-raw/Part_S1_Andy_Nonperen_data_ASCI_CL_11_4_2022.csv") |>
  mutate(
    FlowStatus = case_when(
      Flow_SOP == "RFI_SFI" ~ "SFI",
      Flow_SOP == "RI" ~ "RFI",
      .default = Flow_SOP
    ),
    FlowStatus_label = case_when(
      FlowStatus == "P" ~ "Perennial",
      FlowStatus == "RFI" ~ "Regularly flowing intermittent",
      FlowStatus == "SFI" ~ "Seldom flowing intermittent",
      .default = "Other"
    )
  ) |>
  st_as_sf(coords = c("New_Long", "New_Lat"), crs = 4326)

table_S1_2 <- nonperen_sf |>
  st_transform(crs = st_crs(psa_sf)) |>
  st_join(psa_sf) |>
  st_drop_geometry() |>
  select(masterid, FlowStatus_label, PSA6) |>
  distinct() |>
  group_by(PSA6, FlowStatus_label) |>
  tally(name = "# reference sites") |>
  rename(Ecoregion = PSA6, `Flow status` = FlowStatus_label)

write.csv(table_S1_2, "tables/Part_S1_Table_S1-2.csv", row.names = FALSE)
