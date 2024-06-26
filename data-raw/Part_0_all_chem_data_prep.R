library(dplyr)

# for informational purposes only

all_data_phab2 <- DBI::dbGetQuery(con, 
  "
  SELECT DISTINCT
  	sde.lu_stations.masterid,
  	sde.analysis_phabmetrics.stationcode,
  	sde.analysis_phabmetrics.sampledate,
  	sde.analysis_phabmetrics.variable,
  	sde.analysis_phabmetrics.result 
  FROM
  	sde.lu_stations
  	INNER JOIN sde.tblgismetrics_archived ON sde.tblgismetrics_archived.masterid = sde.lu_stations.masterid
  	INNER JOIN sde.analysis_phabmetrics ON sde.analysis_phabmetrics.stationcode = sde.lu_stations.stationid 
  WHERE
    sde.analysis_phabmetrics.VARIABLE IN ('PCT_SAFN','XFC_NAT_SWAMP','PCT_FAST','XCMG','PCT_MAP','XSLOPE','PCT_FN','PCT_HP') AND
    sde.analysis_phabmetrics.sampledate < '2022-07-21'
  "
)  |>
  group_by(masterid, sampledate, variable) |>
  slice_head(n = 1) |> #We have some replicates within analyte that need to be cleared
  ungroup() |>
  tidyr::pivot_wider(names_from = variable, values_from = result)


# 'XWDO', 'XWSC','XWPH','XWTC' <- if you want wq variables out of phab metrics

all_data_spcond <- DBI::dbGetQuery(con, 
  "
  SELECT
    sde.analysis_combined_specificconductivity.masterid,
    sde.analysis_combined_specificconductivity.sampledate,
    sde.analysis_combined_specificconductivity.fieldreplicate,
    sde.analysis_combined_specificconductivity.sampletypecode,
    sde.analysis_combined_specificconductivity.labreplicate,
    sde.analysis_combined_specificconductivity.analytename,
    sde.analysis_combined_specificconductivity.result,
    sde.analysis_combined_specificconductivity.unit
  FROM
    sde.analysis_combined_specificconductivity
  WHERE
    sde.analysis_combined_specificconductivity.sampledate < '2022-07-21'
  "
) |>
  mutate(analytename = analytename |> stringr::str_replace_all(" ", "_")) |>
  mutate(analytename = analytename |> stringr::str_replace_all(",", "")) |>
  filter(result != -88) |>
  filter(!sampletypecode %in% c("MS1", "MS2", "MSBLDup")) |>
  group_by(masterid, sampledate, analytename, unit)  |>
  summarise(result = mean(result, na.rm = T)) |>
  ungroup() |>
  select(-unit) |>
  tidyr::pivot_wider(names_from = analytename, values_from = result)


all_data_nutrients <- DBI::dbGetQuery(con, 
  "
  SELECT DISTINCT
    sde.analysis_chem_nutrients_0.masterid,
    sde.analysis_chem_nutrients_0.sampledate,
    sde.analysis_chem_nutrients_0.fieldreplicate,
    sde.analysis_chem_nutrients_0.labreplicate,
    sde.analysis_chem_nutrients_0.matrixname,
    sde.analysis_chem_nutrients_0.sampletypecode,
    sde.analysis_chem_nutrients_0.total_n_mgl,
    sde.analysis_chem_nutrients_0.total_p_mgl
  FROM
    sde.analysis_chem_nutrients_0
  WHERE
    sde.analysis_chem_nutrients_0.sampledate < '2022-07-21'
  "
) |>
  group_by(masterid, sampledate) |>
  summarise(TN = mean(total_n_mgl, na.rm = T),
            TP = mean(total_p_mgl, na.rm = T)) |>
  ungroup()

all_data_alg <- DBI::dbGetQuery(con, 
  "
  SELECT DISTINCT
  	sde.unified_chemistry.analytename,
  	sde.lu_stations.masterid,
  	sde.unified_chemistry.sampledate,
  	sde.unified_chemistry.sampletypecode,
  	sde.unified_chemistry.matrixname,
  	sde.unified_chemistry.fieldreplicate,
  	sde.unified_chemistry.labreplicate,
  	sde.unified_chemistry.unit,
  	sde.unified_chemistry.result
  FROM
  	sde.unified_chemistry
  	INNER JOIN sde.lu_stations ON sde.lu_stations.stationid = sde.unified_chemistry.stationcode 
  WHERE
  	sde.unified_chemistry.analytename IN ('Ash Free Dry Mass', 'Chlorophyll a' ,'AFDM_Algae, Particulate', 'Chlorophyll a, Particulate') AND
  	sde.unified_chemistry.sampledate < '2022-07-21'
  "
) |>
  filter(!unit %in% c("mg/L", "mg/cm3", "mg/m3", "ug/L")) |>
  filter(!matrixname %in% c("blankmatrix", "blankwater")) |>
  mutate(
    analytename = case_when(
      analytename == "AFDM_Algae, Particulate" ~ "Ash Free Dry Mass",
      analytename == "Chlorophyll a, Particulate" ~ "Chlorophyll a",
      .default = analytename
    )
  ) |>
  mutate(result = as.numeric(result)) |>
  mutate(
    unit = stringr::str_replace_all(unit, "²", "2"),
    result = case_when(
      result < 0 ~ 0, 
      analytename == "Ash Free Dry Mass" & unit == "g/m2" ~ result,
      analytename == "Ash Free Dry Mass" & unit == "mg/cm2" ~ result * 10,
      analytename == "Chlorophyll a" & unit == "mg/m2" ~ result,
      analytename == "Chlorophyll a" & unit == "ug/cm2" ~ result * 10,
      .default = NA_real_
    ),
    analytename = case_when(
      analytename == "Ash Free Dry Mass" ~ "AFDM_g_m2",
      analytename == "Chlorophyll a" ~ "Chl-a_mg_m2",
      .default = "Other"
    )
  ) |>
  group_by(masterid, sampledate, analytename) |>
  summarise(result = mean(result, na.rm = T)) |>
  ungroup() |>
  tidyr::pivot_wider(names_from = analytename, values_from = result)

lu_masterid <- readr::read_csv("data-raw/Part_1_lu_masterid.csv")

all_data_chem_phab <- all_data_spcond |>
  full_join(all_data_nutrients) |>
  full_join(all_data_alg) |>
  full_join(all_data_phab2 |> select(-stationcode)) |>
  filter(masterid != "-88", masterid %in% lu_masterid$masterid)


test_data_gis <- readr::read_csv("data-raw/Part_1_test_data_gis.csv")

test_data_chem_phab <- all_data_chem_phab |> 
  filter(masterid %in% test_data_gis$masterid)

readr::write_csv(test_data_chem_phab, "data-raw/Part_0_test_data_chem_phab.csv")
readr::write_csv(all_data_chem_phab, "data-raw/Part_0_all_data_chem_phab.csv")


all_data_slope_fines <- all_data_chem_phab |>
  select(masterid, sampledate, XSLOPE, PCT_FN, PCT_HP) |>
  filter(!is.na(XSLOPE) | !is.na(PCT_FN) | !is.na(PCT_HP))

readr::write_csv(all_data_slope_fines, "data-raw/Part_0_all_data_slope_fines.csv")
