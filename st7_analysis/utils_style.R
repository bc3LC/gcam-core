scen_palette_studyReference = c(
  'St7_Reference_original' = '#8100D5',
  'St7_Reference_R-M-F' = '#25CA11'
)

scen_palette_calibrateSppFuelPrefElast = c(
  'St7_Reference_original' = '#8100D5',
  'St7_Reference_R-M-F' = '#25CA11'
)


rename_scen = function(data) {

  data = data %>%
    mutate(scenario = ifelse(stringr::str_detect(scenario, ",date"),
                             stringr::str_extract(scenario, "^[^,]+(?=,date)"),
                             scenario))

  return(invisible(data))
}
