# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L354.FuelStandards
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L354.FuelStandards}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L354.FuelStandards <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_FuelStandards",
             "L254.StubTranTechCoef",
             "UCD_trn_data",
             FILE = "policy/mappings/UCD_addtl_subsector_mapping",
             FILE = "energy/mappings/UCD_ctry",
             FILE = "common/GCAM_region_names",
             FILE = "common/iso_GCAM_regID"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L354.FuelStandards"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ---------------
    A_FuelStandards <- get_data(all_data, "policy/A_FuelStandards")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    UCD_trn_data <- get_data(all_data, "UCD_trn_data")
    UCD_addtl_subsector_mapping <- get_data(all_data, "policy/mappings/UCD_addtl_subsector_mapping")
    UCD_ctry <- get_data(all_data, "energy/mappings/UCD_ctry")
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")

    # 1. Convert to long and interpolate ---------------
    L354.FuelStandards_long <- A_FuelStandards %>%
      gather_years(value_col = "coefficient") %>%
      # Interpolate between years if needed
      filter(!is.na(coefficient)) %>%
      group_by(region, supplysector, tranSubsector, stub.technology) %>%
      complete(nesting(region, supplysector, tranSubsector, stub.technology),
               year = seq(min(year), max(year), 5)) %>%
      # If group only has one, approx_fun doesn't work, so we use this workaround
      mutate(coefficient_NA = as.numeric(approx_fun(year, coefficient))) %>%
      ungroup %>%
      mutate(coefficient = if_else(!is.na(coefficient_NA), coefficient_NA, coefficient)) %>%
      select(-coefficient_NA) %>%
      mutate(market.name = region)


    # 2. Map coefficients to new classes ---------------
    # Need historical UCD data to weight old classes
    UCD_hist_data <- UCD_trn_data %>%
      filter(variable == "energy")

    region_mapping <- iso_GCAM_regID %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      left_join(UCD_ctry, by = c("iso", "country_name")) %>%
      na.omit() %>% # Not concerned about omitting some islands, old USSR countries etc
      group_by(region, UCD_region) %>%
      summarise(n = dplyr::n()) %>%
      # Keep the region mapping with the most countries included
      # Only exception is China
      filter(n == max(n)) %>%
      ungroup %>%
      mutate(UCD_region = if_else(region == "China", "China", UCD_region)) %>%
      select(-n)

    L354.FuelStandards_revised_classes <- L354.FuelStandards_long %>%
      filter(old_classes == 1) %>%
      left_join_error_no_match(region_mapping, by = "region") %>%
      # Any additional mapping needed
      left_join(UCD_addtl_subsector_mapping, by = c("region", "tranSubsector")) %>%
      mutate(tranSubsector = if_else(!is.na(mapped.size.class), mapped.size.class, tranSubsector)) %>%
      select(-mapped.size.class) %>%
      left_join(UCD_hist_data, by = c("UCD_region",
                                      "tranSubsector" = "size.class",
                                      "stub.technology" = "UCD_technology",
                                      "SSP_sce" = "sce")) %>%
      # Values of zero can cause issues, so set them to 0.001
      mutate(value = if_else(value == 0, 0.001, value)) %>%
      group_by(region, market.name, supplysector, rev_size.class, stub.technology, year.x, xml, SSP_sce, minicam.energy.input) %>%
      summarise(coefficient = weighted.mean(coefficient, value)) %>%
      ungroup %>%
      select(region, supplysector, tranSubsector = rev_size.class, stub.technology, year = year.x, xml, SSP_sce,
             minicam.energy.input, coefficient, market.name)

    L354.FuelStandards_new <- L354.FuelStandards_long %>%
      filter(old_classes == 0) %>%
      select(-old_classes) %>%
      bind_rows(L354.FuelStandards_revised_classes)

    # 3.Join in lowest GCAM coefficients and select the higher coefficient ---------------
    L254.StubTranTechCoef_min <- L254.StubTranTechCoef %>%
      group_by(region, supplysector, tranSubsector, stub.technology,
               year, minicam.energy.input, market.name) %>%
      summarise(coefficient = min(coefficient)) %>%
      ungroup
    L354.FuelStandards_max <- L354.FuelStandards_new %>%
      left_join_error_no_match(L254.StubTranTechCoef_min,
                               by = c("region", "supplysector", "tranSubsector", "stub.technology",
                                      "minicam.energy.input", "year",  "market.name")) %>%
      mutate(coefficient = pmax(coefficient.x, coefficient.y)) %>%
      select(xml, LEVEL2_DATA_NAMES[["StubTranTechCoef"]])

    # 4.Make sure future coefficients don't increase ---------------
    L354.FuelStandards_future <- L354.FuelStandards_max %>%
      group_by(xml, region, supplysector, tranSubsector, stub.technology, minicam.energy.input, market.name) %>%
      complete(nesting(xml, region, supplysector, tranSubsector, stub.technology, minicam.energy.input, market.name),
               year = seq(min(year), max(MODEL_FUTURE_YEARS), 5)) %>%
      left_join(filter(L254.StubTranTechCoef, sce == "CORE"),
                by = c("region", "supplysector", "tranSubsector", "stub.technology",
                       "minicam.energy.input", "market.name", "year")) %>%
      # We want to know if there would be any increases in coefficient for the time series
      mutate(coefficient = if_else(is.na(coefficient.x), coefficient.y, coefficient.x),
             rebound = coefficient > lag(coefficient)) %>%
      # Filter to the time series with rebounds
      filter(any(rebound)) %>%
      # Set the coefficient to the minimum of all previous values
      mutate(coefficient = cummin(coefficient)) %>%
      ungroup %>%
      # We only need the values where that new coefficient would be less than the CORE scenario
      filter(coefficient < coefficient.y) %>%
      select(-coefficient.x, -coefficient.y, -rebound, -sce)

    # Produce outputs ---------------
    L354.FuelStandards_future %>%
      add_title("Policy fuel standards", overwrite = T) %>%
      add_units("BTU/vkm") %>%
      add_precursors("policy/A_FuelStandards",
                     "L254.StubTranTechCoef") ->
      L354.FuelStandards

    return_data(L354.FuelStandards)
  } else {
    stop("Unknown command")
  }
}

