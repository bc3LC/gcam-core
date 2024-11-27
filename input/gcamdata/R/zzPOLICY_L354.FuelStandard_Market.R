# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L354.FuelStandards_Market
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L354.NewTrnCoefs}, \code{L354.FuelStandards_coef},\code{L354.FuelStandards_secout},\code{L354.FuelStandards_policy}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L354.FuelStandards_Market <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_FuelStandards_Market",
             "L254.StubTranTechCoef",
             "L254.StubTranTechLoadFactor",
             FILE = "emissions/A_PrimaryFuelCCoef",
             FILE = "policy/A_FuelStandards_Market_CoefOverwrite",
             FILE = "policy/A_FuelStandards_Market_OutputOverwrite"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L354.NewTrnCoefs",
             "L354.FuelStandards_coef",
             "L354.FuelStandards_secout",
             "L354.FuelStandards_policy"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_FuelStandards_Market <- get_data(all_data, "policy/A_FuelStandards_Market")
    A_FuelStandards_Market_CoefOverwrite <- get_data(all_data, "policy/A_FuelStandards_Market_CoefOverwrite")
    A_FuelStandards_Market_OutputOverwrite <- get_data(all_data, "policy/A_FuelStandards_Market_OutputOverwrite")
    A_PrimaryFuelCCoef <- get_data(all_data, "emissions/A_PrimaryFuelCCoef")
    L254.StubTranTechCoef <- get_data(all_data, "L254.StubTranTechCoef")
    L254.StubTranTechLoadFactor <- get_data(all_data, "L254.StubTranTechLoadFactor") %>%
      distinct(region, supplysector, tranSubsector, year, loadFactor, sce)

    SSP_SCE <- "CORE"

    # Need this so that coefficient will end up as vkm
    BTU_per_EJ <- 1 / (CONV_KBTU_EJ/1000)

    # Convert to long and interpolate between model years
    A_FuelStandards_Market <- A_FuelStandards_Market %>%
      gather_years(value_col = "coefficient") %>%
      filter(!is.na(coefficient)) %>%
      group_by(xml, policy_name, market, SSP_sce, region, supplysector, tranSubsector, Units) %>%
      complete(nesting(xml, policy_name, market, SSP_sce, region, supplysector, tranSubsector, Units),
               year = seq(min(year), max(year), 5)) %>%
      # If group only has one, approx_fun doesn't work, so we use this workaround
      mutate(coefficient_NA = approx_fun(year, coefficient)) %>%
      ungroup %>%
      mutate(coefficient = if_else(!is.na(coefficient_NA), coefficient_NA, coefficient)) %>%
      select(-coefficient_NA)

    # We want roughly half of the fuel standard to be met by improved coefficients
    # But with the maximum coefficient improvement to the lowest of the SSPs
    # First, calculate CO2 coefficients implied from energy inputs in liquid/gas vehicles
    L354.baseCO2pervkm <- L254.StubTranTechCoef %>%
      semi_join(A_FuelStandards_Market, by = c("region", "supplysector", "tranSubsector", "year")) %>%
      filter(grepl("liquids|gas", minicam.energy.input)) %>%
      left_join_error_no_match(A_PrimaryFuelCCoef, by = c("minicam.energy.input" = "PrimaryFuelCO2Coef.name")) %>%
      mutate(gCO2_per_vkm = coefficient * CONV_BTU_KJ / 1e3 * PrimaryFuelCO2Coef * 44 / 12)

    # Add in min coefficient from all SSP scenarios
    L354.baseCO2pervkm <- L354.baseCO2pervkm %>%
      group_by(region, supplysector, tranSubsector, stub.technology, year) %>%
      mutate(min_coef = min(coefficient),
             min_CO2 = min(gCO2_per_vkm)) %>%
      ungroup

    # Now compare default inputs with the fuel standard
    # If the largest min_CO2 is less than target, use default coefficient
    # If largest min_CO2 is greater than target use x = min_CO2 - 0.5 * (min_CO2 - CO2_target)
    # If max(x) greater than min_CO2, calculate percent diff between max(x) and gCO2_per_vkm, apply pct diff to all coefficients in supplysector
    # If max(x) less than min_CO2, use min_coef for entire supplysector
    L354.NewTrnCoefs <- L354.baseCO2pervkm %>%
      semi_join(A_FuelStandards_Market, by = c("region", "supplysector", "tranSubsector", "year", "sce" = "SSP_sce")) %>%
      left_join_error_no_match(select(A_FuelStandards_Market, region, supplysector, tranSubsector, year, SSP_sce, CO2_target = coefficient),
                               by = c("region", "supplysector", "tranSubsector", "year" , "sce" = "SSP_sce")) %>%
      mutate(x = min_CO2 - 0.5 * (min_CO2 - CO2_target),
             x_pct = x / gCO2_per_vkm) %>%
      group_by(region, supplysector, year) %>%
      mutate(x_pct = x_pct[gCO2_per_vkm == max(gCO2_per_vkm)],
             new_coef = if_else(max(min_CO2) < CO2_target, coefficient,
                          if_else(max(x) < min_CO2, min_coef,
                                  coefficient * x_pct)),
             new_coef = if_else(new_coef < min_coef, min_coef, new_coef)
             ) %>%
      ungroup %>%
      select(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input, coefficient = new_coef, market.name)

    # If there are any coefs to overwrite, do it here
    if(nrow(A_FuelStandards_Market_CoefOverwrite) > 0){
      CoefOverwrite <- A_FuelStandards_Market_CoefOverwrite %>%
        gather_years(value_col = "coefficient") %>%
        filter(!is.na(coefficient)) %>%
        group_by(region, supplysector, tranSubsector, stub.technology) %>%
        complete(nesting(region, supplysector, tranSubsector, stub.technology),
                 year = seq(min(year), max(year), 5)) %>%
        # If group only has one, approx_fun doesn't work, so we use this workaround
        mutate(coefficient_NA = as.numeric(approx_fun(year, coefficient))) %>%
        ungroup %>%
        mutate(coefficient = if_else(!is.na(coefficient_NA), coefficient_NA, coefficient)) %>%
        select(-coefficient_NA) %>%
        mutate(market.name = region) %>%
        left_join_error_no_match(L254.StubTranTechCoef, by = c("region", "supplysector", "tranSubsector",
                                                               "stub.technology", "year", "market.name", "SSP_sce" = "sce")) %>%
        select(region, supplysector, tranSubsector, stub.technology, year, minicam.energy.input, coefficient = coefficient.x, market.name)

      L354.NewTrnCoefs <- anti_join(L354.NewTrnCoefs, CoefOverwrite,
                                    by = c("region", "supplysector", "tranSubsector", "stub.technology",
                                           "year", "minicam.energy.input", "market.name")) %>%
        bind_rows(CoefOverwrite)
    }

    # Ensure that future coefficients don't rebound
    L354.NewTrnCoefs_future <- L354.NewTrnCoefs %>%
      group_by(region, supplysector, tranSubsector, stub.technology, minicam.energy.input, market.name) %>%
      complete(nesting(region, supplysector, tranSubsector, stub.technology, minicam.energy.input, market.name),
               year = seq(min(year), max(MODEL_FUTURE_YEARS), 5)) %>%
      left_join(filter(L254.StubTranTechCoef, sce == SSP_SCE),
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


    L354.NewTrnCoefs <- bind_rows(anti_join(L354.NewTrnCoefs, L354.NewTrnCoefs_future,
                                            by = c("region", "supplysector", "tranSubsector", "stub.technology",
                                                   "year", "minicam.energy.input", "market.name")),
                                  L354.NewTrnCoefs_future)

    # Create CO2 based markets - first need the correct coefficients
    all_coefs <- L254.StubTranTechCoef %>%
      anti_join(L354.NewTrnCoefs,
                by = c("region", "supplysector", "tranSubsector", "stub.technology", "year", "minicam.energy.input", "market.name")) %>%
      bind_rows(mutate(L354.NewTrnCoefs, sce = SSP_SCE)) %>%
      filter(sce == SSP_SCE)


    L354.FuelStandards_Market <- A_FuelStandards_Market %>%
      # Join in default GCAM coefficients and load factors and calculate coefficient and sec output for fuel market
      left_join(all_coefs,
                               by = c("region", "supplysector", "tranSubsector",
                                      "year")) %>%
      # A few places where there are slight differences in loadfactor by tech (probably an error)
      # so here just keep first since the differences are so small
      left_join_keep_first_only(L254.StubTranTechLoadFactor,
                by = c("region", "supplysector", "tranSubsector",
                       "year","SSP_sce" = "sce")) %>%
      left_join(A_PrimaryFuelCCoef, by = c("minicam.energy.input" = "PrimaryFuelCO2Coef.name")) %>%
      # Every policy needs different name because we want this to apply to new cars only
      mutate(gCO2_per_vkm = coefficient.y * CONV_BTU_KJ / 1e3 * PrimaryFuelCO2Coef * 44 / 12,
             coefficient = if_else(is.na(gCO2_per_vkm), 0, BTU_per_EJ * gCO2_per_vkm * loadFactor / coefficient.x / 1e6),
             output.ratio = 1,
             policyType = "RES",
             # Every policy needs different name because we want this to apply to new cars only
             policy_name = paste0(policy_name, year))

    if(nrow(A_FuelStandards_Market_OutputOverwrite) > 0){
      OutputOverwrite <- A_FuelStandards_Market_OutputOverwrite %>%
        gather_years(value_col = "output.ratio") %>%
        filter(!is.na(output.ratio)) %>%
        group_by(policy_name, market, region, supplysector, tranSubsector, stub.technology) %>%
        complete(nesting(policy_name, market, region, supplysector, tranSubsector, stub.technology),
                 year = seq(min(year), max(year), 5)) %>%
        # If group only has one, approx_fun doesn't work, so we use this workaround
        mutate(output.ratio_NA = as.numeric(approx_fun(year, output.ratio))) %>%
        ungroup %>%
        mutate(output.ratio = if_else(is.na(output.ratio_NA), output.ratio, output.ratio_NA),
               policy_name = paste0(policy_name, year)) %>%
        select(-output.ratio_NA)

      OutputOverwrite <- L354.FuelStandards_Market %>%
        inner_join(OutputOverwrite, by = c("policy_name", "region", "supplysector", "market",
                                           "tranSubsector", "year", "stub.technology")) %>%
        mutate(output.ratio = output.ratio.y) %>%
        select(-output.ratio.x, -output.ratio.y)

      L354.FuelStandards_Market <- L354.FuelStandards_Market %>%
        anti_join(OutputOverwrite, by = c("policy_name", "region", "supplysector", "market",
                                          "tranSubsector", "year", "stub.technology")) %>%
        bind_rows(OutputOverwrite)
    }

    # Produce outputs
    L354.NewTrnCoefs %>%
      add_title("Calculated new fuel coefficients", overwrite = T) %>%
      add_units("BTU/vkm") %>%
      add_precursors("policy/A_FuelStandards_Market",
                     "L254.StubTranTechCoef",
                     "emissions/A_PrimaryFuelCCoef") ->
    L354.NewTrnCoefs

    L354.FuelStandards_Market  %>%
      select(xml, region, supplysector, tranSubsector, stub.technology, year,
             minicam.energy.input = policy_name, coefficient) %>%
      add_title("Policy fuel coefficients", overwrite = T) %>%
      add_units("BTU/EJ") %>%
      add_precursors("policy/A_FuelStandards_Market",
                     "L254.StubTranTechCoef",
                     "emissions/A_PrimaryFuelCCoef") ->
      L354.FuelStandards_coef

    L354.FuelStandards_Market %>%
      select(xml, region, supplysector, tranSubsector, stub.technology, year,
             res.secondary.output = policy_name, output.ratio) %>%
      add_title("Policy fuel secondary output", overwrite = T) %>%
      add_units("million vkm") %>%
      add_precursors("policy/A_FuelStandards_Market",
                     "L254.StubTranTechCoef",
                     "emissions/A_PrimaryFuelCCoef") ->
      L354.FuelStandards_secout

    L354.FuelStandards_Market %>%
      select(xml, region, policy.portfolio.standard = policy_name,
            market, policyType, year) %>%
      mutate(constraint = 1) %>%
      distinct() %>%
      add_title("Policy fuel market", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_FuelStandards_Market",
                     "L254.StubTranTechCoef",
                     "emissions/A_PrimaryFuelCCoef") ->
    L354.FuelStandards_policy

    return_data(L354.NewTrnCoefs, L354.FuelStandards_coef, L354.FuelStandards_secout, L354.FuelStandards_policy)
  } else {
    stop("Unknown command")
  }
}

