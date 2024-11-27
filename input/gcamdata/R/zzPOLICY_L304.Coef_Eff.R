# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L304.Coef_Eff
#'
#' Produce tech efficiency/coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L304.StubTechCoef}, \code{L304.StubTechEff}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L304.Coef_Eff <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_Coef_Eff"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L304.StubTechCoef",
             "L304.StubTechEff"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_Coef_Eff <- get_data(all_data, "policy/A_Coef_Eff")

    # Convert to long and fill in in-between years
    L304.StubTechCoef <- A_Coef_Eff %>%
      filter(type == "coefficient") %>%
      gather_years(value_col = "coefficient") %>%
      filter(!is.na(coefficient)) %>%
      # Leaving grouped on purpose here
      group_by(xml, region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, supplysector, subsector, stub.technology, minicam.energy.input),
               year = seq(min(year), max(year), 5)) %>%
      mutate(coefficient = approx_fun(year, coefficient),
             market.name = region) %>%
      ungroup %>%
      select(-type)

    L304.StubTechEff <- A_Coef_Eff %>%
      filter(type == "efficiency") %>%
      gather_years(value_col = "efficiency") %>%
      filter(!is.na(efficiency)) %>%
      # Leaving grouped on purpose here
      group_by(xml, region, supplysector, subsector, stub.technology, minicam.energy.input) %>%
      # Interpolates between min and max years for each region/policy combo
      complete(nesting(xml, region, supplysector, subsector, stub.technology, minicam.energy.input),
               year = seq(min(year), max(year), 5)) %>%
      mutate(efficiency = approx_fun(year, efficiency),
             market.name = region) %>%
      ungroup %>%
      select(-type)

    # Produce outputs
    L304.StubTechCoef %>%
      add_title("Policy fuel coefficients", overwrite = T) %>%
      add_units("EJ/EJ") %>%
      add_precursors("policy/A_Coef_Eff") ->
      L304.StubTechCoef

    L304.StubTechEff %>%
      add_title("Policy fuel efficiency", overwrite = T) %>%
      add_units("EJ/EJ") %>%
      add_precursors("policy/A_Coef_Eff") ->
      L304.StubTechEff

    return_data(L304.StubTechCoef,
                L304.StubTechEff)
  } else {
    stop("Unknown command")
  }
}

