# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L326.aeei
#'
#' Produce aeei (autonomous energy efficiency improvement index) parameters by region and sector
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L326.aeei}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 20123
module_policy_L326.aeei <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_aeei"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L326.aeei"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_aeei <- get_data(all_data, "policy/A_aeei")

    # Convert to long format and interpolate any missing years
    L326.aeei <- A_aeei %>%
      gather_years() %>%
      # Interpolates between min and max years in A_aeei
      complete(nesting(xml, region, energy.final.demand), year = seq(min(year), max(year), 5)) %>%
      group_by(xml, region, energy.final.demand) %>%
      mutate(value = approx_fun(year, value)) %>%
      ungroup %>%
      rename(aeei = value)

    # Produce outputs
    L326.aeei %>%
      add_title("AEEI (autonomous energy efficiency improvement index) parameters by region and sector", overwrite = T) %>%
      add_units("Unitless") %>%
      add_precursors("policy/A_aeei") ->
      L326.aeei

    return_data(L326.aeei)
  } else {
    stop("Unknown command")
  }
}
