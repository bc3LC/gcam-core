# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_303.shareweight_overwrite
#'
#' Shareweights to overwrite
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L303.shareweight_overwrite_subsector}, \code{L303.shareweight_overwrite_stubtech}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_303.shareweight_overwrite <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_Shareweights"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L303.shareweight_overwrite_subsector",
             "L303.shareweight_overwrite_trnSubsector",
             "L303.shareweight_overwrite_stubtech",
             "L303.shareweight_overwrite_trnStubtech"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_Shareweights <- get_data(all_data, "policy/A_Shareweights") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # Convert to long and get rid of NA values
    L303.shareweight_overwrite <- A_Shareweights %>%
      gather_years(value_col = "share.weight") %>%
      filter(!is.na(share.weight))

    # Add in interpolation rule - just extend last year to 2100
    L303.shareweight_interp <- L303.shareweight_overwrite %>%
      group_by(xml, region, supplysector, subsector, stub.technology) %>%
      mutate(from.year = max(year)) %>%
      ungroup %>%
      mutate(apply.to = "share-weight", to.year = max(MODEL_YEARS), delete = 1, interpolation.function = "fixed")

    # If "trn_" in supplysector, change subsector to tranSubsector
    L303.shareweight_interp_trn <- L303.shareweight_interp %>%
      filter(grepl("trn", supplysector)) %>%
      rename(tranSubsector = subsector)

    L303.shareweight_interp <- L303.shareweight_interp %>%
      filter(!grepl("trn", supplysector))

    # Produce outputs
    L303.shareweight_interp %>%
      filter(is.na(stub.technology)) %>%
      select(-stub.technology) %>%
      add_title("Subsector shareweights to overwrite", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Shareweights") ->
      L303.shareweight_overwrite_subsector

    L303.shareweight_interp %>%
      filter(!is.na(stub.technology))  %>%
      add_title("Stubtech shareweights to overwrite", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Shareweights") ->
      L303.shareweight_overwrite_stubtech

    L303.shareweight_interp_trn %>%
      filter(is.na(stub.technology)) %>%
      select(-stub.technology) %>%
      add_title("tranSubsector shareweights to overwrite", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Shareweights") ->
      L303.shareweight_overwrite_trnSubsector

    L303.shareweight_interp_trn %>%
      filter(!is.na(stub.technology))  %>%
      add_title("tranStubtech shareweights to overwrite", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_Shareweights") ->
      L303.shareweight_overwrite_trnStubtech

    return_data(L303.shareweight_overwrite_subsector,
                L303.shareweight_overwrite_stubtech,
                L303.shareweight_overwrite_trnSubsector,
                L303.shareweight_overwrite_trnStubtech)
  } else {
    stop("Unknown command")
  }
}

