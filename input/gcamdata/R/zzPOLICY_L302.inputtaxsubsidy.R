# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_302.inputtaxsubsidy
#'
#' Produce fuel standards coefficients
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L302.InputTax}, \code{L302.InputSubsidy}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_302.inputtaxsubsidy <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_InputTaxesSubsidies",
             FILE = "policy/A_InputCapitalFCR"
    ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L302.InputTax",
             "L302.InputTranTax",
             "L302.InputCapitalFCR"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    A_InputTaxesSubsidies <- get_data(all_data, "policy/A_InputTaxesSubsidies") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    A_InputCapitalFCR <- get_data(all_data, "policy/A_InputCapitalFCR") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # Convert to long
    L302.InputTax <- A_InputTaxesSubsidies %>%
      gather_years(value_col = "input.cost") %>%
      na.omit() %>%
      policy_interpolate(group_cols = c(xml, region, supplysector, subsector, stub.technology, minicam.non.energy.input),
                         value_col = input.cost)

    L302.InputTranTax <- L302.InputTax %>%
      filter(grepl("^trn_", supplysector)) %>%
      rename(tranSubsector = subsector)

    L302.InputTax <- L302.InputTax %>%
      filter(!grepl("^trn_", supplysector))

    L302.InputCapitalFCR <- A_InputCapitalFCR %>%
      gather_years(value_col = "fixed.charge.rate") %>%
      mutate(fixed.charge.rate = as.numeric(fixed.charge.rate)) %>%
      na.omit() %>%
      policy_interpolate(group_cols = c(xml, region, supplysector, subsector, stub.technology, input.capital),
                         value_col = fixed.charge.rate)


    # Produce outputs
    L302.InputTax %>%
      add_title("Input taxes for specific techs", overwrite = T) %>%
      add_units("$1975 (usually per GJ") %>%
      add_precursors("policy/A_InputTaxesSubsidies") ->
      L302.InputTax

    L302.InputTranTax %>%
      add_title("Input taxes for tran techs", overwrite = T) %>%
      add_units("$1975 (usually per GJ") %>%
      add_precursors("policy/A_InputTaxesSubsidies") ->
      L302.InputTranTax

    L302.InputCapitalFCR %>%
      add_title("fixed charge rate for capital costs", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_InputCapitalFCR") ->
      L302.InputCapitalFCR


    return_data(L302.InputTax,
                L302.InputTranTax,
                L302.InputCapitalFCR)
  } else {
    stop("Unknown command")
  }
}

