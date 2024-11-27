# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_elasticity.xml
#'
#' Construct XML data structure for \code{policy_elasticity.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_inputtax.xml}.
module_policy_elasticity.xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_elasticity.csv", "policy_elasticity.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L300.elasticity_income",
             "L300.elasticity_price",
             "L300.PerCapitaBased_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L300.elasticity_income <- get_data(all_data, "L300.elasticity_income")
    L300.elasticity_price <- get_data(all_data, "L300.elasticity_price")
    L300.PerCapitaBased_trn <- get_data(all_data, "L300.PerCapitaBased_trn")
    # ===================================================

    # Produce outputs

    for (xml_name in all_xml_names){
      L300.PerCapitaBased_trn_tmp  <- L300.PerCapitaBased_trn %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L300.elasticity_income_tmp <- L300.elasticity_income %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L300.elasticity_price_tmp <- L300.elasticity_price %>%
        filter(xml == xml_name) %>%
        select(-xml)


      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L300.PerCapitaBased_trn_tmp, "PerCapitaBased") %>%
               add_xml_data(L300.elasticity_price_tmp, "PriceElasticity") %>%
               add_xml_data(L300.elasticity_income_tmp, "IncomeElasticity") %>%
               add_precursors("L300.elasticity_income",
                              "L300.elasticity_price",
                              "L300.PerCapitaBased_trn")
      )
    }

    # Need this for loop because having issues with lapply(all_xml_names, get)
    list_of_xmls <- list()
    for(xml_name in all_xml_names){
      list_of_xmls[[xml_name]] <- get(xml_name)
    }
    return_multiple_xmls(list_of_xmls, all_xml_names)
  } else {
    stop("Unknown command")
  }
}
