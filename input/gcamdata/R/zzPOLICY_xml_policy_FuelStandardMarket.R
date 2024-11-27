# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_FuelStandardMarket_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_FuelStandardMarket.xml}.
module_policy_FuelStandardMarket_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_FuelStandards_Market.csv", "policy_FuelStandardMarket.xml")
  if(command == driver.DECLARE_INPUTS) {
    return(c("L354.NewTrnCoefs",
             "L354.FuelStandards_coef",
             "L354.FuelStandards_secout",
             "L354.FuelStandards_policy"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]


    # Load required inputs
    L354.NewTrnCoefs <- get_data(all_data, "L354.NewTrnCoefs")
    L354.FuelStandards_policy <- get_data(all_data, "L354.FuelStandards_policy")
    L354.FuelStandards_coef <- get_data(all_data, "L354.FuelStandards_coef")
    L354.FuelStandards_secout <- get_data(all_data, "L354.FuelStandards_secout")

    # ===================================================

    for (xml_name in all_xml_names){
      L354.policy_port_stnd_tmp <- L354.FuelStandards_policy %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L354.FuelStandards_coef_tmp <- L354.FuelStandards_coef %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L354.FuelStandards_secout_tmp <- L354.FuelStandards_secout %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L354.NewTrnCoefs_tmp <- L354.NewTrnCoefs %>%
        semi_join(L354.FuelStandards_coef_tmp,
                  by = c("region", "supplysector", "tranSubsector", "stub.technology"))


      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L354.policy_port_stnd_tmp, "PortfolioStdConstraint") %>%
               add_xml_data(L354.FuelStandards_coef_tmp, "StubTranTechCoef_NM") %>%
               add_xml_data(L354.FuelStandards_secout_tmp, "StubTranTechRESOutput") %>%
               add_xml_data(L354.NewTrnCoefs_tmp, "StubTranTechCoef") %>%
               add_precursors("L354.FuelStandards_coef",
                              "L354.FuelStandards_secout",
                              "L354.FuelStandards_policy",
                              "L354.NewTrnCoefs")
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
