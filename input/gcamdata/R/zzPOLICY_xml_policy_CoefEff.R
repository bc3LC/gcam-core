# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_CoefEff_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_CoefEff.xml}.
module_policy_CoefEff_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_Coef_Eff.csv", "policy_CoefEff.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L304.StubTechCoef",
             "L304.StubTechEff"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L304.StubTechCoef <- get_data(all_data, "L304.StubTechCoef")
    L304.StubTechEff <- get_data(all_data, "L304.StubTechEff")

    # ===================================================

    # Produce outputs
    for (xml_name in all_xml_names){
      L304.StubTechCoef_tmp <- L304.StubTechCoef %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L304.StubTechEff_tmp <- L304.StubTechEff %>%
        filter(xml == xml_name) %>%
        select(-xml)

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L304.StubTechCoef_tmp, "StubTechCoef") %>%
               add_xml_data(L304.StubTechEff_tmp, "StubTechEff") %>%
               add_precursors("L304.StubTechCoef",
                              "L304.StubTechEff")
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
