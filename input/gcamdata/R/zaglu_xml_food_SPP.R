# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_food_SPP_xml
#'
#' Construct XML data structure for \code{food_flexitarian_[1_3].xml}
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{spp_x2_2025_0.005.xml}
module_aglu_food_SPP_xml <- function(command, ...) {

  all_xml_names <- return_xml_names("L201.spp_scenarios_names.RData")[1:3]

  if(command == driver.DECLARE_INPUTS) {
    return(c("L203.FuelPrefElast_spp",
             "L201.spp_scenarios_names"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    L201.spp_scenarios_names <- get_data(all_data, 'L201.spp_scenarios_names')
    L203.FuelPrefElast_spp <- get_data(all_data, 'L203.FuelPrefElast_spp')

    for (xml_name in all_xml_names) {
      print(xml_name)
      data = L203.FuelPrefElast_spp[[gsub("\\.xml$", "", xml_name)]]

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(data, "L2_FuelPrefElast") %>%
               add_node_equiv_xml("subsector") %>%
               add_rename_foodsubsec_xml() %>%
               add_precursors("L203.FuelPrefElast_spp"))
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
