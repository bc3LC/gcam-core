# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_aeei_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aeei.xml}.
module_policy_aeei_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_aeei.csv", "policy_aeei.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L326.aeei"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L326.aeei <- get_data(all_data, "L326.aeei")
    # ===================================================

    # Produce outputs
    create_xml("policy_aeei.xml") %>%
      add_xml_data(L326.aeei, "aeei") %>%
      add_precursors("L326.aeei") ->
      policy_aeei.xml

    for (xml_name in all_xml_names){
      L326.aeei_tmp <- L326.aeei %>%
        filter(xml == xml_name) %>%
        select(-xml)
      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L326.aeei_tmp, "aeei") %>%
               add_precursors("L326.aeei"))

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
