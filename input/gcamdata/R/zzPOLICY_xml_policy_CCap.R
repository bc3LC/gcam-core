# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_CCap_xml
#'
#' Construct XML data structure for \code{CCap.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_CCap.xml}.
module_policy_CCap_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_CCap_Constraint.csv", "policy_CCap.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L3221.CCap_constraint",
             "L3221.CCap_link_regions",
             "L3221.CCap_tech",
             "L3221.CCap_tranTech",
             "L3221.CCap_resource",
             "L3221.CCap_GHG_Link",
             "L3221.CCap_global_tech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L3221.CCap_constraint <- get_data(all_data, "L3221.CCap_constraint")
    L3221.CCap_link_regions <- get_data(all_data, "L3221.CCap_link_regions")
    L3221.CCap_tech <- get_data(all_data, "L3221.CCap_tech")
    L3221.CCap_tranTech <- get_data(all_data, "L3221.CCap_tranTech")
    L3221.CCap_resource <- get_data(all_data, "L3221.CCap_resource")
    L3221.CCap_GHG_Link <- get_data(all_data, "L3221.CCap_GHG_Link")
    L3221.CCap_global_tech <- get_data(all_data, "L3221.CCap_global_tech")
    # ===================================================
    # Need to split L3221.CCap_constraint into years with fillout and years without
    L3221.CCap_constraint_fillout <- L3221.CCap_constraint %>%
      filter(year.fillout == constraint.year)

    L3221.CCap_constraint_noFillout <- L3221.CCap_constraint %>%
      filter(is.na(year.fillout) | year.fillout != constraint.year) %>%
      select(-year.fillout)

    # Produce outputs
    for (xml_name in all_xml_names){
      L3221.CCap_constraint_noFillout_tmp <- L3221.CCap_constraint_noFillout %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L3221.CCap_constraint_fillout_tmp <- L3221.CCap_constraint_fillout %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L3221.CCap_link_regions_tmp <- L3221.CCap_link_regions %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L3221.CCap_tech_tmp <- L3221.CCap_tech %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L3221.CCap_tranTech_tmp <- L3221.CCap_tranTech %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L3221.CCap_resource_tmp <- L3221.CCap_resource %>%
        filter(xml == xml_name) %>%
        select(-xml)


      L3221.CCap_GHG_Link_tmp <- L3221.CCap_GHG_Link %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L3221.CCap_global_tech_tmp <- L3221.CCap_global_tech %>%
        filter(xml == xml_name) %>%
        select(-xml)

      # Produce outputs
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L3221.CCap_constraint_noFillout_tmp, "GHGConstr") %>%
               add_xml_data(L3221.CCap_constraint_fillout_tmp, "GHGConstrFillout") %>%
               add_xml_data(L3221.CCap_link_regions_tmp, "GHGConstrMkt") %>%
               add_xml_data(L3221.CCap_tech_tmp, "StubTechCO2") %>%
               add_xml_data(L3221.CCap_tranTech_tmp, "StubTranTechCO2") %>%
               add_xml_data(L3221.CCap_resource_tmp, "ResTechCO2") %>%
               add_xml_data(L3221.CCap_GHG_Link_tmp, "GHGConstrLinkPriceAdj") %>%
               add_xml_data(L3221.CCap_GHG_Link_tmp, "GHGConstrLinkDemandAdj") %>%
               add_xml_data(L3221.CCap_GHG_Link_tmp, "GHGConstrLinkMktUnits") %>%
               add_xml_data(L3221.CCap_global_tech_tmp, "GlobalTechCO2") %>%
               add_precursors("L3221.CCap_constraint",
                              "L3221.CCap_link_regions",
                              "L3221.CCap_tech",
                              "L3221.CCap_tranTech",
                              "L3221.CCap_resource",
                              "L3221.CCap_GHG_Link",
                              "L3221.CCap_global_tech")
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
