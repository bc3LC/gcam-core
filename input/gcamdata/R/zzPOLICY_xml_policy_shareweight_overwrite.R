# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_shareweight_overwrite_xml
#'
#' Construct XML data structure for \code{policy_shareweight_overwrite.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_shareweight_overwrite.xml}.
module_policy_shareweight_overwrite_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_Shareweights.csv", "policy_shareweight_overwrite.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L303.shareweight_overwrite_subsector",
             "L303.shareweight_overwrite_stubtech",
             "L303.shareweight_overwrite_trnSubsector",
             "L303.shareweight_overwrite_trnStubtech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L303.shareweight_overwrite_subsector <- get_data(all_data, "L303.shareweight_overwrite_subsector")
    L303.shareweight_overwrite_stubtech <- get_data(all_data, "L303.shareweight_overwrite_stubtech")
    L303.shareweight_overwrite_trnSubsector <- get_data(all_data, "L303.shareweight_overwrite_trnSubsector")
    L303.shareweight_overwrite_trnStubtech <- get_data(all_data, "L303.shareweight_overwrite_trnStubtech")
    # ===================================================
    # Produce outputs
    for (xml_name in all_xml_names){
      L303.shareweight_subsector_tmp <- L303.shareweight_overwrite_subsector %>%
        filter(xml == xml_name) %>%
        select(region, supplysector, subsector, year, share.weight)

      L303.shareweight_subsector_interp_tmp <- L303.shareweight_overwrite_subsector %>%
        filter(xml == xml_name) %>%
        distinct(region, supplysector, subsector, from.year, to.year, apply.to, delete, interpolation.function)

      L303.shareweight_stubtech_tmp <- L303.shareweight_overwrite_stubtech %>%
        filter(xml == xml_name) %>%
        select(region, supplysector, subsector, stub.technology, year, share.weight)

      L303.shareweight_stubtech_interp_tmp <- L303.shareweight_overwrite_stubtech %>%
        filter(xml == xml_name) %>%
        distinct(region, supplysector, subsector, stub.technology, from.year, to.year, apply.to, delete, interpolation.function)

      L303.shareweight_trnSubsector_tmp <- L303.shareweight_overwrite_trnSubsector %>%
        filter(xml == xml_name) %>%
        select(region, supplysector, tranSubsector, year, share.weight)

      L303.shareweight_trnSubsector_interp_tmp <- L303.shareweight_overwrite_trnSubsector %>%
        filter(xml == xml_name) %>%
        distinct(region, supplysector, tranSubsector, from.year, to.year, apply.to, delete, interpolation.function)

      L303.shareweight_trnStubtech_tmp <- L303.shareweight_overwrite_trnStubtech %>%
        filter(xml == xml_name) %>%
        select(region, supplysector, tranSubsector, stub.technology, year, share.weight)

      L303.shareweight_trnStubtech_interp_tmp <- L303.shareweight_overwrite_trnStubtech %>%
        filter(xml == xml_name) %>%
        distinct(region, supplysector, tranSubsector, stub.technology, from.year, to.year, apply.to, delete, interpolation.function)

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L303.shareweight_subsector_interp_tmp, "SubsectorDeleteInterp") %>%
               add_xml_data(L303.shareweight_subsector_tmp, "SubsectorShrwt") %>%
               add_xml_data(L303.shareweight_stubtech_interp_tmp, "StubTechDeleteInterp") %>%
               add_xml_data(L303.shareweight_stubtech_tmp, "StubTechShrwt") %>%
               add_xml_data(L303.shareweight_trnSubsector_interp_tmp, "TranSubsectorDeleteInterp") %>%
               add_xml_data(L303.shareweight_trnSubsector_tmp, "tranSubsectorShrwt") %>%
               add_xml_data(L303.shareweight_trnStubtech_interp_tmp, "TranStubTechDeleteInterp") %>%
               add_xml_data(L303.shareweight_trnStubtech_tmp, "StubTranTechShrwt") %>%
               add_precursors("L303.shareweight_overwrite_subsector",
                              "L303.shareweight_overwrite_stubtech",
                              "L303.shareweight_overwrite_trnSubsector",
                              "L303.shareweight_overwrite_trnStubtech")
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
