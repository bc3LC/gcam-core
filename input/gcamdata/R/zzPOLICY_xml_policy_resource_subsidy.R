# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_resource_subsidy_xml
#'
#' Construct XML data structure for \code{resource_subsidy.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aeei.xml}.
module_policy_resource_subsidy_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_resource_subsidy.csv", "resource_subsidy.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L310.RenewRsrc",
             "L310.RenewRsrcPrice",
             "L310.SmthRenewRsrcCurves",
             "L310.ResTechShrwt",
             "L310.GlobalTranTechShrwt",
             "L310.GlobalTranTechSCurve",
             "L310.StubTranTechLoadFactor",
             "L310.StubTranTechCost",
             "L310.StubTechTrackCapital",
             "L310.StubTranTechCalInput",
             "L310.StubTranTechShwtFuture",
             "L310.StubTranTechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L310.RenewRsrc <- get_data(all_data, "L310.RenewRsrc")
    L310.RenewRsrcPrice <- get_data(all_data, "L310.RenewRsrcPrice")
    L310.SmthRenewRsrcCurves <- get_data(all_data, "L310.SmthRenewRsrcCurves")
    L310.ResTechShrwt <- get_data(all_data, "L310.ResTechShrwt")
    L310.GlobalTranTechShrwt <- get_data(all_data, "L310.GlobalTranTechShrwt")
    L310.GlobalTranTechSCurve <- get_data(all_data, "L310.GlobalTranTechSCurve")
    L310.StubTranTechLoadFactor <- get_data(all_data, "L310.StubTranTechLoadFactor")
    L310.StubTranTechCost <- get_data(all_data, "L310.StubTranTechCost")
    L310.StubTechTrackCapital <- get_data(all_data, "L310.StubTechTrackCapital")
    L310.StubTranTechCalInput <- get_data(all_data, "L310.StubTranTechCalInput")
    L310.StubTranTechShwtFuture <- get_data(all_data, "L310.StubTranTechShwtFuture")
    L310.StubTranTechCoef <- get_data(all_data, "L310.StubTranTechCoef")

    # ===================================================

    # Produce outputs
    for (xml_name in all_xml_names){
      L310.RenewRsrc_tmp <- L310.RenewRsrc %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.RenewRsrcPrice_tmp <- L310.RenewRsrcPrice %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.SmthRenewRsrcCurves_tmp <- L310.SmthRenewRsrcCurves %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.ResTechShrwt_tmp <- L310.ResTechShrwt %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.GlobalTranTechShrwt_tmp <- L310.GlobalTranTechShrwt %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.GlobalTranTechSCurve_tmp <- L310.GlobalTranTechSCurve %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTranTechLoadFactor_tmp <- L310.StubTranTechLoadFactor %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTranTechCost_tmp <- L310.StubTranTechCost %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTechTrackCapital_tmp <- L310.StubTechTrackCapital %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTranTechCalInput_tmp <- L310.StubTranTechCalInput %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTranTechShwtFuture_tmp <-  L310.StubTranTechShwtFuture %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTranTechCoef_tmp <- L310.StubTranTechCoef %>%
        filter(xml == xml_name) %>%
        select(-xml)

      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L310.RenewRsrc_tmp, "RenewRsrc") %>%
               add_xml_data(L310.RenewRsrcPrice_tmp, "RenewRsrcPrice") %>%
               add_xml_data(L310.SmthRenewRsrcCurves_tmp, "SmthRenewRsrcCurves") %>%
               add_xml_data(L310.ResTechShrwt_tmp, "RenewResTechShrwt") %>%
               add_xml_data(L310.StubTranTechCalInput_tmp, "StubTranTechCalInput") %>%
               add_xml_data(L310.StubTranTechLoadFactor_tmp, "StubTranTechLoadFactor") %>%
               add_node_equiv_xml("subsector") %>%
               add_xml_data(L310.StubTechTrackCapital_tmp, "StubTechTrackCapital") %>%
               add_xml_data(L310.StubTranTechCost_tmp, "StubTranTechCost") %>%
               add_xml_data(L310.StubTranTechShwtFuture_tmp, "StubTranTechShrwt") %>%
               add_xml_data(L310.StubTranTechCoef_tmp, "StubTranTechCoef") %>%
               add_xml_data(L310.GlobalTranTechShrwt_tmp, "GlobalTranTechShrwt") %>%
               add_xml_data(L310.GlobalTranTechSCurve_tmp, "GlobalTranTechSCurve") %>%
               add_precursors("L310.RenewRsrc",
                              "L310.RenewRsrcPrice",
                              "L310.SmthRenewRsrcCurves",
                              "L310.ResTechShrwt",
                              "L310.GlobalTranTechShrwt",
                              "L310.GlobalTranTechSCurve",
                              "L310.StubTranTechLoadFactor",
                              "L310.StubTranTechCost",
                              "L310.StubTechTrackCapital",
                              "L310.StubTranTechCalInput",
                              "L310.StubTranTechShwtFuture",
                              "L310.StubTranTechCoef"))

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
