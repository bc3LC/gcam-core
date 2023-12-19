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
  INPUT_NAMES <- c("L310.RenewRsrc",
                   "L310.RenewRsrcPrice",
                   "L310.SmthRenewRsrcCurves",
                   "L310.ResTechShrwt",
                   # TRANSPORT
                   "L310.GlobalTranTechShrwt",
                   "L310.GlobalTranTechSCurve",
                   "L310.StubTranTechLoadFactor",
                   "L310.StubTranTechCost",
                   "L310.StubTechTrackCapital",
                   "L310.StubTranTechCalInput",
                   "L310.StubTranTechShwtFuture",
                   "L310.StubTranTechCoef",
                   # BUILDINGS
                   "L310.GlobalTechShrwt_bld",
                   "L310.GlobalTechCost_bld",
                   "L310.GlobalTechTrackCapital_bld",
                   "L310.SubsectorShrwt",
                   "L310.SubsectorLogit_bld",
                   "L310.FuelPrefElast_bld",
                   "L310.StubTech_bld",
                   "L310.StubTechEff_bld",
                   "L310.StubTechCalInputNoShrwt_bld",
                   "L310.StubTechIntGainOutputRatio")
  if(command == driver.DECLARE_INPUTS) {
    return(INPUT_NAMES)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, data_list = INPUT_NAMES)

    # ===================================================

    # Produce outputs
    for (xml_name in all_xml_names){
      # RESOURCES ----------------
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

      # TRANSPORT ----------------
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

      # BUILDINGS ----------------
      L310.GlobalTechShrwt_bld_tmp <- L310.GlobalTechShrwt_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.GlobalTechCost_bld_tmp <- L310.GlobalTechCost_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.GlobalTechTrackCapital_bld_tmp <- L310.GlobalTechTrackCapital_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.SubsectorShrwt_tmp <- L310.SubsectorShrwt %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.SubsectorLogit_bld_tmp <- L310.SubsectorLogit_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.FuelPrefElast_bld_tmp <- L310.FuelPrefElast_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTech_bld_tmp <- L310.StubTech_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTechEff_bld_tmp <- L310.StubTechEff_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTechCalInputNoShrwt_bld_tmp <- L310.StubTechCalInputNoShrwt_bld %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L310.StubTechIntGainOutputRatio_tmp <- L310.StubTechIntGainOutputRatio %>%
        filter(xml == xml_name) %>%
        select(-xml)

      # Produce output-------------
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L310.RenewRsrc_tmp, "RenewRsrc") %>%
               add_xml_data(L310.RenewRsrcPrice_tmp, "RenewRsrcPrice") %>%
               add_xml_data(L310.SmthRenewRsrcCurves_tmp, "SmthRenewRsrcCurves") %>%
               add_xml_data(L310.ResTechShrwt_tmp, "RenewResTechShrwt") %>%
               # TRANSPORT
               add_xml_data(L310.StubTranTechCalInput_tmp, "StubTranTechCalInput") %>%
               add_xml_data(L310.StubTranTechLoadFactor_tmp, "StubTranTechLoadFactor") %>%
               add_node_equiv_xml("subsector") %>%
               add_xml_data(L310.StubTechTrackCapital_tmp, "StubTechTrackCapital") %>%
               add_xml_data(L310.StubTranTechCost_tmp, "StubTranTechCost") %>%
               add_xml_data(L310.StubTranTechShwtFuture_tmp, "StubTranTechShrwt") %>%
               add_xml_data(L310.StubTranTechCoef_tmp, "StubTranTechCoef") %>%
               add_xml_data(L310.GlobalTranTechShrwt_tmp, "GlobalTranTechShrwt") %>%
               add_xml_data(L310.GlobalTranTechSCurve_tmp, "GlobalTranTechSCurve") %>%
               # BUILDINGS
               add_xml_data(L310.SubsectorShrwt_tmp, "SubsectorShrwt") %>%
               add_logit_tables_xml(L310.SubsectorLogit_bld_tmp, "SubsectorLogit") %>%
               add_xml_data(L310.FuelPrefElast_bld_tmp, "FuelPrefElast") %>%
               add_xml_data(L310.StubTech_bld_tmp, "StubTech") %>%
               add_xml_data(L310.StubTechEff_bld_tmp, "StubTechEff") %>%
               add_xml_data(L310.StubTechCalInputNoShrwt_bld_tmp, "StubTechCalInputNoShrwt") %>%
               add_xml_data(L310.StubTechIntGainOutputRatio_tmp, "StubTechIntGainOutputRatio") %>%
               add_xml_data(L310.GlobalTechShrwt_bld_tmp, "GlobalTechShrwt") %>%
               add_node_equiv_xml("input") %>%
               add_xml_data(L310.GlobalTechTrackCapital_bld_tmp, "GlobalTechTrackCapital") %>%
               add_xml_data(L310.GlobalTechCost_bld_tmp, "GlobalTechCost") %>%
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
                              "L310.StubTranTechCoef",
                              # BUILDINGS
                              "L310.GlobalTechShrwt_bld",
                              "L310.GlobalTechCost_bld",
                              "L310.GlobalTechTrackCapital_bld",
                              "L310.SubsectorShrwt",
                              "L310.SubsectorLogit_bld",
                              "L310.FuelPrefElast_bld",
                              "L310.StubTech_bld",
                              "L310.StubTechEff_bld",
                              "L310.StubTechCalInputNoShrwt_bld",
                              "L310.StubTechIntGainOutputRatio"))

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
