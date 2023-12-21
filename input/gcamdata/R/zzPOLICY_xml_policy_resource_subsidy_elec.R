# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_resource_subsidy_elec_xml
#'
#' Construct XML data structure for \code{resource_subsidy.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{aeei.xml}.
module_policy_resource_subsidy_elec_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_resource_subsidy_elec.csv", "resource_subsidy_elec.xml")
  AUTOMATED_OUTPUTS <-  gsub("L223.|L2233.", "L3101.", inputs_of("module_water_electricity_water_xml"))
  INPUTS <- c("L3101.RenewRsrc",
              "L3101.RenewRsrcPrice",
              "L3101.SmthRenewRsrcCurves",
              "L3101.ResTechShrwt",
              "L3101.StubTechCoef_elec_cool",
              AUTOMATED_OUTPUTS)
  if(command == driver.DECLARE_INPUTS) {
    return(INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    get_data_list(all_data, data_list = INPUTS)

    L3101.GlobalIntTechEff_elec_cool      <- rename(L3101.GlobalIntTechEff_elec_cool, `intermittent.technology` = technology)
    L3101.GlobalIntTechLifetime_elec_cool <- rename(L3101.GlobalIntTechLifetime_elec_cool, `intermittent.technology` = technology )
    L3101.GlobalIntTechShrwt_elec_cool    <- rename(L3101.GlobalIntTechShrwt_elec_cool,  `intermittent.technology` = technology )
    L3101.GlobalIntTechCapFac_elec_cool   <- rename(L3101.GlobalIntTechCapFac_elec_cool,  `intermittent.technology` = technology )


    # ===================================================

    # Function to filter based on xml name
    filter_xml_name <- function(df, xml_name){
      if ("xml" %in% names(df)){
        return(df %>%
                 filter(xml == xml_name) %>%
                 select(-xml))
      } else {
        return(df)
      }
    }

    # Produce outputs
    for (xml_name in all_xml_names){
      for(input_name in INPUTS){
        assign(paste0(input_name, "_tmp"),
               filter_xml_name(get(input_name), xml_name)
               )
      }

      # Produce output-------------

     create_xml(xml_name) %>%
        add_xml_data(L3101.RenewRsrc_tmp, "RenewRsrc") %>%
        add_xml_data(L3101.RenewRsrcPrice_tmp, "RenewRsrcPrice") %>%
        add_xml_data(L3101.SmthRenewRsrcCurves_tmp, "SmthRenewRsrcCurves") %>%
        add_xml_data(L3101.ResTechShrwt_tmp, "RenewResTechShrwt") %>%
        add_xml_data(L3101.StubTechCoef_elec_cool_tmp, "StubTechCoef") %>%
       add_node_equiv_xml("sector") %>%
       add_node_equiv_xml("technology") %>%
       add_logit_tables_xml(L3101.Supplysector_elec_tmp, "Supplysector") %>%
       add_xml_data(L3101.SubsectorShrwtFllt_elec_tmp, "SubsectorShrwtFllt") %>%
       add_xml_data(L3101.ElecReserve_tmp, "ElecReserve") %>%
       add_xml_data(L3101.SectorUseTrialMarket_elec_tmp, "SectorUseTrialMarket") %>%
       add_xml_data(L3101.StubTechCapFactor_elec_tmp, "StubTechCapFactor") %>%
       add_xml_data(L3101.SubsectorInterp_elec_tmp, "SubsectorInterp") %>%
       add_xml_data(L3101.SubsectorInterpTo_elec_tmp, "SubsectorInterpTo") %>%
       add_logit_tables_xml(L3101.SubsectorLogit_elec_tmp, "SubsectorLogit") %>%
       add_xml_data(L3101.SubsectorShrwt_coal_tmp, "SubsectorShrwt") %>%
       add_xml_data(L3101.SubsectorShrwt_nuc_tmp, "SubsectorShrwt") %>%
       add_xml_data(L3101.SubsectorShrwt_renew_tmp, "SubsectorShrwt") %>%
       add_xml_data(L3101.AvgFossilEffKeyword_elec_cool_tmp, "AvgFossilEffKeyword") %>%
       add_xml_data(L3101.GlobalIntTechBackup_elec_cool_tmp, "GlobalIntTechBackup") %>%
       add_xml_data(L3101.GlobalIntTechCapFac_elec_cool_tmp, "GlobalIntTechCapFac") %>%
       add_xml_data(L3101.GlobalIntTechEff_elec_cool_tmp, "GlobalIntTechEff") %>%
       add_xml_data(L3101.GlobalIntTechLifetime_elec_cool_tmp, "GlobalIntTechLifetime") %>%
       add_xml_data(L3101.GlobalIntTechShrwt_elec_cool_tmp, "GlobalIntTechShrwt") %>%
       add_xml_data(L3101.GlobalTechCapFac_elec_cool_tmp, "GlobalTechCapFac") %>%
       add_xml_data(L3101.GlobalTechCapture_elec_cool_tmp, "GlobalTechCapture") %>%
       add_xml_data(L3101.GlobalTechEff_elec_cool_tmp, "GlobalTechEff") %>%
       add_xml_data(L3101.GlobalTechLifetime_elec_cool_tmp, "GlobalTechLifetime") %>%
       add_xml_data(L3101.GlobalTechProfitShutdown_elec_cool_tmp, "GlobalTechProfitShutdown") %>%
       add_xml_data(L3101.GlobalTechSCurve_elec_cool_tmp, "GlobalTechSCurve") %>%
       add_xml_data(L3101.GlobalTechShrwt_elec_cool_tmp, "GlobalTechShrwt") %>%
       add_xml_data(L3101.PrimaryRenewKeyword_elec_cool_tmp, "PrimaryRenewKeyword") %>%
       add_xml_data(L3101.PrimaryRenewKeywordInt_elec_cool_tmp, "PrimaryRenewKeywordInt") %>%
       add_xml_data(L3101.StubTech_elecPassthru_tmp, "StubTech") %>%
       add_xml_data(L3101.StubTechProd_elecPassthru_tmp, "StubTechProd") %>%

       add_xml_data(L3101.StubTechCapital_elecPassthru_tmp, "StubTechCapital") %>%
       add_xml_data(L3101.StubTechOMfixed_elecPassthru_tmp, "StubTechOMfixed") %>%
       add_xml_data(L3101.StubTechOMvar_elecPassthru_tmp, "StubTechOMvar") %>%

       add_xml_data(L3101.GlobalPassThroughTech_tmp, "GlobalPassThroughTech") %>%
       add_xml_data(L3101.GlobalTechEff_elecPassthru_tmp, "GlobalTechEff") %>%
       add_xml_data(L3101.GlobalTechShrwt_elecPassthru_tmp, "GlobalTechShrwt") %>%
       add_xml_data(L3101.GlobalIntTechCapital_elec_tmp, "GlobalIntTechCapital", "GlobalTechCapital") %>%
       add_xml_data(L3101.GlobalTechCapital_elecPassthru_tmp, "GlobalTechCapital") %>%
       add_xml_data(L3101.GlobalIntTechOMfixed_elec_tmp, "GlobalIntTechOMfixed", "GlobalTechOMfixed") %>%
       add_xml_data(L3101.GlobalTechOMfixed_elecPassthru_tmp, "GlobalTechOMfixed") %>%
       add_xml_data(L3101.GlobalIntTechOMvar_elec_tmp, "GlobalIntTechOMvar", "GlobalTechOMvar") %>%
       add_xml_data(L3101.GlobalTechOMvar_elecPassthru_tmp, "GlobalTechOMvar") %>%
       add_xml_data(L3101.GlobalTechInterp_elecPassthru_tmp, "GlobalTechInterp") %>%
       add_xml_data(L3101.PassThroughSector_elec_cool_tmp, "PassThroughSector") %>%
       add_logit_tables_xml(L3101.Supplysector_elec_cool_tmp, "Supplysector") %>%
       add_xml_data(L3101.ElecReserve_elec_cool_tmp, "ElecReserve") %>%
       add_xml_data(L3101.SubsectorShrwtFllt_elec_cool_tmp, "SubsectorShrwtFllt") %>%
       add_logit_tables_xml(L3101.SubsectorLogit_elec_cool_tmp, "SubsectorLogit") %>%
       add_xml_data(L3101.StubTech_elec_cool_tmp, "StubTech") -> tmp

     if (energy.ELEC_COST_SOURCE == "ATB"){
       tmp <- tmp %>%
         add_xml_data(L3101.StubTechTrackCapital_elec_tmp, "StubTechTrackCapital") %>%
         add_xml_data(L3101.StubTechTrackCapital_elec_tmp, "StubTechCost")
     }

     tmp  %>%
       add_xml_data(L3101.StubTechEff_elec_cool_tmp, "StubTechEff") %>%
       add_xml_data(L3101.StubTechSecOut_desal_elec_cool_tmp, "StubTechSecOut") %>%
       add_xml_data(L3101.StubTechProd_elec_cool_tmp, "StubTechProd") %>%
       add_xml_data(L3101.StubTechCapFactor_elec_cool_tmp, "StubTechCapFactor") %>%
       add_xml_data(L3101.StubTechFixOut_hydro_tmp, "StubTechFixOut") %>%
       add_xml_data(L3101.StubTechShrwt_elec_cool_tmp, "StubTechShrwt") %>%
       add_xml_data(L3101.GlobalTechCapital_elec_cool_tmp, "GlobalTechCapital") %>%
       add_xml_data(L3101.GlobalIntTechCapital_elec_cool_tmp, "GlobalIntTechCapital", "GlobalTechCapital") %>%
       add_xml_data(L3101.GlobalTechCapFac_elec_tmp, "GlobalTechCapFac") %>%
       add_precursors(INPUTS) -> xml

             assign(xml_name, xml)

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
