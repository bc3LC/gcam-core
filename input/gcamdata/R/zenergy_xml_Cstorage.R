# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_Cstorage.xml
#'
#' Construct XML data structure for \code{Cstorage.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{Cstorage.xml}. The corresponding file in the
#' original data system was \code{batch_Cstorage.xml.R} (energy XML).
module_energy_Cstorage_xml <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c("L261.Rsrc",
              "L261.UnlimitRsrc",
              "L261.RsrcCurves_C",
              "L261.ResTechShrwt_C",
              "L261.Supplysector_C",
              "L261.SubsectorLogit_C",
              "L261.SubsectorShrwtFllt_C",
              "L261.StubTech_C",
              "L261.GlobalTechCoef_C",
              "L261.GlobalTechCost_C",
              "L261.GlobalTechShrwt_C",
              "L261.ResSubresourceProdLifetime",
              "L261.ResReserveTechLifetime",
              "L261.ResReserveTechDeclinePhase",
              "L261.ResReserveTechProfitShutdown",
              "L261.ResReserveTechInvestmentInput",
              "L261.CStorageCurvesDynamic",
              "L261.DynamicCstorageRsrcMax",
              "L261.DynamicRsrc",
              "L261.DynamicResTechShrwt_C",
              "L261.RsrcPrice",
              "L261.StubTechShrwt",
              "L261.StubTechEff",
              "L261.TechPmult",
              "L261.DeleteStubTech"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c(XML = "Cstorage.xml",
             XML = "Cstorage_slow.xml",
             XML = "Cstorage_med.xml",
             XML = "Cstorage_rapid.xml"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L261.Rsrc <- get_data(all_data, "L261.Rsrc")
    L261.UnlimitRsrc <- get_data(all_data, "L261.UnlimitRsrc")
    L261.RsrcCurves_C <- get_data(all_data, "L261.RsrcCurves_C")
    L261.ResTechShrwt_C <- get_data(all_data, "L261.ResTechShrwt_C")
    L261.Supplysector_C <- get_data(all_data, "L261.Supplysector_C")
    L261.SubsectorLogit_C <- get_data(all_data, "L261.SubsectorLogit_C")
    L261.SubsectorShrwtFllt_C <- get_data(all_data, "L261.SubsectorShrwtFllt_C")
    L261.StubTech_C <- get_data(all_data, "L261.StubTech_C")
    L261.GlobalTechCoef_C <- get_data(all_data, "L261.GlobalTechCoef_C")
    L261.GlobalTechCost_C <- get_data(all_data, "L261.GlobalTechCost_C")
    L261.GlobalTechShrwt_C <- get_data(all_data, "L261.GlobalTechShrwt_C")
    L261.ResSubresourceProdLifetime <- get_data(all_data, "L261.ResSubresourceProdLifetime")
    L261.ResReserveTechLifetime <- get_data(all_data, "L261.ResReserveTechLifetime")
    L261.ResReserveTechDeclinePhase <- get_data(all_data, "L261.ResReserveTechDeclinePhase")
    L261.ResReserveTechProfitShutdown <- get_data(all_data, "L261.ResReserveTechProfitShutdown")
    L261.ResReserveTechInvestmentInput <- get_data(all_data, "L261.ResReserveTechInvestmentInput")
    L261.CStorageCurvesDynamic <- get_data(all_data, "L261.CStorageCurvesDynamic")
    L261.DynamicCstorageRsrcMax <- get_data(all_data,"L261.DynamicCstorageRsrcMax")
    L261.DynamicRsrc <- get_data(all_data,"L261.DynamicRsrc")
    L261.DynamicResTechShrwt_C <- get_data(all_data,"L261.DynamicResTechShrwt_C")
    L261.RsrcPrice <- get_data(all_data,"L261.RsrcPrice")
    L261.StubTechShrwt <- get_data(all_data,"L261.StubTechShrwt")
    L261.StubTechEff <- get_data(all_data,"L261.StubTechEff")
    L261.TechPmult <- get_data(all_data,"L261.TechPmult")
    L261.DeleteStubTech <- get_data(all_data, "L261.DeleteStubTech")

    # ===================================================


    create_xml("Cstorage.xml") %>%
      add_xml_data(L261.Rsrc, "Rsrc") %>%
      add_xml_data(L261.UnlimitRsrc, "UnlimitRsrc") %>%
      #add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L261.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
      add_xml_data(L261.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L261.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L261.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
      add_xml_data(L261.ResReserveTechInvestmentInput, "ResReserveTechInvestmentInput") %>%
      add_xml_data(L261.RsrcCurves_C, "RsrcCurves") %>%
      add_xml_data(L261.ResTechShrwt_C, "ResTechShrwt") %>%
      add_logit_tables_xml(L261.Supplysector_C, "Supplysector") %>%
      add_logit_tables_xml(L261.SubsectorLogit_C, "SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C, "SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C, "StubTech") %>%
      add_xml_data(L261.GlobalTechCoef_C, "GlobalTechCoef") %>%
      add_xml_data(L261.GlobalTechCost_C, "GlobalTechCost") %>%
      add_xml_data(L261.GlobalTechShrwt_C, "GlobalTechShrwt") %>%
      add_xml_data(L261.StubTechShrwt, "StubTechShrwt") %>%
      add_xml_data(L261.DeleteStubTech, "DeleteStubTech") %>%
      add_precursors("L261.Rsrc", "L261.UnlimitRsrc", "L261.RsrcCurves_C", "L261.ResTechShrwt_C", "L261.Supplysector_C", "L261.SubsectorLogit_C", "L261.SubsectorShrwtFllt_C", "L261.StubTech_C", "L261.GlobalTechCoef_C", "L261.GlobalTechCost_C", "L261.GlobalTechShrwt_C",
                     "L261.ResSubresourceProdLifetime","L261.ResReserveTechLifetime","L261.ResReserveTechDeclinePhase","L261.ResReserveTechProfitShutdown","L261.ResReserveTechInvestmentInput",
                     "L261.StubTechShrwt","L261.DeleteStubTech") ->
      Cstorage.xml


    # Produce outputs
    create_xml("Cstorage_slow.xml") %>%
      add_xml_data(L261.Rsrc, "Rsrc") %>%
      add_xml_data(L261.UnlimitRsrc, "UnlimitRsrc") %>%
      add_xml_data(L261.DynamicRsrc, "RenewRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L261.CStorageCurvesDynamic %>% filter(scenario == 'slow growth rate'), "GrdRenewRsrcCurves") %>%
      add_xml_data(L261.DynamicCstorageRsrcMax, "GrdRenewRsrcMax") %>%
      add_xml_data(L261.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
      add_xml_data(L261.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L261.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L261.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
      add_xml_data(L261.ResReserveTechInvestmentInput, "ResReserveTechInvestmentInput") %>%
      #add_xml_data(L261.RsrcCurves_C, "RsrcCurves") %>%
      #add_xml_data(L261.ResTechShrwt_C, "ResTechShrwt") %>%
      add_xml_data(L261.DynamicResTechShrwt_C, "ResTechShrwt") %>%
      add_logit_tables_xml(L261.Supplysector_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "Supplysector") %>%
      add_logit_tables_xml(L261.SubsectorLogit_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "StubTech") %>%
      add_xml_data(L261.GlobalTechCoef_C, "GlobalTechCoef") %>%
      add_xml_data(L261.GlobalTechCost_C, "GlobalTechCost") %>%
      add_xml_data(L261.GlobalTechShrwt_C, "GlobalTechShrwt") %>%
      add_xml_data(L261.RsrcPrice, "RenewRsrcPrice") %>%
      add_xml_data(L261.StubTechEff %>%
                     filter(scenario == 'slow growth rate'), "StubTechEff") %>%
      add_xml_data(L261.TechPmult %>%
                     filter(scenario == 'slow growth rate'), "TechPmult") %>%
      add_xml_data(L261.StubTechShrwt, "StubTechShrwt") %>%
      add_xml_data(L261.DeleteStubTech, "DeleteStubTech") %>%
      add_precursors("L261.Rsrc", "L261.UnlimitRsrc", "L261.RsrcCurves_C", "L261.ResTechShrwt_C", "L261.Supplysector_C", "L261.SubsectorLogit_C", "L261.SubsectorShrwtFllt_C", "L261.StubTech_C", "L261.GlobalTechCoef_C", "L261.GlobalTechCost_C", "L261.GlobalTechShrwt_C",
                     "L261.ResSubresourceProdLifetime","L261.ResReserveTechLifetime","L261.ResReserveTechDeclinePhase","L261.ResReserveTechProfitShutdown",
                     "L261.ResReserveTechInvestmentInput",
                     "L261.CStorageCurvesDynamic","L261.DynamicCstorageRsrcMax","L261.DynamicRsrc","L261.DynamicResTechShrwt_C","L261.RsrcPrice",
                     "L261.StubTechEff","L261.TechPmult") ->
      Cstorage_slow.xml

    create_xml("Cstorage_med.xml") %>%
      add_xml_data(L261.Rsrc, "Rsrc") %>%
      add_xml_data(L261.UnlimitRsrc, "UnlimitRsrc") %>%
      add_xml_data(L261.DynamicRsrc, "RenewRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L261.CStorageCurvesDynamic %>% filter(scenario == 'medium growth rate'), "GrdRenewRsrcCurves") %>%
      add_xml_data(L261.DynamicCstorageRsrcMax, "GrdRenewRsrcMax") %>%
      add_xml_data(L261.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
      add_xml_data(L261.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L261.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L261.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
      add_xml_data(L261.ResReserveTechInvestmentInput, "ResReserveTechInvestmentInput") %>%
      #add_xml_data(L261.RsrcCurves_C, "RsrcCurves") %>%
      #add_xml_data(L261.ResTechShrwt_C, "ResTechShrwt") %>%
      add_xml_data(L261.DynamicResTechShrwt_C, "ResTechShrwt") %>%
      add_logit_tables_xml(L261.Supplysector_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "Supplysector") %>%
      add_logit_tables_xml(L261.SubsectorLogit_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "StubTech") %>%
      add_xml_data(L261.GlobalTechCoef_C, "GlobalTechCoef") %>%
      add_xml_data(L261.GlobalTechCost_C, "GlobalTechCost") %>%
      add_xml_data(L261.GlobalTechShrwt_C, "GlobalTechShrwt") %>%
      add_xml_data(L261.RsrcPrice, "RenewRsrcPrice") %>%
      add_xml_data(L261.StubTechEff %>%
                     filter(scenario == 'medium growth rate'), "StubTechEff") %>%
      add_xml_data(L261.TechPmult %>%
                     filter(scenario == 'medium growth rate'), "TechPmult") %>%
      add_xml_data(L261.StubTechShrwt, "StubTechShrwt") %>%
      add_xml_data(L261.DeleteStubTech, "DeleteStubTech") %>%
      add_precursors("L261.Rsrc", "L261.UnlimitRsrc", "L261.RsrcCurves_C", "L261.ResTechShrwt_C", "L261.Supplysector_C", "L261.SubsectorLogit_C", "L261.SubsectorShrwtFllt_C", "L261.StubTech_C", "L261.GlobalTechCoef_C", "L261.GlobalTechCost_C", "L261.GlobalTechShrwt_C",
                     "L261.ResSubresourceProdLifetime","L261.ResReserveTechLifetime","L261.ResReserveTechDeclinePhase","L261.ResReserveTechProfitShutdown",
                     "L261.ResReserveTechInvestmentInput",
                     "L261.CStorageCurvesDynamic","L261.DynamicCstorageRsrcMax","L261.DynamicRsrc","L261.DynamicResTechShrwt_C","L261.RsrcPrice",
                     "L261.StubTechEff","L261.TechPmult") ->
      Cstorage_med.xml


    create_xml("Cstorage_rapid.xml") %>%
      add_xml_data(L261.Rsrc, "Rsrc") %>%
      add_xml_data(L261.UnlimitRsrc, "UnlimitRsrc") %>%
      add_xml_data(L261.DynamicRsrc, "RenewRsrc") %>%
      add_node_equiv_xml("resource") %>%
      add_node_equiv_xml("subresource") %>%
      add_node_equiv_xml("technology") %>%
      add_xml_data(L261.CStorageCurvesDynamic %>% filter(scenario == 'rapid growth rate'), "GrdRenewRsrcCurves") %>%
      add_xml_data(L261.DynamicCstorageRsrcMax, "GrdRenewRsrcMax") %>%
      add_xml_data(L261.ResSubresourceProdLifetime, "ResSubresourceProdLifetime") %>%
      add_xml_data(L261.ResReserveTechDeclinePhase, "ResReserveTechDeclinePhase") %>%
      add_xml_data(L261.ResReserveTechProfitShutdown, "ResReserveTechProfitShutdown") %>%
      add_xml_data(L261.ResReserveTechLifetime, "ResReserveTechLifetime") %>%
      add_xml_data(L261.ResReserveTechInvestmentInput, "ResReserveTechInvestmentInput") %>%
      #add_xml_data(L261.RsrcCurves_C, "RsrcCurves") %>%
      #add_xml_data(L261.ResTechShrwt_C, "ResTechShrwt") %>%
      add_xml_data(L261.DynamicResTechShrwt_C, "ResTechShrwt") %>%
      add_logit_tables_xml(L261.Supplysector_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "Supplysector") %>%
      add_logit_tables_xml(L261.SubsectorLogit_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "SubsectorLogit") %>%
      add_xml_data(L261.SubsectorShrwtFllt_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "SubsectorShrwtFllt") %>%
      add_xml_data(L261.StubTech_C %>% filter(supplysector != energy.CO2.STORAGE.MARKET), "StubTech") %>%
      add_xml_data(L261.GlobalTechCoef_C, "GlobalTechCoef") %>%
      add_xml_data(L261.GlobalTechCost_C, "GlobalTechCost") %>%
      add_xml_data(L261.GlobalTechShrwt_C, "GlobalTechShrwt") %>%
      add_xml_data(L261.RsrcPrice, "RenewRsrcPrice") %>%
      add_xml_data(L261.StubTechEff %>%
                     filter(scenario == 'rapid growth rate'), "StubTechEff") %>%
      add_xml_data(L261.TechPmult %>%
                     filter(scenario == 'rapid growth rate'), "TechPmult") %>%
      add_xml_data(L261.StubTechShrwt, "StubTechShrwt") %>%
      add_xml_data(L261.DeleteStubTech, "DeleteStubTech") %>%
      add_precursors("L261.Rsrc", "L261.UnlimitRsrc", "L261.RsrcCurves_C", "L261.ResTechShrwt_C", "L261.Supplysector_C", "L261.SubsectorLogit_C", "L261.SubsectorShrwtFllt_C", "L261.StubTech_C", "L261.GlobalTechCoef_C", "L261.GlobalTechCost_C", "L261.GlobalTechShrwt_C",
                     "L261.ResSubresourceProdLifetime","L261.ResReserveTechLifetime","L261.ResReserveTechDeclinePhase","L261.ResReserveTechProfitShutdown",
                     "L261.ResReserveTechInvestmentInput",
                     "L261.CStorageCurvesDynamic","L261.DynamicCstorageRsrcMax","L261.DynamicRsrc","L261.DynamicResTechShrwt_C","L261.RsrcPrice",
                     "L261.StubTechEff","L261.TechPmult") ->
      Cstorage_rapid.xml

    return_data(Cstorage.xml,
                Cstorage_slow.xml,
                Cstorage_med.xml,
                Cstorage_rapid.xml)
  } else {
    stop("Unknown command")
  }
}
