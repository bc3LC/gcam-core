# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_FixedOutputTech_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_FixedOutputTech.xml}.
module_policy_FixedOutputTech_xml <- function(command, ...) {
  all_xml_names <- union(get_xml_names("policy/A_FixedOutputTech.csv", "policy_FixedOutputTech.xml"),
                         get_xml_names("policy/A_FixedOutputTranTech.csv", "policy_FixedOutputTech.xml"))
  names(all_xml_names) <- rep("XML", length(all_xml_names))

  if(command == driver.DECLARE_INPUTS) {
    return(c("L305.StubTechFixedOutput",
             "L305.StubTechLifetime",
             "L305.GlbTechFixedOutput",
             "L305.StubTranTechFixedOutput",
             "L305.GlobalTranTechInterp",
             "L305.GlobalTranTechShrwt",
             "L305.GlobalTranTechSCurve",
             "L305.StubTranTechLoadFactor",
             "L305.StubTranTechCost",
             "L305.StubTechTrackCapital",
             "L305.StubTranTechCalInput",
             "L305.StubTranTechCoef"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L305.StubTechFixedOutput <- get_data(all_data, "L305.StubTechFixedOutput")
    L305.StubTechLifetime <- get_data(all_data, "L305.StubTechLifetime")
    L305.GlbTechFixedOutput <- get_data(all_data, "L305.GlbTechFixedOutput")
    L305.StubTranTechFixedOutput  <- get_data(all_data, "L305.StubTranTechFixedOutput")
    L305.GlobalTranTechInterp <- get_data(all_data, "L305.GlobalTranTechInterp")
    L305.GlobalTranTechShrwt <- get_data(all_data, "L305.GlobalTranTechShrwt")
    L305.GlobalTranTechSCurve <- get_data(all_data, "L305.GlobalTranTechSCurve")
    L305.StubTranTechLoadFactor <- get_data(all_data, "L305.StubTranTechLoadFactor")
    L305.StubTranTechCost <- get_data(all_data, "L305.StubTranTechCost")
    L305.StubTechTrackCapital <- get_data(all_data, "L305.StubTechTrackCapital")
    L305.StubTranTechCalInput <- get_data(all_data, "L305.StubTranTechCalInput")
    L305.StubTranTechCoef <- get_data(all_data, "L305.StubTranTechCoef")

    L305.GlbTechShrwt <- L305.GlbTechFixedOutput %>%
      select(-minicam.non.energy.input, -input.cost)
    L305.GlbTechCost <- L305.GlbTechFixedOutput %>%
      select(-share.weight)


    # ===================================================

    # Produce outputs
    for (xml_name in all_xml_names){
      L305.StubTechFixedOutput_tmp <- L305.StubTechFixedOutput %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.StubTechLifetime_tmp <- L305.StubTechLifetime %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.GlbTechShrwt_tmp <- L305.GlbTechShrwt %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.GlbTechCost_tmp <- L305.GlbTechCost %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.StubTranTechFixedOutput_tmp <- L305.StubTranTechFixedOutput %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.GlobalTranTechInterp_tmp <- L305.GlobalTranTechInterp %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.GlobalTranTechShrwt_tmp <- L305.GlobalTranTechShrwt %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.GlobalTranTechSCurve_tmp <- L305.GlobalTranTechSCurve %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.StubTranTechLoadFactor_tmp <- L305.StubTranTechLoadFactor %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.StubTranTechCost_tmp <- L305.StubTranTechCost %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.StubTechTrackCapital_tmp <- L305.StubTechTrackCapital %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.StubTranTechCalInput_tmp <- L305.StubTranTechCalInput %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L305.StubTranTechCoef_tmp <- L305.StubTranTechCoef %>%
        filter(xml == xml_name) %>%
        select(-xml)

      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L305.StubTechLifetime_tmp, "StubTechLifetime") %>%
               add_xml_data(L305.StubTechFixedOutput_tmp, "StubTechFixOutNoSW") %>%
               add_xml_data(L305.GlbTechShrwt_tmp, "GlobalTechShrwt") %>%
               add_xml_data(L305.GlbTechCost_tmp,"GlobalTechCost") %>%
               # add_node_equiv_xml("technology") %>%
               # add_node_equiv_xml("input") %>%
               add_xml_data(L305.StubTranTechFixedOutput_tmp, "StubTranTechFixedOutput") %>%
               add_xml_data(L305.StubTranTechCalInput_tmp, "StubTranTechCalInput") %>%
               add_xml_data(L305.StubTranTechLoadFactor_tmp, "StubTranTechLoadFactor") %>%
               add_node_equiv_xml("subsector") %>%
               add_xml_data(L305.StubTechTrackCapital_tmp, "StubTechTrackCapital") %>%
               add_xml_data(L305.StubTranTechCost_tmp, "StubTranTechCost") %>%
               add_xml_data(L305.StubTranTechCoef_tmp, "StubTranTechCoef") %>%
               add_xml_data(L305.GlobalTranTechInterp_tmp, "GlobalTranTechInterp") %>%
               add_xml_data(L305.GlobalTranTechShrwt_tmp, "GlobalTranTechShrwt") %>%
               add_xml_data(L305.GlobalTranTechSCurve_tmp, "GlobalTranTechSCurve") %>%
               add_precursors("L305.StubTechFixedOutput",
                              "L305.GlbTechFixedOutput"))


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
