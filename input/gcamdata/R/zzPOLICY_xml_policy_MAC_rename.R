# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_MAC_rename_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_MAC_rename.xml}.
module_policy_MAC_rename_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_MAC_rename.csv", "policy_MAC_rename.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(c("L306.ResMAC_fos",
             "L306.MAC_higwp",
             "L306.AgMAC",
             "L306.MAC_an",
             "L306.ResMAC_fos_tc_average",
             "L306.ResMAC_fos_phaseInTime",
             "L306.MAC_higwp_tc_average",
             "L306.MAC_higwp_phaseInTime"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L306.ResMAC_fos <- get_data(all_data, "L306.ResMAC_fos")
    L306.MAC_higwp <- get_data(all_data, "L306.MAC_higwp")
    L306.AgMAC <- get_data(all_data, "L306.AgMAC")
    L306.MAC_an <- get_data(all_data, "L306.MAC_an")
    L306.ResMAC_fos_tc_average <- get_data(all_data, "L306.ResMAC_fos_tc_average")
    L306.ResMAC_fos_phaseInTime <- get_data(all_data, "L306.ResMAC_fos_phaseInTime")
    L306.MAC_higwp_tc_average <- get_data(all_data, "L306.MAC_higwp_tc_average")
    L306.MAC_higwp_phaseInTime <- get_data(all_data, "L306.MAC_higwp_phaseInTime")

    # ===================================================

    # Produce outputs
    for (xml_name in all_xml_names){
      L306.ResMAC_fos_tmp <- L306.ResMAC_fos %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L306.MAC_higwp_tmp <- L306.MAC_higwp %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L306.AgMAC_tmp <- L306.AgMAC %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L306.MAC_an_tmp <- L306.MAC_an %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L306.ResMAC_fos_tc_average_tmp <- L306.ResMAC_fos_tc_average %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L306.ResMAC_fos_phaseInTime_tmp <- L306.ResMAC_fos_phaseInTime %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L306.MAC_higwp_tc_average_tmp <- L306.MAC_higwp_tc_average %>%
        filter(xml == xml_name) %>%
        select(-xml)

      L306.MAC_higwp_phaseInTime_tmp <- L306.MAC_higwp_phaseInTime %>%
        filter(xml == xml_name) %>%
        select(-xml)

        assign(xml_name,
               create_xml(xml_name) %>%
                 add_xml_data(L306.ResMAC_fos_tmp, "ResMAC") %>%
                 add_xml_data(L306.MAC_higwp_tmp, "MAC") %>%
                 add_xml_data(L306.AgMAC_tmp, "AgMAC") %>%
                 add_xml_data(L306.MAC_an_tmp, "MAC") %>%
                 add_xml_data(L306.ResMAC_fos_tc_average_tmp, "ResMACTC") %>%
                 add_xml_data(L306.ResMAC_fos_phaseInTime_tmp, "ResMACPhaseIn") %>%
                 add_xml_data(L306.MAC_higwp_tc_average_tmp, "MACTC") %>%
                 add_xml_data(L306.MAC_higwp_phaseInTime_tmp, "MACPhaseIn") %>%
                 add_precursors("L306.ResMAC_fos",
                                "L306.MAC_higwp",
                                "L306.AgMAC",
                                "L306.MAC_an",
                                "L306.ResMAC_fos_tc_average",
                                "L306.ResMAC_fos_phaseInTime",
                                "L306.MAC_higwp_tc_average",
                                "L306.MAC_higwp_phaseInTime")
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
