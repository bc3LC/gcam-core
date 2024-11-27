# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_ceilings_floors_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_ceilings_floors.xml}.
module_policy_ceilings_floors_xml <- function(command, ...) {
  all_xml_names <- union(get_xml_names("policy/A_energy_constraints.csv", "policy_ceilings_floors.xml"),
                         get_xml_names("policy/A_renewable_energy_standards.csv", "policy_ceilings_floors.xml"))
  names(all_xml_names) <- rep("XML", length(all_xml_names))

  if(command == driver.DECLARE_INPUTS) {
    return(c("L301.policy_port_stnd",
             "L301.policy_RES_coefs",
             "L301.RES_secout",
             "L301.pmultiplier",
             "L301.input_tax",
             "L301.input_subsidy",
             "L301.XML_policy_map",
             "L301.policy_RES_coefs_NG",
             "L301.RES_secout_NG",
             "L301.pmultiplier_NG",
             "L301.input_tax_NG",
             "L301.input_subsidy_NG"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]


    # Load required inputs
    L301.policy_RES_coefs <- get_data(all_data, "L301.policy_RES_coefs")
    L301.RES_secout <- get_data(all_data, "L301.RES_secout")
    L301.pmultiplier <-  get_data(all_data, "L301.pmultiplier")
    L301.input_tax <- get_data(all_data, "L301.input_tax")
    L301.input_subsidy <- get_data(all_data, "L301.input_subsidy")
    L301.policy_port_stnd <- get_data(all_data, "L301.policy_port_stnd")
    L301.XML_policy_map <- get_data(all_data, "L301.XML_policy_map")

    L301.policy_RES_coefs_NG <- get_data(all_data, "L301.policy_RES_coefs_NG")
    L301.RES_secout_NG <- get_data(all_data, "L301.RES_secout_NG")
    L301.pmultiplier_NG <-  get_data(all_data, "L301.pmultiplier_NG")
    L301.input_tax_NG <- get_data(all_data, "L301.input_tax_NG")
    L301.input_subsidy_NG <- get_data(all_data, "L301.input_subsidy_NG")

    # Match XML names in A_Policy_XML_Names to policies
    # If no xml listed for given region/market/policy, assign to policy_ceilings_floors.xml
    L301.policy_port_stnd_xml <- L301.policy_port_stnd %>%
      left_join(L301.XML_policy_map, by = c("policy.portfolio.standard", "market")) %>%
      replace_na(list(xml = "policy_ceilings_floors.xml"))

    # ===================================================

    for (xml_name in all_xml_names){
      L301.policy_port_stnd_tmp <- L301.policy_port_stnd_xml %>%
        filter(xml == xml_name)

      # Use as filter for other tables
      policy_rgn_tmp <- L301.policy_port_stnd_tmp %>%
        distinct(region, policy.portfolio.standard, policyType, xml)

      L301.policy_RES_coefs_tmp <- L301.policy_RES_coefs %>%
        semi_join(policy_rgn_tmp, by = c("region", "policyType", "minicam.energy.input" = "policy.portfolio.standard"))

      L301.RES_secout_tmp <- L301.RES_secout %>%
        semi_join(policy_rgn_tmp, by = c("region", "res.secondary.output" = "policy.portfolio.standard"))

      L301.pmultiplier_tmp <- L301.pmultiplier %>%
        semi_join(policy_rgn_tmp, by = c("region", "res.secondary.output" = "policy.portfolio.standard"))

      L301.input_tax_tmp <- L301.input_tax %>%
        semi_join(policy_rgn_tmp, by = c("region", "input.tax" = "policy.portfolio.standard"))

      L301.input_subsidy_tmp <- L301.input_subsidy %>%
        semi_join(policy_rgn_tmp, by = c("region", "input.subsidy" = "policy.portfolio.standard"))

      # Repeat for NG
      L301.policy_RES_coefs_NG_tmp <- L301.policy_RES_coefs_NG %>%
        semi_join(policy_rgn_tmp, by = c("region", "policyType",
                                         "minicam.energy.input" = "policy.portfolio.standard")) %>%
        rename(stub.technology = technology)

      L301.RES_secout_NG_tmp <- L301.RES_secout_NG %>%
        semi_join(policy_rgn_tmp, by = c("region",
                                         "res.secondary.output" = "policy.portfolio.standard")) %>%
        rename(stub.technology = technology)

      L301.pmultiplier_NG_tmp <- L301.pmultiplier_NG %>%
        semi_join(policy_rgn_tmp, by = c("region",
                                         "res.secondary.output" = "policy.portfolio.standard"))%>%
        rename(stub.technology = technology)

      L301.input_tax_NG_tmp <- L301.input_tax_NG %>%
        semi_join(policy_rgn_tmp, by = c("region", "input.tax" = "policy.portfolio.standard"))%>%
        rename(stub.technology = technology)

      L301.input_subsidy_NG_tmp <- L301.input_subsidy_NG %>%
        semi_join(policy_rgn_tmp, by = c("region", "input.subsidy" = "policy.portfolio.standard"))%>%
        rename(stub.technology = technology)

      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L301.policy_port_stnd_tmp, "PortfolioStdConstraint") %>%
               add_xml_data(L301.policy_RES_coefs_tmp, "StubTechCoef_NM_Policy") %>%
               add_xml_data_generate_levels(L301.policy_RES_coefs_NG_tmp,
                                            "StubTechCoef_NM_Policy","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(L301.RES_secout_tmp, "StubTechResSecOut") %>%
               add_xml_data_generate_levels(L301.RES_secout_NG_tmp,
                                            "StubTechResSecOut","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(L301.input_tax_tmp, "StubTechInputTax") %>%
               add_xml_data_generate_levels(L301.input_tax_NG_tmp,
                                            "StubTechInputTax","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(L301.input_subsidy_tmp, "StubTechInputSubsidy") %>%
               add_xml_data_generate_levels(L301.input_subsidy_NG_tmp,
                                            "StubTechInputSubsidy","subsector","nesting-subsector",1,FALSE) %>%
               add_xml_data(L301.pmultiplier_tmp, "StubTechResSecOutPMult") %>%
               add_xml_data_generate_levels(L301.pmultiplier_NG_tmp,
                                            "StubTechResSecOutPMult","subsector","nesting-subsector",1,FALSE) %>%
               add_precursors("L301.policy_port_stnd",
                              "L301.policy_RES_coefs",
                              "L301.RES_secout",
                              "L301.pmultiplier",
                              "L301.input_tax",
                              "L301.input_subsidy",
                              "L301.policy_RES_coefs_NG",
                              "L301.RES_secout_NG",
                              "L301.pmultiplier_NG",
                              "L301.input_tax_NG",
                              "L301.input_subsidy_NG")
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
