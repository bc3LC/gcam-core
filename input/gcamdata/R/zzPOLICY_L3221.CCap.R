# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3221.CCap
#'
#' Produce custom carbon cap targets and markets
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L3221.CCap_constraint}, \code{L3221.CCap_link_regions}, \code{L3221.CCap_tech}, \code{L3221.CCap_tranTech}, \code{L3221.CCap_resource}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3221.CCap <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "policy/A_CCap_Constraint",
             FILE = "policy/mappings/policy_tech_mappings",
             FILE = "policy/mappings/policy_tranSubsector_mappings",
             FILE = "policy/mappings/policy_resource_mappings",
             FILE = "policy/mappings/ghg_link",
             FILE = "policy/GCAM_results/CO2byTech",
             FILE = "policy/mappings/market_region_mappings",
             "L210.ResTechCoef",
             "L221.StubTech_en",
             "L222.StubTech_en",
             "L224.StubTech_heat",
             "L223.StubTech_elec",
             "L2233.StubTech_elec_cool",
             "L226.StubTech_en",
             "L232.StubTech_ind",
             "L2321.StubTech_cement",
             "L2322.StubTech_Fert",
             "L2323.StubTech_iron_steel",
             "L2324.StubTech_Off_road",
             "L2325.StubTech_chemical",
             "L2326.StubTech_aluminum",
             "L244.StubTech_bld",
             "L225.StubTech_h2",
             "L239.PrimaryConsKeyword_en",
             "L254.StubTranTech",
             "L201.GDP_Scen"
             ))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3221.CCap_constraint",
             "L3221.CCap_link_regions",
             "L3221.CCap_tech",
             "L3221.CCap_tranTech",
             "L3221.CCap_resource",
             "L3221.CCap_GHG_Link",
             "L3221.CCap_global_tech"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs -------------
    A_CCap_Constraint <- get_data(all_data, "policy/A_CCap_Constraint") %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))
    policy_tech_mappings <- get_data(all_data, "policy/mappings/policy_tech_mappings")
    policy_tranSubsector_mappings <- get_data(all_data, "policy/mappings/policy_tranSubsector_mappings")
    policy_resource_mappings <- get_data(all_data, "policy/mappings/policy_resource_mappings")
    policy_mappings <- bind_rows(policy_tech_mappings,
                                 policy_tranSubsector_mappings,
                                 policy_resource_mappings)
    market_region_mappings <- get_data(all_data, "policy/mappings/market_region_mappings")
    ghg_link <- get_data(all_data, "policy/mappings/ghg_link")


    L3221.StubTech_All <- bind_rows(get_data(all_data, "L221.StubTech_en"),
                                    get_data(all_data, "L222.StubTech_en"),
                                    get_data(all_data, "L223.StubTech_elec"),
                                    get_data(all_data, "L2233.StubTech_elec_cool"),
                                    get_data(all_data, "L224.StubTech_heat"),
                                    get_data(all_data, "L225.StubTech_h2"),
                                    get_data(all_data, "L226.StubTech_en"),
                                    get_data(all_data, "L232.StubTech_ind"),
                                    get_data(all_data, "L2321.StubTech_cement"),
                                    get_data(all_data, "L2322.StubTech_Fert"),
                                    get_data(all_data, "L2323.StubTech_iron_steel"),
                                    get_data(all_data, "L2324.StubTech_Off_road"),
                                    get_data(all_data, "L2325.StubTech_chemical"),
                                    get_data(all_data, "L2326.StubTech_aluminum"),
                                    get_data(all_data, "L244.StubTech_bld"),
                                    get_data(all_data, "L239.PrimaryConsKeyword_en") %>%
                                      select(-primary.consumption, -year) %>%
                                      distinct() %>%
                                      rename(stub.technology = technology))

    L254.StubTranTech <- get_data(all_data, "L254.StubTranTech") %>%
                                      filter(sce == "CORE") %>%
                                      select(-sce)
    L210.ResTech <- get_data(all_data, "L210.ResTechCoef") %>%
      distinct(region, resource, reserve.subresource, resource.reserve.technology)

    L201.GDP_Scen <- get_data(all_data, "L201.GDP_Scen")
    CO2byTech <- get_data(all_data, "policy/GCAM_results/CO2byTech") %>%
      gather_years()

    # 1. Write constraint to correct regions ----------------------------
    # When a constraint applies to more than one region, we don't need to write the
    # constraint to all regions, can just pass the ghg policy forward to the "linked" regions

    L3221.CCap_constraint <- A_CCap_Constraint %>%
      gather_years() %>%
      # Assumes that linked regions have no constraint listed
      filter(!is.na(value)) %>%
      rename(constraint.year = year, constraint = value) %>%
      left_join_keep_first_only(market_region_mappings, by = "market") %>%
      mutate(region = if_else(!is.na(region), region, market))

    L3221.CCap_link_regions <- L3221.CCap_constraint %>%
      filter(market != "World") %>%
      # first filter to needed markets
      semi_join(market_region_mappings, by = "market") %>%
      # then add in all regions
      left_join(market_region_mappings, by = "market") %>%
      # filter out region where constraint is held
      filter(region.x != region.y) %>%
      distinct(xml, region = region.y, market, ghgpolicy, mapping.name)

    # 2. Add custom CO2 market to stub technologies ----------------------------
    # Note that we remove all biomass technologies since they shouldn't contribute to
    # CO2 constraints in most cases
    # For technologies, we need all regions
    tech_all_regions <- bind_rows(L3221.CCap_constraint, L3221.CCap_link_regions)  %>%
      distinct(xml, market, region, ghgpolicy, mapping.name) %>%
      filter(market != "World")

    # Need to add all techs for the given regions/sectors
    # First for non-transport techs, then for transport techs, then for resources
    L3221.CCap_tech <- policy_mappings %>%
      filter(is.na(tranSubsector), is.na(resource),
             !grepl("^trn_", supplysector)) %>%
      select(mapping.name = tech_mapping, supplysector, subsector, stub.technology) %>%
      right_join(tech_all_regions, by = c("mapping.name")) %>%
      left_join(L3221.StubTech_All, by = c("supplysector", "subsector", "stub.technology", "region")) %>%
      # # Remove bio techs
      # filter(!grepl("bio", subsector, ignore.case = T),
      #        !grepl("bio", stub.technology, ignore.case = T)) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))

    L3221.CCap_tech_NAs <- L3221.CCap_tech %>%
      filter(is.na(stub.technology)) %>%
      distinct(xml, CO2, region, supplysector)

    # If there are NAs, we need to remove them, but here we print a message with any NA sectors
    if (nrow(L3221.CCap_tech_NAs) > 0){
      print("Constraint removed for the following sectors:")
      for(i in 1:nrow(L3221.CCap_tech_NAs)){
        print(paste(L3221.CCap_tech_NAs[i,], collapse = "---"))
      }
    }

    L3221.CCap_tech <- filter(L3221.CCap_tech, !is.na(stub.technology))

    # Shouldn't have any NAs left
    stopifnot(!any(is.na(L3221.CCap_tech)))

    # 3. Add custom CO2 market to transportation technologies ----------------------------
    L3221.CCap_tranTech_pre <- policy_mappings %>%
      filter(!is.na(tranSubsector) | grepl("^trn_", supplysector)) %>%
      select(mapping.name, supplysector, tranSubsector) %>%
      left_join(tech_all_regions, by = c("mapping.name")) %>%
      filter(!is.na(region))

    if (nrow(L3221.CCap_tranTech_pre) == 0){
      # If no caps to implement, create empty tibble
      tbl_colnames <- c("xml", LEVEL2_DATA_NAMES[["StubTranTechCO2"]])
      L3221.CCap_tranTech <- tibble::tibble(!!!tbl_colnames, .rows = 0, .name_repair = ~ tbl_colnames)

    } else {
      L3221.CCap_tranTech_sector <- L3221.CCap_tranTech_pre %>%
        filter(is.na(tranSubsector)) %>%
        select(-tranSubsector) %>%
        left_join(L254.StubTranTech, by = c("supplysector", "region"))

      L3221.CCap_tranTech <-  L3221.CCap_tranTech_pre %>%
        filter(!is.na(tranSubsector)) %>%
        left_join(L254.StubTranTech, by = c("supplysector", "region", "tranSubsector")) %>%
        bind_rows(L3221.CCap_tranTech_sector) %>%
        rename(CO2 = ghgpolicy) %>%
        select(-market) %>%
        repeat_add_columns(tibble(year = MODEL_YEARS))

      # Check that there are no NAs
      stopifnot(!any(is.na(L3221.CCap_tranTech)))
    }


    # 4. Add custom CO2 market to resource technologies ------------------------------
    L3221.CCap_resource <- policy_mappings %>%
      filter(!is.na(resource)) %>%
      select(-supplysector, -tranSubsector) %>%
      left_join(tech_all_regions, by = c("mapping.name")) %>%
      filter(!is.na(region)) %>%
      left_join(L210.ResTech, by = c("resource", "reserve.subresource", "region")) %>%
      rename(CO2 = ghgpolicy) %>%
      select(-market)

    if (nrow(L3221.CCap_resource) > 0){
      L3221.CCap_resource <- L3221.CCap_resource %>% repeat_add_columns(tibble(year = MODEL_YEARS))
    } else {
      L3221.CCap_resource <- L3221.CCap_resource %>% repeat_add_columns(tibble(year = numeric()))

      }

    # Check that there are no NAs
    stopifnot(!any(is.na(L3221.CCap_resource)))

    # 5 Global tech db technologies ---------------
    # For technologies, we need all regions
    tech_global_db <- L3221.CCap_constraint  %>%
      distinct(xml, market, ghgpolicy, mapping.name) %>%
      filter(market == "World")

    if (nrow(tech_global_db) == 0){
      # If no caps to implement, create empty tibble
      tbl_colnames <- c("xml", LEVEL2_DATA_NAMES[["GlobalTechCO2"]])
      L3221.CCap_global_tech <- tibble::tibble(!!!tbl_colnames, .rows = 0, .name_repair = ~ tbl_colnames)
    } else {
    # Need to add all techs for the given regions/sectors
    # First for non-transport techs, then for transport techs, then for resources
    L3221.CCap_global_tech <- policy_mappings %>%
      # global-tech-db won't work for resources
      select(-resource, -reserve.subresource) %>%
      mutate(mapping.name = if_else(is.na(mapping.name), tech_mapping, mapping.name)) %>%
      right_join(tech_global_db, by = c("mapping.name")) %>%
      mutate(subsector = if_else(is.na(subsector), tranSubsector, subsector)) %>%
      select(xml, CO2 = ghgpolicy, sector.name = supplysector, subsector.name = subsector, technology = stub.technology) %>%
      repeat_add_columns(tibble(year = MODEL_YEARS))
    }

    # 6. Adjust any GDPIntensity targets -------------------------
    if (any(!is.na(L3221.CCap_constraint$GDPIntensity_BaseYear))){
      GDP_Intensity_targets <- L3221.CCap_constraint %>%
        filter(!is.na(GDPIntensity_BaseYear))

      # First calculate GDP series - we have baseGDP, growth rate in perCapitaGDP and population
      L3221.GDP <- L201.GDP_Scen %>%
        filter(scenario == paste0("g", socioeconomics.BASE_GDP_SCENARIO))

      # Next get the energy technologies that we want to constraint
      L3221.emissions_techs <- bind_rows(L3221.CCap_tech,
                                      rename(L3221.CCap_tranTech, subsector = tranSubsector),
                                      rename(L3221.CCap_resource, supplysector = resource,
                                             subsector = reserve.subresource,
                                             stub.technology = resource.reserve.technology)) %>%
        semi_join(GDP_Intensity_targets, by = c("region", "CO2" = "ghgpolicy", "year" = "constraint.year")) %>%
        left_join_error_no_match(select(GDP_Intensity_targets, region, CO2 = ghgpolicy,
                                        GDPIntensity_BaseYear, constraint, year = constraint.year),
                                 by = c("region", "CO2", "year"))

      # Calculate the CO2 emissions of the techs in the base year
      L3221.baseEmissions <- L3221.emissions_techs %>%
        select(-year, -constraint) %>%
        distinct() %>%
        left_join(CO2byTech, by = c("region", "supplysector" = "sector", "subsector",
                                                         "stub.technology" = "technology", "GDPIntensity_BaseYear" = "year")) %>%
        group_by(region, CO2, GDPIntensity_BaseYear) %>%
        summarise(value = sum(value, na.rm = T)) %>%
        ungroup

      # Add in GDP and calculate base GDP Intensity
      L3221.baseIntensity <- L3221.baseEmissions %>%
        left_join_error_no_match(L3221.GDP, by = c("region", "GDPIntensity_BaseYear" = "year")) %>%
        mutate(GDP_intensity = value / GDP) %>%
        select(region, CO2, GDPIntensity_BaseYear, GDP_intensity)

      # Future GDP intensities
      L3221.futureIntensity <- GDP_Intensity_targets %>%
        left_join_error_no_match(L3221.baseIntensity,
                                 by = c("region", "ghgpolicy" = "CO2", "GDPIntensity_BaseYear")) %>%
        mutate(GDP_intensity = GDP_intensity * (100 + constraint) / 100)

      # Calculate future energy constraints based on GDP intensities
      L3221.futureEmissions <- L3221.futureIntensity %>%
        left_join_error_no_match(L3221.GDP, by = c("region", "constraint.year" = "year")) %>%
        mutate(constraint = GDP_intensity * GDP) %>%
        select(xml, region, market, ghgpolicy, year.fillout, constraint.year, constraint)

      L3221.CCap_constraint <- L3221.CCap_constraint %>%
        filter(is.na(GDPIntensity_BaseYear)) %>%
        bind_rows(L3221.futureEmissions)


    }

    # 7. Get ghg link for each -------------------------
    L3221.CCap_GHG_Link <- A_CCap_Constraint %>%
      distinct(xml, link.type, market, ghgpolicy, adjust.year) %>%
      left_join(market_region_mappings, by = "market") %>%
      mutate(region = if_else(!is.na(region), region, market),
             year = adjust.year) %>%
      filter(!is.na(link.type)) %>%
      left_join(ghg_link, by = "link.type") %>%
      rename(linked.policy = ghgpolicy) %>%
      select(-link.type)

    # Produce outputs ------------------------------------
    L3221.CCap_constraint %>%
      select(-GDPIntensity_BaseYear, -link.type) %>%
      add_title("Custom carbon constraints", overwrite = T) %>%
      add_units("MTC") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CCap_Region") ->
      L3221.CCap_constraint

    L3221.CCap_link_regions %>%
      add_title("Linked regions to carbon constraints", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CCap_Region") ->
      L3221.CCap_link_regions

    L3221.CCap_tech %>%
      add_title("Technology mapping to custom ghg policy", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Region",
                     "policy/A_CCap_Sector",
                     "L221.StubTech_en",
                     "L222.StubTech_en",
                     "L224.StubTech_heat",
                     "L2233.StubTech_elec_cool",
                     "L226.StubTech_en",
                     "L232.StubTech_ind",
                     "L2321.StubTech_cement",
                     "L2322.StubTech_Fert",
                     "L2323.StubTech_iron_steel",
                     "L2324.StubTech_Off_road",
                     "L2325.StubTech_chemical",
                     "L2326.StubTech_aluminum",
                     "L244.StubTech_bld") ->
      L3221.CCap_tech

    L3221.CCap_tranTech %>%
      add_title("Transport technology mapping to custom ghg policy", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Region",
                     "policy/A_CCap_Sector",
                     "L254.StubTranTech") ->
      L3221.CCap_tranTech

    L3221.CCap_resource %>%
      add_title("Resource technology mapping to custom ghg policy", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Region",
                     "policy/A_CCap_Resource",
                     "L210.ResTechCoef") ->
      L3221.CCap_resource

    L3221.CCap_GHG_Link %>%
      add_title("GHG Link for carbon/GHG caps", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/A_CTax_Link") ->
    L3221.CCap_GHG_Link

    L3221.CCap_global_tech %>%
      add_title("CO2 Names for global-tech-db", overwrite = T) %>%
      add_units("NA") %>%
      add_precursors("policy/A_CCap_Constraint",
                     "policy/mappings/policy_tech_mappings",
                     "policy/mappings/policy_tranSubsector_mappings",
                     "policy/mappings/policy_resource_mappings") ->
      L3221.CCap_global_tech

    return_data(L3221.CCap_constraint, L3221.CCap_link_regions,
                L3221.CCap_tech, L3221.CCap_tranTech, L3221.CCap_resource,
                L3221.CCap_GHG_Link, L3221.CCap_global_tech)
  } else {
    stop("Unknown command")
  }
}

