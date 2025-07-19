# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L141.hfc_R_S_T_Y
#'
#' Calculate HFC emissions from EDGAR, by residential and commercial cooling shares,
#' adjusted to match the Guus Velders HFC inventory.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L141.hfc_R_S_T_Yh}, \code{L141.hfc_ef_R_cooling_Yh}. The corresponding file in the
#' original data system was \code{L141.hfc_R_S_T_Y.R} (emissions level1).
#' @details Describe in detail what this chunk does.
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange bind_rows filter group_by left_join matches mutate select summarise vars
#' @importFrom tidyr replace_na
#' @author RMH Aug 2017
module_emissions_L141.hfc_R_S_T_Y <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/gcam_fgas_tech",
             FILE = "emissions/other_f_gases",
             "L144.in_EJ_R_bld_serv_F_Yh",
             "L144.base_service_EJ_serv",
             FILE = "common/iso_GCAM_regID",
             FILE = "emissions/EDGAR/EDGAR_sector_fgas",
             FILE = "emissions/EDGAR/EDGAR_all_F_gases",
              FILE = "emissions/EDGAR/EDGAR_HFC125", #TODO Get rid of all of these, update dependencies
             FILE = "emissions/EDGAR/EDGAR_HFC134a",
             FILE = "emissions/EDGAR/EDGAR_HFC143a",
             FILE = "emissions/EDGAR/EDGAR_HFC152a",
             FILE = "emissions/EDGAR/EDGAR_HFC227ea",
             FILE = "emissions/EDGAR/EDGAR_HFC23",
             FILE = "emissions/EDGAR/EDGAR_HFC236fa",
             FILE = "emissions/EDGAR/EDGAR_HFC245fa",
             FILE = "emissions/EDGAR/EDGAR_HFC32",
             FILE = "emissions/EDGAR/EDGAR_HFC365mfc",
             FILE = "emissions/EDGAR/EDGAR_HFC43",
             FILE = "socioeconomics/income_shares",
             "L244.GenericShares",
             "L244.ThermalShares"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L141.hfc_R_S_T_Yh",
             "L141.hfc_ef_R_cooling_Yh", "L141.EDGAR_Fgas"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    #silence packages
    emiss_share <- supply <- emscalar <- tot_emissions <- MAC_type1 <- gwp <- EPA_emissions <- EPA_sector <- NULL


    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    gcam_fgas_tech <- get_data(all_data, "emissions/gcam_fgas_tech", strip_attributes = TRUE)
    other_f_gases <- get_data(all_data, "emissions/other_f_gases")
    L144.in_EJ_R_bld_serv_F_Yh <- get_data(all_data, "L144.in_EJ_R_bld_serv_F_Yh")
    L144.base_service_EJ_serv <- get_data(all_data, "L144.base_service_EJ_serv")
    iso_GCAM_regID <- get_data(all_data, "common/iso_GCAM_regID")
    EDGAR_sector <- get_data(all_data, "emissions/EDGAR/EDGAR_sector_fgas")
    EDGAR_ALL_EM <- get_data(all_data, "emissions/EDGAR/EDGAR_all_F_gases")

    . <- year <- value <- GCAM_region_ID <- sector <- fuel <- service <-
      IPCC_description <- agg_sector <- ISO_A3 <- iso <- EDGAR_agg_sector <-
      Non.CO2 <- emissions <- Sector <- total <- share <- Species <- Emissions <-
      SSP2_tot <- EDGAR_tot <- scaler <- supplysector <- subsector <- stub.technology <-
      adj_emissions <- energy <- em_fact <- NULL  # silence package check notes

    # Processing and Mapping EDGAR HFC emissions to GCAM technologies
    # ===============================================================

    # Initial re-formatting of EDGAR Emissions
    # Note EDGAR has no cooling emissions in developing regions
    # All emissions are lumped in with commertial refrigeration
    # Velders does something analogous, lumped into their "ICR" sector
    EDGAR_ALL_EM_V2 <- EDGAR_ALL_EM %>%
      rename(IPCC = ipcc_code_1996_for_standard_report) %>%
      rename(Non.CO2 = Substance) %>%
      mutate(Non.CO2=gsub('HFC-43-10-mee', 'HFC43', Non.CO2)) %>% # special case with different pattern
      mutate(Non.CO2=gsub('-', '', Non.CO2)) %>%
      mutate(iso = tolower(Country_code_A3), Country_code_A3 = NULL)

    # Combine F-gas with sector and region mapping
    EDGAR_ALL_EM_V2 %>%
        left_join_error_no_match(EDGAR_sector %>% select(-IPCC_description), by = "IPCC") %>% # Add Edgar agg_sector
        rename(EDGAR_agg_sector = agg_sector) %>% # rename agg_sector to EDGAR_agg_sector
        change_iso_code('rou', 'rom') %>% # Convert Romania iso code to pre-2002 value
        left_join_error_no_match(iso_GCAM_regID, by = "iso") %>% # Map iso to GCAM region
        select(GCAM_region_ID, iso, EDGAR_agg_sector, Non.CO2, matches(YEAR_PATTERN)) %>%
        gather_years(value_col = "emissions") %>%
        # Filter out the two HCFCs in EDGAR, Hector has these exogenously
        filter(!(Non.CO2 %in% c("HCFC-141b","HCFC-142b"))) %>%
        mutate(emissions = as.numeric(emissions)) -> F_gases_formatted

    # Combine emission species with small (GWP-weighted) amounts with another emission species
    F_gases_formatted <- F_gases_formatted %>%
      # Using AR6 GWPs - these are very small amounts, so will make no practical difference so have hard coded these
      mutate( emissions = if_else(Non.CO2 == "HFC134", emissions * 1260.0/1530.0, emissions)) %>%
      mutate( Non.CO2 = if_else(Non.CO2 == "HFC134", "HFC134a", Non.CO2)) %>%

      # Lifetime of HFC-143 is far shorter than HFC-143a, so map to HFC-32 instead which has a closer lifetime
      mutate( emissions = if_else(Non.CO2 == "HFC143", emissions * 364.0/771.0, emissions)) %>%
      mutate( Non.CO2 = if_else(Non.CO2 == "HFC143", "HFC32", Non.CO2)) %>%

    # Lifetime of HFC-41 is very short so also convert to HFC-32 (shortest lifetime F-gas in hector)
      mutate( emissions = if_else(Non.CO2 == "HFC41", emissions * 135.0/771.0, emissions)) %>%
      mutate( Non.CO2 = if_else(Non.CO2 == "HFC41", "HFC32", Non.CO2))

    L141.EDGAR_HFC <- F_gases_formatted %>%
      group_by(GCAM_region_ID, EDGAR_agg_sector, Non.CO2,  year) %>%
      summarise(emissions = sum(emissions))

    # Set years using data instead of by constant
    edgar_names<-as.numeric(names(EDGAR_ALL_EM_V2))
    emissions.EDGAR_YEARS <- 1971:max(edgar_names[!is.na(edgar_names)])

    # Revise sector mapping to leave only HFC-23 in halo_prod
    # Other F-gases in this category are very small portion of total so move to generic other sector
         # TODO YET

    # Map Emissions to GCAM technologies
    L141.hfc_R_S_T_Yh.long <- gcam_fgas_tech %>%
      repeat_add_columns(tibble(GCAM_region_ID = unique(GCAM_region_names$GCAM_region_ID))) %>%
      repeat_add_columns(tibble(year = emissions.EDGAR_YEARS)) %>%
      repeat_add_columns(tibble(Non.CO2 = unique(L141.EDGAR_HFC$Non.CO2))) %>%
      left_join(L141.EDGAR_HFC,
                by = c("EDGAR_agg_sector", "GCAM_region_ID", "year", "Non.CO2")) %>% # there should be NAs as some sectors have no emissions
      replace_na(list(emissions = 0)) # replace NAs with zero

    # Calculates shares of IEA cooling energy balances that go to residential and commercial
    # ======================================================================================
    # Select residential and cooling emisssions from L144.in_EJ_R_bld_serv_F_Yh
    L141.R_cooling_T_Yh.long <- L144.in_EJ_R_bld_serv_F_Yh %>%
      filter(grepl("cooling",service), fuel == "electricity")
    # Group by GCAM region and ID and year in new data frame (use to calculate share of total later)
    L141.R_cooling_Yh <-  L141.R_cooling_T_Yh.long %>%
      group_by(GCAM_region_ID,year) %>%
      summarise(total = sum(value))
    # Join in the totals we just calculated and calulate the shares
    L141.R_cooling_T_Yh.long <- left_join_error_no_match(L141.R_cooling_T_Yh.long,L141.R_cooling_Yh, by = c("GCAM_region_ID", "year")) %>%
      mutate(share = value / total) %>%
      select(GCAM_region_ID, year, service, share, value)

    # Add res/com cooling share to HFC and make adjustment
    L141.hfc_R_S_T_Yh.long %>%
      left_join(L141.R_cooling_T_Yh.long, by = c("GCAM_region_ID", "year", "supplysector" = "service")) %>% # there should be NAs.
      # there are sectors that don't have emissions
      replace_na(list(share = 1)) %>% # replace those shares with "1"
      mutate(emissions = emissions * share) %>%
      select(-share, -value) ->
      L141.hfc_R_S_T_Yh_coolshare

    # make sure EDGAR have data till 2015 for EPA BAU calibration
    # remaing_years is the missing base years that EPA has but EDGAR does not
    # Duplicate EDGAR last-year data and rename as missing years just as placeholders, which will be scaled to EPA later
    # Note: here additional years are for all missing years (2009, 2010, 2011 etc), not just GCAM modeling years
    # So we can deal with base-year time shifting
    additional_years <-
      HISTORICAL_YEARS[HISTORICAL_YEARS > emissions.EDGAR_YEARS[length(emissions.EDGAR_YEARS)]]

    if(length(additional_years) > 0){
      warning("EDGAR F-gas data does not extent to base year. Suggest updating EDGAR data. \n")
      TEMP <- filter(L141.hfc_R_S_T_Yh_coolshare, year == emissions.EDGAR_YEARS[length(emissions.EDGAR_YEARS)]) %>%
        select(-year) %>%
        repeat_add_columns(tibble(year = additional_years))

      L141.hfc_R_S_T_Yh_coolshare <- bind_rows(L141.hfc_R_S_T_Yh_coolshare, TEMP)
    }

    # Compute cooling HFC emissions factors
    L141.hfc_R_S_T_Yh.long  %>%
      filter(grepl("cooling",supplysector), year %in% HISTORICAL_YEARS) %>%
      # F-gas emission factors are output-based, so calculate EF as emissions over output service
      left_join_error_no_match(L144.base_service_EJ_serv %>% select(GCAM_region_ID, year, service, value),
                               by = c("GCAM_region_ID", "year", "supplysector" = "service")) %>%
      mutate(em_fact = emissions / value) %>%
      select(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2, year, em_fact) %>%
      replace_na(list(em_fact = 0)) %>%
      rename(value = em_fact) -> L141.hfc_ef_R_cooling_Yh

    # Rename column to what is expected below
    # (name change was result of now removed EPA data processing)
    names(L141.hfc_R_S_T_Yh.long)[names(L141.hfc_R_S_T_Yh.long) == "emissions"] <- "value"
    L141.hfc_R_S_T_Yh <- L141.hfc_R_S_T_Yh.long

    # ===============================================================
    # Need to allocate residential energy to the different consumer groups using the computed shares:
    L244.GenericShares<- get_data(all_data, "L244.GenericShares",strip_attributes = TRUE) %>%
      select(region,gcam.consumer,building.service.input,year,gen_share) %>%
      rename(supplysector=building.service.input,
             share=gen_share) %>%
      filter(grepl("resid",supplysector)) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      unite(supplysector,c("supplysector","group"),sep = "_") %>%
      select(-gcam.consumer) %>%
      complete(nesting(region,supplysector), year = c(year, unique(L141.hfc_R_S_T_Yh$year))) %>%
      # Interpolate
      group_by(region,supplysector) %>%
      mutate(share = approx_fun(year, share, rule = 2))

    L244.ThermalShares<- get_data(all_data, "L244.ThermalShares",strip_attributes = TRUE) %>%
      select(region,gcam.consumer,thermal.building.service.input,year,thermal_share) %>%
      rename(supplysector=thermal.building.service.input,
             share=thermal_share)%>%
      filter(grepl("resid",supplysector)) %>%
      separate(gcam.consumer,c("gcam.consumer","group"),sep = "_") %>%
      unite(supplysector,c("supplysector","group"),sep = "_") %>%
      select(-gcam.consumer)%>%
      complete(nesting(region,supplysector), year = c(year, unique(L141.hfc_R_S_T_Yh$year))) %>%
      # Interpolate
      group_by(region,supplysector) %>%
      mutate(share = approx_fun(year, share, rule = 2))

    L244.Shares<-bind_rows(L244.GenericShares,L244.ThermalShares) %>%
      left_join_error_no_match(GCAM_region_names, by = "region")

    # Save subregional categories
    cons.gr.adj<-get_data(all_data, "socioeconomics/income_shares",strip_attributes = TRUE)  %>%
      select(category) %>%
      distinct()

    # Adjust L141.hfc_R_S_T_Yh and L141.hfc_ef_R_cooling_Yh for the multiple consumers

    # L141.hfc_R_S_T_Yh: Represents emissions, value needs to be multiplied by the share to allocate across multiple consumers
    L141.hfc_R_S_T_Yh_resid<- L141.hfc_R_S_T_Yh %>%
      filter(grepl("resid",supplysector)) %>%
      repeat_add_columns(tibble(group=unique(cons.gr.adj$category))) %>%
      unite(supplysector,c("supplysector","group"),sep = "_") %>%
      # add shares
      left_join_error_no_match(L244.Shares, by = c("GCAM_region_ID", "year", "supplysector")) %>%
      mutate(value = value * share) %>%
      select(-region,-share)

    L141.hfc_R_S_T_Yh<-L141.hfc_R_S_T_Yh %>%
      filter(!grepl("resid",supplysector)) %>%
      bind_rows(L141.hfc_R_S_T_Yh_resid)

    #L141.hfc_ef_R_cooling_Yh: Represents emission factors, so they just need to be extended to multiple consumers
    L141.hfc_ef_R_cooling_Yh_resid<-L141.hfc_ef_R_cooling_Yh %>%
      filter(grepl("resid",supplysector)) %>%
      repeat_add_columns(tibble(group=unique(cons.gr.adj$category))) %>%
      unite(supplysector,c("supplysector","group"),sep = "_")

    L141.hfc_ef_R_cooling_Yh<-L141.hfc_ef_R_cooling_Yh %>%
      filter(!grepl("resid",supplysector)) %>%
      bind_rows(L141.hfc_ef_R_cooling_Yh_resid)

    # ===============
    # Produce outputs
    F_gases_formatted %>%
      ungroup() %>%
      add_title("EDGAR F-gas emissions by region / sector / technology / gas / historical year") %>%
      add_units("Gg") %>%
      add_comments("Edgar emissions matched to GCAM sectors but otherwise not processed") %>%
      add_legacy_name("none") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/EDGAR/EDGAR_HFC125",
                     "emissions/EDGAR/EDGAR_HFC134a",
                     "emissions/EDGAR/EDGAR_HFC143a",
                     "emissions/EDGAR/EDGAR_HFC152a",
                     "emissions/EDGAR/EDGAR_HFC227ea",
                     "emissions/EDGAR/EDGAR_HFC23",
                     "emissions/EDGAR/EDGAR_HFC236fa",
                     "emissions/EDGAR/EDGAR_HFC245fa",
                     "emissions/EDGAR/EDGAR_HFC32",
                     "emissions/EDGAR/EDGAR_HFC365mfc",
                     "emissions/EDGAR/EDGAR_HFC43",
                     "socioeconomics/income_shares",
                     "L244.GenericShares",
                     "L244.ThermalShares") ->
      L141.EDGAR_Fgas

    L141.hfc_R_S_T_Yh %>%
      add_title("HFC emissions by region / sector / technology / gas / historical year") %>%
      add_units("Gg") %>%
      add_comments("Edgar emissions, scaled to Guus HFC inventory for residential and commercial cooling") %>%
      add_legacy_name("L141.hfc_R_S_T_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/EDGAR/EDGAR_HFC125",
                     "emissions/EDGAR/EDGAR_HFC134a",
                     "emissions/EDGAR/EDGAR_HFC143a",
                     "emissions/EDGAR/EDGAR_HFC152a",
                     "emissions/EDGAR/EDGAR_HFC227ea",
                     "emissions/EDGAR/EDGAR_HFC23",
                     "emissions/EDGAR/EDGAR_HFC236fa",
                     "emissions/EDGAR/EDGAR_HFC245fa",
                     "emissions/EDGAR/EDGAR_HFC32",
                     "emissions/EDGAR/EDGAR_HFC365mfc",
                     "emissions/EDGAR/EDGAR_HFC43",
                     "socioeconomics/income_shares",
                     "L244.GenericShares",
                     "L244.ThermalShares") ->
      L141.hfc_R_S_T_Yh

    L141.hfc_ef_R_cooling_Yh %>%
      add_title("HFC emissions factors for cooling by region / sector / technology / gas / historical year") %>%
      add_units("Gg / EJ") %>%
      add_comments("HFC emissions (scaled to Guus data) divided by GCAM cooling energy use") %>%
      add_legacy_name("L141.hfc_ef_R_cooling_Yh") %>%
      add_precursors("common/GCAM_region_names",
                     "emissions/gcam_fgas_tech",
                     "emissions/other_f_gases",
                     "L144.in_EJ_R_bld_serv_F_Yh",
                     "common/iso_GCAM_regID",
                     "common/iso_GCAM_regID",
                     "emissions/EDGAR/EDGAR_sector_fgas",
                     "emissions/EDGAR/EDGAR_HFC125",
                     "emissions/EDGAR/EDGAR_HFC134a",
                     "emissions/EDGAR/EDGAR_HFC143a",
                     "emissions/EDGAR/EDGAR_HFC152a",
                     "emissions/EDGAR/EDGAR_HFC227ea",
                     "emissions/EDGAR/EDGAR_HFC23",
                     "emissions/EDGAR/EDGAR_HFC236fa",
                     "emissions/EDGAR/EDGAR_HFC245fa",
                     "emissions/EDGAR/EDGAR_HFC32",
                     "emissions/EDGAR/EDGAR_HFC365mfc",
                     "emissions/EDGAR/EDGAR_HFC43",
                     "socioeconomics/income_shares",
                     "L244.GenericShares",
                     "L244.ThermalShares") ->
      L141.hfc_ef_R_cooling_Yh

    return_data(L141.hfc_R_S_T_Yh, L141.hfc_ef_R_cooling_Yh, L141.EDGAR_Fgas)
  } else {
    stop("Unknown command")
  }
}
