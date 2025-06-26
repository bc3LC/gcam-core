# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_emissions_L241.fgas
#'
#' Format fgases emission inputs for GCAM and estimates future emission factors for f gases for the SSP scenarios.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L241.hfc_all}, \code{L241.pfc_all}, \code{L241.hfc_future}, \code{L241.fgas_all_units}. The corresponding file in the
#' original data system was \code{L241.fgas.R} (emissions level2).
#' @details Formats hfc and pfc gas emissions for input. Calculates future emission factors for hfc gases based on 2010 region emissions and USA emission factors and emission factors from Guus Velders (http://www.sciencedirect.com/science/article/pii/S135223101530488X) for the  SSP scenarios.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else group_by left_join mutate select
#' @importFrom tidyr gather spread
#' @author KD July 2017
module_emissions_L241.fgas <- function(command, ...) {
  if(command == driver.DECLARE_INPUTS) {
    return(c(FILE = "common/GCAM_region_names",
             FILE = "emissions/A_regions",
             FILE = "emissions/FUT_EMISS_GV",
             FILE = "emissions/FUT_EMISS_GV_CP_High",
             FILE = "emissions/FUT_EMISS_GV_CP_Low",
             FILE = "emissions/FUT_EMISS_GV_Kigali_High",
             FILE = "emissions/FUT_EMISS_GV_Kigali_Low",
             FILE = "emissions/mappings/Montreal_nonA5_GCAMreg",
             "L141.hfc_R_S_T_Yh",
             "L141.hfc_ef_R_cooling_Yh",
             "L142.pfc_R_S_T_Yh"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L241.hfc_all",
             "L241.pfc_all",
             "L241.hfc_future",
             "L241.fgas_all_units"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    GCAM_region_names <- get_data(all_data, "common/GCAM_region_names")
    A_regions         <- get_data(all_data, "emissions/A_regions")
    FUT_EMISS_GV      <- get_data(all_data, "emissions/FUT_EMISS_GV")
    FUT_EMISS_GV_CP_High      <- get_data(all_data, "emissions/FUT_EMISS_GV_CP_High")
    FUT_EMISS_GV_CP_Low      <- get_data(all_data, "emissions/FUT_EMISS_GV_CP_Low")
    FUT_EMISS_GV_Kigali_Low      <- get_data(all_data, "emissions/FUT_EMISS_GV_Kigali_Low")
    FUT_EMISS_GV_Kigali_High      <- get_data(all_data, "emissions/FUT_EMISS_GV_Kigali_High")
    iso_Montreal_nonA5_reg <-  get_data(all_data, "emissions/mappings/Montreal_nonA5_GCAMreg")
    L142.pfc_R_S_T_Yh <- get_data(all_data, "L142.pfc_R_S_T_Yh", strip_attributes = T)
    L141.hfc_R_S_T_Yh <- get_data(all_data, "L141.hfc_R_S_T_Yh", strip_attributes = T)
    L141.hfc_ef_R_cooling_Yh <- get_data(all_data, "L141.hfc_ef_R_cooling_Yh", strip_attributes = T)

    ## silence package check.
    . <- `2010` <- `2020` <- `2030` <- EF <- Emissions <- GCAM_region_ID <- GDP <-
      Non.CO2 <- Ratio_2020 <- Ratio_2030 <- Scenario <- Species <- USA_factor <-
      Year <- curr_table <- emiss.coeff <- input.emissions <- region <-
      stub.technology <- subsector <- supplysector <- value <- year <-
      year_min <- NULL

    # ===================================================
    # Use data from Guus Velders et al to derive future HFC emission factor trends.

    # This is the latest base year in GV data until we get to a 2030 base year!
    # (This projection data should be updated long before then...)
    emissions.HFC_MODEL_BASE_YEARS    <- MODEL_YEARS[MODEL_YEARS <= MODEL_FINAL_BASE_YEAR]

    # Combine data into one DF
    FUT_EMISS_GV_CP_Low$scenario <- "FUT_EMISS_GV_CP_Low"
    FUT_EMISS_GV_CP_High$scenario <- "FUT_EMISS_GV_CP_High"
    FUT_EMISS_GV_Kigali_Low$scenario <- "FUT_EMISS_GV_Kigali_Low"
    FUT_EMISS_GV_Kigali_High$scenario <- "FUT_EMISS_GV_Kigali_High"

    # For Kigali scenarios, substitute in the corresponding production data from
    # current policies as this is the driver that we would want to use to derive
    # future emission factor reductions since this is the baseline activity
    # before Kigali phase out. We, therefore, will be estimating how much
    # emissions factors decrease due to Kigali phase-outs
    FUT_EMISS_GV_Kigali_Low$Prod_tot <- FUT_EMISS_GV_CP_Low$Prod_tot
    FUT_EMISS_GV_Kigali_High$Prod_tot <- FUT_EMISS_GV_CP_High$Prod_tot
    FUT_EMISS_GV_Kigali_Low$Prod_A5 <- FUT_EMISS_GV_CP_Low$Prod_A5
    FUT_EMISS_GV_Kigali_High$Prod_A5 <- FUT_EMISS_GV_CP_High$Prod_A5
    FUT_EMISS_GV_Kigali_Low$Prod_nonA5 <- FUT_EMISS_GV_CP_Low$Prod_nonA5
    FUT_EMISS_GV_Kigali_High$Prod_nonA5 <- FUT_EMISS_GV_CP_High$Prod_nonA5

    FUT_EMISS_GV_NEW <- rbind(FUT_EMISS_GV_CP_High,FUT_EMISS_GV_Kigali_Low,FUT_EMISS_GV_Kigali_High,FUT_EMISS_GV_CP_Low)
    emissions.GV_ALL_YEARS <- intersect(FUT_EMISS_GV_NEW$Year[unique(FUT_EMISS_GV_NEW$Year)], MODEL_YEARS)
    emissions.GV_FUTURE_YEARS <- intersect(FUT_EMISS_GV_NEW$Year[unique(FUT_EMISS_GV_NEW$Year) > MODEL_FINAL_BASE_YEAR], MODEL_YEARS)

    MAX_DATA_YEAR <- max(intersect(emissions.GV_ALL_YEARS, emissions.HFC_MODEL_BASE_YEARS))

    # In case max_data_year is not in FUT_EMISS_GV (ie timeshift), use minimum year instead
    if(MAX_DATA_YEAR %in% FUT_EMISS_GV_NEW$Year){
      ratio_years <- c(MAX_DATA_YEAR, emissions.GV_FUTURE_YEARS)
    } else {
      ratio_years <-  c(min(FUT_EMISS_GV$Year), emissions.GV_FUTURE_YEARS)}

    # When production declines in the future, we don't want the EF to increase,
    # (which it will tend to do if we use Em/Prod, due to banks - emissions
    # continue while production declines), so instead keep production flat if
    # production starts to decline. This change avoids an unrealistic steep
    # increase in emissions in nonA5 regions in the immediate future.
    FirstYr <- MODEL_FINAL_BASE_YEAR

    FUT_EMISS_GV_FUT <- FUT_EMISS_GV_NEW %>%
      filter(Year >=MODEL_FINAL_BASE_YEAR)

    # First extract maximum production in last historical year or future year
    FUT_EMISS_GV_FUT %>%
      group_by(Species, scenario) %>%
      dplyr::slice_max(Prod_A5) %>% select(Species, scenario,Year) %>%
      rename(MaxA5Year = Year ) %>%  ungroup() -> MaxProdA5Year

    FUT_EMISS_GV_FUT %>%
      group_by(Species, scenario) %>%
      dplyr::slice_max(Prod_nonA5) %>% select(Species, scenario,Year) %>%
      rename(MaxNonA5Year = Year ) %>%  ungroup() -> MaxProdNonA5Year

    FUT_EMISS_GV_FUT %>%
      group_by(Species, scenario) %>%
      # Use left_join's because number of rows differ
      left_join(MaxProdA5Year, by = c("Species", "scenario"), relationship = "many-to-many") %>%
      left_join(MaxProdNonA5Year, by = c("Species", "scenario"), relationship = "many-to-many") %>%
      mutate(Prod_A5max = max(Prod_A5), Prod_nonA5max = max(Prod_nonA5)) %>%
      ungroup() %>%
      mutate(Prod_A5 = if_else( Year > MaxA5Year, Prod_A5max, Prod_A5)) %>%
      mutate(Prod_nonA5 = if_else( Year > MaxNonA5Year, Prod_nonA5max, Prod_A5)) ->
      FUT_EMISS_GV_FUT


    FUT_EMISS_GV_FUT %>%
      # Define emissions factor as emissions over production
      # This is not exactly correct, since emission banks play a big role, but is the closest we can get to GCAM's activity driven formulation
      mutate( EF = Emis_tot / Prod_tot) %>%
      mutate( EF_nonA5 = Emis_nonA5 / Prod_nonA5) %>%
      mutate( EF_A5 = Emis_A5 / Prod_A5) %>%
      rename(year = Year) %>%
      filter(year %in% ratio_years) %>%
      group_by(Species) %>%
      # Calculate EF trends we will use to adjust GCAM's emission trajectory
      # These trends represent Kigali phase downs compared to the CP (no Kigali) scenario
      # We use the regionally differentiated trends below, but the global one is also calculated
      mutate(ratio = EF / EF[year == min(ratio_years)]) %>%
      mutate(ratio_nonA5 = EF_nonA5 / EF_nonA5[year == min(ratio_years)]) %>%
      mutate(ratio_A5 = EF_A5 / EF_A5[year == min(ratio_years)]) %>%
      ungroup %>%
      # Format the FUT_EMISS_GV species to be consistent with GCAM names by removing the "-"
      mutate(Species=gsub('HFC-43-10mee', 'HFC43', Species)) %>% # special case with different pattern
      mutate(Species = gsub("-", "", Species)) %>%
      select(-Bank_tot, -Mix_tot,  -Bank_nonA5, -Bank_A5, -Emis_nonA5, -Emis_A5, -Emis_tot, -Prod_tot) %>%
      select(-Prod_nonA5, -Prod_A5, -EF, -EF_nonA5, -EF_A5  ) %>%
      filter(year %in% emissions.GV_FUTURE_YEARS) ->
      L241.FUT_EF_Ratio_All

    # Default to the "high" scenario set, although there is not a large difference
    SCEN_HighLow <- "_High"

    # For single scenario selection, indicate that scenario here
    SELECT_SCENARIO <- "FUT_EMISS_GV_Kigali"

    # If this variable is > 0, then a hybrid scenario
    # If BLEND_FRACT > 0 then scenario is BLEND_FRACT*Kigali + (1-BLEND_FRACT)*CP
    # If BLEND_FRACT < 0 then reduce below Kigali scenario by BLEND_FRACT fraction by 2100
    BLEND_FRACT <- 0.0

    # Select the base F-gas future scenario to use here
    L241.FUT_EF_Ratio_All %>%
      filter(scenario %in% paste0(SELECT_SCENARIO,SCEN_HighLow)) %>%
      select(-scenario, - ratio) ->
      L241.FUT_EF_Ratio

    # Modify if requested (BLEND_FRACT == 0 means just use the selected base scenario)
    if ( BLEND_FRACT > 0 ) { # Branch for incomplete implementation of Kigali
      if( !grepl("Kigali",SELECT_SCENARIO) ) stop('Select a Kigali base scenario in order to generate a blended scenario')
      L241.FUT_EF_Ratio_All %>%
        filter(scenario %in% paste0("FUT_EMISS_GV_CP",SCEN_HighLow)) %>% select(-scenario, - ratio) %>%
        rename("CP_ratio_nonA5" = "ratio_nonA5", "CP_ratio_A5" = "ratio_A5") ->
        CP_Scenario
      L241.FUT_EF_Ratio <- L241.FUT_EF_Ratio %>%
        # We expect NAs here because HFC43 emissions are only in one region
        left_join(CP_Scenario, by = c("Species", "year")) %>%
        group_by(Species,year) %>%
        mutate( ratio_nonA5 = ratio_nonA5 + BLEND_FRACT*(CP_ratio_nonA5 - ratio_nonA5) ) %>%
        mutate( ratio_A5 = ratio_A5 + BLEND_FRACT*(CP_ratio_A5 - ratio_A5) ) %>%
        select(-CP_ratio_nonA5, -CP_ratio_A5) %>%
        ungroup()

    } else if ( BLEND_FRACT < 0 ) { # Branch for implementation beyond Kigali
      if( !grepl("Kigali",SELECT_SCENARIO) ) stop('Select a Kigali base scenario in order to generate a blended scenario')
      FDEC_START_YEAR <- 2040 # Year that ambition would begin to be strengthened
      # Generate a linear fraction
      L241.FUT_EF_Ratio <- L241.FUT_EF_Ratio %>%
        # Generate a linearly increasing fraction
        group_by(Species) %>%
        mutate(fraction = 1 - (year - FDEC_START_YEAR) / (max(MODEL_YEARS) - FDEC_START_YEAR)*abs(BLEND_FRACT)) %>%
        mutate(fraction = if_else(year < FDEC_START_YEAR, 1, fraction)) %>%
        group_by(Species) %>%
        mutate( ratio_nonA5 = ratio_nonA5 * fraction, ratio_A5 = ratio_A5 * fraction ) %>%
        select( -fraction)
     }

    # ===================================================
    # Scale EDGAR emission global totals to match Velders
    # Velders totals are more definitive since they are calibrated to observed concentrations
    # This corrects signifiant biases in two gases in partciular

    # Add marker column to iso_Montreal_nonA5_reg
    iso_Montreal_nonA5_reg$marker <- 1

    # Calculate totals for both datasets
    L141.hfc_R_S_T_Yh %>% dplyr::group_by(Non.CO2, year) %>%
      dplyr::summarise(EDGAR_total = sum(value, na.rm = TRUE)) -> EDGAR_Global_Em

    FUT_EMISS_GV_NEW %>%
      filter(scenario %in% SELECT_SCENARIO) %>%
      mutate(Species=gsub('HFC-43-10mee', 'HFC43', Species)) %>% # special case with different pattern
      mutate(Species = gsub("-", "", Species)) %>%
      dplyr::group_by(Non.CO2=Species, year=Year) %>%
      # Change to same units as EDGAR
      mutate(Emis_tot = Emis_tot / 1000) %>%
      dplyr::summarise(Velders_total = sum(Emis_tot, na.rm = TRUE)) -> Velders_Global_Em

    EDGAR_Global_Em %>% full_join(Velders_Global_Em, by = c("Non.CO2", "year")) %>%
      filter(grepl("HFC",Non.CO2)) %>%
      mutate(ratio = Velders_total / EDGAR_total) %>%
      filter(year <= MODEL_FINAL_BASE_YEAR) %>%
      na.omit() %>% #Ok to omit since only scale where species is in both datasets
      select(-Velders_total, -EDGAR_total) -> EM_scaler

    # Now scale EDGAR emissions to global Velders's totals
    L141.hfc_R_S_T_Yh %>%
      # Use left join since there will be NAs
      left_join(EM_scaler, by = c("Non.CO2","year")) %>%
      mutate(value = if_else(is.na(ratio), value, value * ratio)) %>%
      select(-ratio) -> L141.hfc_R_S_T_Yh

    # Also scale edgar emission factors to global Velders' totals
    L141.hfc_ef_R_cooling_Yh %>%
      # Use left join since there will be NAs
      left_join(EM_scaler, by = c("Non.CO2","year")) %>%
      mutate(value = if_else(is.na(ratio) , value , value * ratio)) %>%
      select(-ratio) -> L141.hfc_ef_R_cooling_Yh

    # Format and round emission values for HFC gas emissions for technologies in all regions.
    L141.hfc_R_S_T_Yh %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      mutate(input.emissions = round(value, emissions.DIGITS_EMISSIONS)) %>%
      select(-GCAM_region_ID, -value) ->
      L241.hfc_all

    # ===================================================
    # L241.pfc: F-gas emissions for technologies in all regions.
    #
    # Remove anything that's zero in all base years for any technology, because no future
    # coefs are read in for any techs.
    #
    # Then round future gas emissions and format the data frame.
    L142.pfc_R_S_T_Yh %>%
      group_by(GCAM_region_ID, supplysector, subsector, stub.technology, Non.CO2) %>%
      filter(sum(value) != 0, year %in% MODEL_BASE_YEARS) %>%
      mutate(input.emissions = round(value, emissions.DIGITS_EMISSIONS), year = as.numeric(year)) %>%
      ungroup() %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID, -value) ->
      L241.pfc_all

    # Now subset only the relevant technologies and gases (i.e., drop ones whose values are zero in all years).
    L241.pfc_all %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      filter(sum(input.emissions) != 0, year %in% MODEL_BASE_YEARS) %>%
      mutate(year = as.numeric(year)) %>%
      ungroup ->
      L241.pfc_all_screened

    # ===================================================
    # ---------------------------------------------------------
    # Estimate future emission trends for cooling emissions

    # First, create a subset of the cooling emission factors from the max year
    # Eventually these values will be used to estimate future emission factors by scaling with
    # USA emission factors.

    L141.hfc_ef_R_cooling_Yh %>%
      filter(year == MAX_DATA_YEAR) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L141.hfc_ef_cooling_maxhistyr

    # Use the future emission factor ratios to scale process emission factors
    L141.hfc_ef_cooling_maxhistyr %>%
      select(-year) %>%
      # Since Guus Velders data set contains information on extra gases we
      # use left_join here because we expect there to be NAs that will latter be dealt with
      left_join(L241.FUT_EF_Ratio, by = c("Non.CO2" = "Species")) %>%
      # Again use left_join here because mapping is only for nonA5 regions
      left_join(iso_Montreal_nonA5_reg, , by = c("GCAM_region_ID","region")) %>%
      mutate(value = if_else(is.na(marker) , value * ratio_nonA5, value * ratio_A5)) %>%
      select(-ratio_A5, -ratio_nonA5, -marker ) %>%
      # Ok to use na.omit since this for future EFs, emissions without Velders
      # data will  have default growth trend
      na.omit() %>%
      # Keep only for future years
      filter(!year %in% emissions.HFC_MODEL_BASE_YEARS) ->
      L241.hfc_cool_ef_update_all

    # ===================================================
    # ---------------------------------------------------------
    # Estimate future emission trends for process emissions.
    #
    # First, subset the hfc emissions for process emission sectors
    # and extract the process emission EF for the last historical year
    L141.hfc_R_S_T_Yh %>%
      filter(grepl("processes",supplysector)) %>%
      # EF is emissions / cal input value for processes emission sectors
      mutate(value = value / emissions.INDURB_PROCESS_MISCEMISSIONS_CALVAL) %>%
      filter(year == MAX_DATA_YEAR) %>%
      filter(value > 0) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L241.hfc_ef_maxhistyr

    # Use the future emission factor ratios to scale process emission factors
    L241.hfc_ef_maxhistyr %>%
      select(-year) %>%
      # Since Velders data set fewer  gases we
      # use left_join here because we expect there to be NAs
      left_join(L241.FUT_EF_Ratio, by = c("Non.CO2" = "Species")) %>%
      # Again use left_join here because mapping is only for nonA5 regions
      left_join(iso_Montreal_nonA5_reg, , by = c("GCAM_region_ID","region")) %>%
      mutate(value = if_else(is.na(marker) , value * ratio_nonA5, value * ratio_A5)) %>%
      select(-ratio_A5, -ratio_nonA5, -marker ) %>%
      # Ok to use na.omit since this for future EFs, emissions without Velders
      # data will  have default growth trend
      na.omit() %>%
      filter(!year %in% emissions.HFC_MODEL_BASE_YEARS) ->
      L241.hfc_ef_update_all

    # ===================================================
    # Combine the updated energy and process hfc gas emission
    # factor data frames together.

    L241.hfc_ef_update_all %>%
      bind_rows(L241.hfc_cool_ef_update_all) %>%
      mutate(emiss.coeff = round(value, emissions.DIGITS_EMISSIONS),
             year = as.numeric(year)) %>%
      select(region, supplysector, subsector, stub.technology, year, Non.CO2, emiss.coeff) %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      tidyr::complete(year = min(MODEL_FUTURE_YEARS)) %>%
      mutate(emiss.coeff = approx_fun(year, emiss.coeff)) %>%
      ungroup() %>%
      # note: we actually allow some base year rows in the "future" table because we
      # need to include some emissions factors for some missing regions
      filter(year %in% MODEL_YEARS) ->
      L241.hfc_future

    # Now subset only the relevant technologies and gases (i.e., drop ones whose values are zero in all years).
    L241.hfc_all %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      filter(sum(input.emissions) != 0, year %in% MODEL_BASE_YEARS) %>%
      mutate(year = as.numeric(year)) %>%
      ungroup ->
      L241.hfc_all_screened

    # Set the units string for the hfc and pfc gases.
    L241.pfc_all_screened %>%
      bind_rows(L241.hfc_all_screened) %>%
      bind_rows(L241.hfc_future) %>%
      # we need to just add the unit tag for all gasses and model years
      # however we need to be careful because some gasses do not start
      # in the same year
      select(region, supplysector, subsector, stub.technology, year, Non.CO2) %>%
      group_by(region, supplysector, subsector, stub.technology, Non.CO2) %>%
      summarize(year_min = min(year)) %>%
      ungroup() %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      filter(year >= year_min) %>%
      select(-year_min) %>%
      mutate(emissions.unit = emissions.F_GAS_UNITS) ->
      L241.fgas_all_units

    # ===================================================

    L241.hfc_all_screened %>%
      add_title("HFC gas emission input table") %>%
      add_units("Gg") %>%
      add_comments("Emission values from L1 rounded to the appropriate digits.") %>%
      add_legacy_name("L241.hfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.hfc_all

    L241.pfc_all_screened %>%
      add_title("PFC gas emission input table") %>%
      add_units("Gg") %>%
      add_comments("Emission values from L1 are rounded to the appropriate digits.") %>%
      add_legacy_name("L241.pfc_all") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.pfc_all

    L241.hfc_future %>%
      add_title("Future HFC emission factors") %>%
      add_units("Gg") %>%
      add_comments("Cooling future emission factors are calculated from 2010 USA emission factors.") %>%
      add_comments("Non-cooling future emission factors are calculated from Guus Velders emission factors.") %>%
      add_legacy_name("L241.hfc_future") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.hfc_future

    L241.fgas_all_units %>%
      add_title("Units for f gases.") %>%
      add_units("Gg") %>%
      add_comments("NA") %>%
      add_legacy_name("L241.fgas_all_units") %>%
      add_precursors("common/GCAM_region_names", "emissions/A_regions", "emissions/FUT_EMISS_GV",
                     "L141.hfc_R_S_T_Yh", "L142.pfc_R_S_T_Yh",
                     "L141.hfc_ef_R_cooling_Yh") ->
      L241.fgas_all_units

    return_data(L241.hfc_all, L241.pfc_all, L241.hfc_future, L241.fgas_all_units)

  } else {
    stop("Unknown command")
  }
}
