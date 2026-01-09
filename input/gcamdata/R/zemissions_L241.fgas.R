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
             FILE = "emissions/FUT_EMISS_GV", # TODO Delete this and update dependencies
             FILE = "emissions/Velders_HFC/CP2021_constrProdEmis_ObsAgage_OECD-SSP5",
             FILE = "emissions/Velders_HFC/CP2021_constrProdEmis_ObsAgage_OECD-SSP3",
             FILE = "emissions/Velders_HFC/KGL2021_constrProdEmis_ObsAgage_OECD-SSP5",
             FILE = "emissions/Velders_HFC/KGL2021_constrProdEmis_ObsAgage_OECD-SSP3",
             FILE = "emissions/mappings/Montreal_nonA5_GCAMreg",
             FILE = "emissions/A_regions",
             "L201.Pop_GCAM3",
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
    CP2021_ssp5      <- get_data(all_data, "emissions/Velders_HFC/CP2021_constrProdEmis_ObsAgage_OECD-SSP5")
    CP2021_ssp3      <- get_data(all_data, "emissions/Velders_HFC/CP2021_constrProdEmis_ObsAgage_OECD-SSP3")
    KGL2021_ssp3      <- get_data(all_data, "emissions/Velders_HFC/KGL2021_constrProdEmis_ObsAgage_OECD-SSP3")
    KGL2021_ssp5      <- get_data(all_data, "emissions/Velders_HFC/KGL2021_constrProdEmis_ObsAgage_OECD-SSP5")
    iso_Montreal_nonA5_reg <-  get_data(all_data, "emissions/mappings/Montreal_nonA5_GCAMreg")
    L142.pfc_R_S_T_Yh <- get_data(all_data, "L142.pfc_R_S_T_Yh", strip_attributes = T)
    L141.hfc_R_S_T_Yh <- get_data(all_data, "L141.hfc_R_S_T_Yh", strip_attributes = T)
    L141.hfc_ef_R_cooling_Yh <- get_data(all_data, "L141.hfc_ef_R_cooling_Yh", strip_attributes = T)
    L201.Pop_GCAM3 <- get_data(all_data,"L201.Pop_GCAM3", strip_attributes = T)

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
    CP2021_ssp3$scenario <- "CP2021_ssp3"
    CP2021_ssp5$scenario <- "CP2021_ssp5"
    KGL2021_ssp3$scenario <- "KGL2021_ssp3"
    KGL2021_ssp5$scenario <- "KGL2021_ssp5"

    # For Kigali scenarios, substitute in the corresponding production data from
    # current policies as this is the driver that we would want to use to derive
    # future emission factor reductions since this is the baseline activity
    # before Kigali phase out. We, therefore, will be estimating how much
    # emissions factors decrease due to Kigali phase-outs
    KGL2021_ssp3$Prod_tot <- CP2021_ssp3$Prod_tot
    KGL2021_ssp5$Prod_tot <- CP2021_ssp5$Prod_tot
    KGL2021_ssp3$Prod_A5 <- CP2021_ssp3$Prod_A5
    KGL2021_ssp5$Prod_A5 <- CP2021_ssp5$Prod_A5
    KGL2021_ssp3$Prod_nonA5 <- CP2021_ssp3$Prod_nonA5
    KGL2021_ssp5$Prod_nonA5 <- CP2021_ssp5$Prod_nonA5

    FUT_EMISS_GV_NEW <- rbind(CP2021_ssp5,KGL2021_ssp3,KGL2021_ssp5,CP2021_ssp3)
    emissions.GV_ALL_YEARS <- intersect(FUT_EMISS_GV_NEW$Year[unique(FUT_EMISS_GV_NEW$Year)], MODEL_YEARS)
    emissions.GV_FUTURE_YEARS <- intersect(FUT_EMISS_GV_NEW$Year[unique(FUT_EMISS_GV_NEW$Year) > MODEL_FINAL_BASE_YEAR], MODEL_YEARS)

    MAX_DATA_YEAR <- max(intersect(emissions.GV_ALL_YEARS, emissions.HFC_MODEL_BASE_YEARS))

    # In case max_data_year is not in FUT_EMISS_GV (ie timeshift), use minimum year instead
    if(MAX_DATA_YEAR %in% FUT_EMISS_GV_NEW$Year){
      ratio_years <- c(MAX_DATA_YEAR, emissions.GV_FUTURE_YEARS)
    } else {
      ratio_years <-  c(min(FUT_EMISS_GV_NEW$Year), emissions.GV_FUTURE_YEARS)}

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
      # Keep production at max value so the emissions modifier we are calcuating
      # doesn't decline unrealistically
      mutate(Prod_A5 = if_else( Year > MaxA5Year, Prod_A5max, Prod_A5)) %>%
      mutate(Prod_nonA5 = if_else( Year > MaxNonA5Year, Prod_nonA5max, Prod_nonA5)) %>%
      select(-Prod_nonA5max, -Prod_A5max, -MaxA5Year, -MaxNonA5Year) %>%
      rename(year = Year) %>%
      # Format the FUT_EMISS_GV species to be consistent with GCAM names by removing the "-"
      mutate(Species=gsub('HFC-43-10mee', 'HFC43', Species)) %>% # special case with different pattern
      mutate(Species = gsub("-", "", Species)) ->
      FUT_EMISS_GV_FUT

    FUT_EMISS_GV_FUT %>%
      # Define emissions factor as emissions over production
      # This is not exactly correct, since emission banks play a big role, but
      # is the closest we can get to GCAM's activity driven formulation
      mutate( EF = Emis_tot / Prod_tot) %>%
      mutate( EF_nonA5 = Emis_nonA5 / Prod_nonA5) %>%
      mutate( EF_A5 = Emis_A5 / Prod_A5) %>%
      filter(year %in% ratio_years) %>%
      group_by(Species) %>%
      # Calculate EF trends we will use to adjust GCAM's emission trajectory
      # These trends represent Kigali phase downs compared to the CP (no Kigali) scenario
      # We use the regionally differentiated trends below, but the global one is also calculated
      mutate(ratio = EF / EF[year == min(ratio_years)]) %>%
      mutate(ratio_nonA5 = EF_nonA5 / EF_nonA5[year == min(ratio_years)]) %>%
      mutate(ratio_A5 = EF_A5 / EF_A5[year == min(ratio_years)]) %>%
      ungroup() %>%
      select(-Bank_tot, -Mix_tot,  -Bank_nonA5, -Bank_A5, -Emis_tot, -Prod_tot) %>%
      select(-Prod_nonA5, -Prod_A5, -EF, -EF_nonA5, -EF_A5 )  ->
      L241.FUT_EF_Ratio_All

    # Default to the "high" scenario set, although there is not a large difference
    SCEN_HighLow <- "_ssp5"

    # For single scenario selection, indicate that scenario here
    SELECT_SCENARIO <- "KGL2021"
    CP_BASE_SCENARIO <- "CP2021"

    # If this variable is <> 0, then a hybrid scenario
    # If BLEND_FRACT > 0 then scenario is (1-BLEND_FRACT)*Kigali + BLEND_FRACT*CP
    # If BLEND_FRACT < 0 then reduce below Kigali scenario by BLEND_FRACT fraction
    #    by 2100 starting in year FDEC_START_YEAR
    # we create 3 scenarios by repeating driver_drake with 3 different BLEND_FRACT settings
    #
    BLEND_FRACT <- -0.5

    # Select the base F-gas future scenario to use here
    L241.FUT_EF_Ratio_All %>%
      filter(scenario %in% paste0(SELECT_SCENARIO,SCEN_HighLow)) %>%
      filter(year %in% emissions.GV_FUTURE_YEARS)  %>%
      select(-scenario, - ratio, -Emis_nonA5, -Emis_A5) ->
      L241.FUT_EF_Ratio

    #get rid of duplicates (mostly rows with NA in 2025), we only have 16 species times 10 periods...
    L241.FUT_EF_Ratio <- unique(L241.FUT_EF_Ratio)

    L241.FUT_EF_Ratio_base <- L241.FUT_EF_Ratio |> rename(base_nonA5=ratio_nonA5, base_A5 = ratio_A5)

    # Modify if requested (BLEND_FRACT == 0 means just use the selected base scenario)
    if ( BLEND_FRACT != 0){
      if ( BLEND_FRACT > 0 ) { # Branch for incomplete implementation of Kigali
      if( !grepl("KGL",SELECT_SCENARIO) ) stop('Select a Kigali base scenario in order to generate a blended scenario')
      L241.FUT_EF_Ratio_All %>%
        select(-Emis_nonA5, -Emis_A5) %>%
        filter(scenario %in% paste0(CP_BASE_SCENARIO,SCEN_HighLow)) %>% select(-scenario, - ratio) %>%
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
      if( !grepl("KGL",SELECT_SCENARIO) ) stop('Select a Kigali base scenario in order to generate a blended scenario')
      FDEC_START_YEAR <- 2040 # Year that ambition would begin to be strengthened
      # Generate a linear fraction
      L241.FUT_EF_Ratio <- L241.FUT_EF_Ratio %>%
        # Generate a linearly increasing fraction
        group_by(Species) %>%
        mutate(fraction = 1 - (year - FDEC_START_YEAR) / (max(MODEL_YEARS) - FDEC_START_YEAR)*abs(BLEND_FRACT)) %>%
        mutate(fraction = if_else(year < FDEC_START_YEAR, 1, fraction)) %>%
        group_by(Species) %>%
        mutate( ratio_nonA5 = ratio_nonA5 * fraction, ratio_A5 = ratio_A5 * fraction ) #%>%
       # select( -fraction)
    }
     #phase in difference to base scenario (pure Kigali, blend = 0)
      phase_in <- data.frame(year = seq(2025,2100,5),share_blended = c(0,seq(0.05,0.95,0.15),rep(1,8)))
      L241.FUT_EF_Ratio <- L241.FUT_EF_Ratio |> left_join(L241.FUT_EF_Ratio_base,by = c(Species,year)) |> left_join(phase_in)
      L241.FUT_EF_Ratio <- L241.FUT_EF_Ratio |> mutate(ratio_nonA5 = base_nonA5*(1-share_blended) + ratio_nonA5 * share_blended,
                                                       ratio_A5 = base_A5 * (1-share_blended) + ratio_A5 * share_blended)
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
      filter(scenario %in% paste0(SELECT_SCENARIO,SCEN_HighLow)) %>%
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

    # Also scale edgar emission factors to global Velders' totals by same ratio
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

    # ---------------------------------------------------------
    # First create default emission factor trends focusing on population-driven emissions
    # Create default trend for population-based emissions
    L201.Pop_GCAM3 %>%
      group_by(region) %>%
      mutate(pop_trend = totalPop / totalPop[year == MODEL_FINAL_BASE_YEAR]) %>%
      ungroup() %>%
      select(-totalPop) -> L201.Pop_Trends

    L241.FUT_EF_Ratio_All %>%
      filter(scenario %in% paste0(CP_BASE_SCENARIO,SCEN_HighLow)) %>%
      select(-scenario, - ratio) ->
      L241.FUT_EF_Ratio_Base

    # Now calculate Velders emission trends to use for initial period for nonA5 regions
    L241.FUT_EF_Ratio_Base %>%
      select(Species, year, Emis_nonA5, Emis_A5) %>%
      # Here we want the actual emission trends
      group_by(Species) %>%
      mutate( Etrend_nonA5 = Emis_nonA5 / Emis_nonA5[year == MODEL_FINAL_BASE_YEAR]) %>%
      mutate( Etrend_A5 = Emis_A5 / Emis_A5[year == MODEL_FINAL_BASE_YEAR]) %>%
      ungroup() %>%
      mutate( Etrend_nonA5 = if_else(is.nan(Etrend_nonA5),1,Etrend_nonA5)) %>%
      mutate( Etrend_A5 = if_else(is.nan(Etrend_A5),1,Etrend_A5)) %>%
      select(-Emis_nonA5, -Emis_A5) -> Velder_EmTrends

    Velder_EmTrends %>%
      filter(year %in% ratio_years) %>%
      # Use left join because we know rows will not match
      left_join(L201.Pop_Trends,by = c(region,year), relationship = "many-to-many") %>%
      # Use left join since file only marks nonA5 regions
      left_join(iso_Montreal_nonA5_reg, by = c("region")) %>%
      # Now assign appropriate region to EmGrowthScaler
      mutate(EmGrowthScaler = if_else(is.na(marker), Etrend_A5, Etrend_nonA5)) %>%
      select(-Etrend_nonA5, -Etrend_A5, -marker, -GCAM_region_ID ) %>%
      # Now set EF_Growth_Mod as EmGrowthScaler / pop_trend
      # since pop trend is already the default driver in GCAM
      mutate(EF_Growth_Mod = EmGrowthScaler / pop_trend ) %>%
      select(-EmGrowthScaler, -pop_trend) ->
      raw_EF_Growth_Mod

    # Set transition points for country groups where EF trend mod stays
    # constant so growth isn't never-ending. If we don't do this, the ratio of
    # emissions to, for example, floorspace gets really high
    nonA5_transition = MODEL_FINAL_BASE_YEAR
    A5_transition = 2040

    # Incorporate transition years
    raw_EF_Growth_Mod  %>%
      # Use left join since file only marks nonA5 regions
      left_join(iso_Montreal_nonA5_reg, by = c("region")) %>%
      select(-GCAM_region_ID) %>%
      # Now assign transition years
      mutate(transitionYear = if_else(is.na(marker), A5_transition, nonA5_transition)) %>%
      unique() %>%
      # Earlier transition for China
      left_join(A_regions, by = c("region")) %>%
      mutate(transitionYear = if_else(Velders_region=="CHINA", nonA5_transition, transitionYear)) %>%
      select(-marker, -bio_N2O_coef, -GAINS_region, -Velders_region, -MAC_region, -SO2_name,-GCAM_region_ID) %>%
      # Keep modification constant after transition year
      group_by(region, Species) %>%
      mutate(EF_Growth_Mod = if_else(year > transitionYear, EF_Growth_Mod[year==transitionYear], EF_Growth_Mod)) %>%
      ungroup() %>%
      # Add GCAM_region_ID
      left_join_error_no_match(GCAM_region_names, by = "region") %>%
      select(-transitionYear, -region) ->
      EF_Growth_Mod

    # First, create a subset of the cooling emission factors from the max year
    L141.hfc_ef_R_cooling_Yh %>%
      filter(year == MAX_DATA_YEAR) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      L141.hfc_ef_cooling_maxhistyr

    # Create full mapping between meta regions and GCAM regions
    GCAM_region_names %>%
      left_join(iso_Montreal_nonA5_reg) %>%
      rename(meta_region = marker) %>%
      mutate(meta_region = if_else(is.na(meta_region),"A5", "nonA5"))-> Velders_to_GCAM_Reg

    # Expand ratios into one column by GCAM region
    L241.FUT_EF_Ratio %>%
      pivot_longer(c(ratio_nonA5,ratio_A5), values_to = "fut_ratio",names_to = "meta_region") %>%
      mutate(meta_region = if_else(meta_region == "ratio_nonA5", "nonA5", "A5") ) %>%
      dplyr::right_join(Velders_to_GCAM_Reg %>% select(-GCAM_region_ID),
                        by="meta_region",relationship = "many-to-many") %>%
      select(-meta_region) ->
      L241.FUT_EF_Ratio_long

    # Use the future emission factor ratios to scale process emission factors
    L141.hfc_ef_cooling_maxhistyr %>%
      select(-year) %>%
      # Technologies with zero base year value will have zero in future, so can omit
      filter(value != 0) %>%
      # Use left join since will be more columns after match
      left_join(L241.FUT_EF_Ratio_long, , by = c("region", "Non.CO2" = "Species"),
                relationship = "many-to-many") %>%
      # Now add modification for growth rates different than population
      left_join(EF_Growth_Mod,by=c("GCAM_region_ID","year","Non.CO2" = "Species"),relationship = "many-to-many") %>%
      # Use na.omit since this for future EFs, emissions without Velders
      # data will have default growth trend
      na.omit() %>%
      mutate(value = value * fut_ratio * EF_Growth_Mod) %>%
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
      # Technologies with zero base year value will have zero in future, so can omit
      filter(value != 0) %>%
      # Use left join since will be more columns after match
      left_join(L241.FUT_EF_Ratio_long, , by = c("region", "Non.CO2" = "Species"),
                relationship = "many-to-many") %>%
      # Now add modification for growth rates different than population
      left_join(EF_Growth_Mod,by=c("GCAM_region_ID","year","Non.CO2" = "Species"),relationship = "many-to-many") %>%
      # Use na.omit since this for future EFs, emissions without Velders
      # data will have default growth trend
      na.omit() %>%
      mutate(EF_Growth_Mod = if_else(supplysector=="urban processes",EF_Growth_Mod,1)) %>%
      mutate(value = value * EF_Growth_Mod * fut_ratio ) %>%
      # Keep only for future years
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
