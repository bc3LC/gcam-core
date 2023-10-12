# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L101.ag_FAO_R_C_Y
#'
#' Aggregate FAO food consumption, ag production, and harvested area
#' data to GCAM regions and GCAM commodities.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L101.ag_HA_bm2_R_C_Y}, \code{L101.ag_Prod_Mt_R_C_Y}. The corresponding file in the
#' original data system was \code{LA101.ag_FAO_R_C_Y.R} (aglu level1).
#' @details This chunk aggregates FAO food consumption, agricultural production,
#' and harvested area data up to GCAM commodities and GCAM regions. Data is converted
#' from FAO units (tons, hectares) to GCAM units (Mt, Pcal, billion km2). Note that
#' FAO's alfalfa production in the USA is divided by 4 "for consistency with USDA".
#' Note (August 2018 GPK revision) - The FAO production and harvested area are disaggregated
#' to basin PRIOR to aggregation by GCAM region. This reduces the bias from using a single
#' year (around 2000) to disaggregate to basin, in multi-country regions.
#' XZ 03-2022 Food related processing was moved so this chunk only downscales prod and area to GLU level ----
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows distinct filter full_join if_else group_by inner_join left_join mutate right_join select summarise
#' @importFrom tidyr complete drop_na replace_na
#' @author KVC March 2017 (revised August 2018 by GPK); XZ 2022
module_aglu_L101.ag_FAO_R_C_Y <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "aglu/FAO/FAO_ag_items_PRODSTAT",
      FILE = "aglu/LDS/LDS_land_types",
      "L100.FAO_PRODSTAT_TO_DOWNSCAL",
      "L100.LDS_ag_HA_ha",
      "L100.LDS_ag_prod_t")

  MODULE_OUTPUTS <-
    c("L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj",
      "L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj",
      "L101.ag_Yield_kgm2_R_C_Y_GLU_BeforeAdj")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    GLU <- default_share_GLU <- HA_share_GLU <- prod_share_GLU <-
      Category <- LT_SAGE <- LT_HYDE <- countries <- country.codes <-
      item.codes <- element <- element.codes <- GCAM_commodity <-
      value <- GCAM_region_ID <- year <- Mcal_t <- value.y <- value.x <-
      item <- iso <- production <- harvested_area <- GCAM_subsector <- NULL # silence package check.

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # DOWNSCALE UPDATE Aggregate to GCAM regions first ----
    ## Generate LDS_ctry_crop_SHARES ----
    # we downscale the data from countries to basins, using the basin-within-country shares
    # of each GCAM commodity in the Monfreda (pre-processed by LDS) data on harvested area and production
    # Note - using GCAM commodities and regions. This avoids dropping data, particularly
    # for the grass fodder crops which are poorly matched with the FAO data.

    # Method update:
    # Instead of downscaling production, we will create yield index and check/correct the distribution first
    # The goal here is to ensure yield after downscaling makes sense

    L100.LDS_ag_HA_ha %>% rename(harvested_area = value) %>%
      left_join_error_no_match(L100.LDS_ag_prod_t %>% rename(production = value),
                               by = c("iso", "GLU", "GTAP_crop")) %>%                                           # Join the Monfreda/LDS datasets of production and harvested area
      left_join(FAO_ag_items_PRODSTAT[c("GTAP_crop", "GCAM_commodity", "GCAM_subsector")], by = "GTAP_crop") %>%                  # Join in the GCAM commodities and aggregate.
      drop_na(GCAM_commodity) %>%   # drop any crops not considered in GCAM
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU) %>%
      summarise(harvested_area = sum(harvested_area),
                production = sum(production)) %>%
      ungroup() %>%
      mutate(yield = production / harvested_area) ->
      LDS_ctry_crop_AreaProdYield

    # Calculate region-average yield
    LDS_ctry_crop_AreaProdYield %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector) %>%
      summarize(yield_regional = sum(production) / sum(harvested_area),
                production = sum(production), harvested_area = sum(harvested_area)) %>%
      ungroup() %>%
      group_by(GCAM_commodity, GCAM_subsector) %>%
      mutate(yield_world = sum(production) / sum(harvested_area)) %>%
      ungroup() %>%
      select(-production, -harvested_area) ->
      LDS_ctry_crop_Yield_Regional_World

    # Add yield index relative to the regional-average
    LDS_ctry_crop_AreaProdYield %>%
      left_join_error_no_match(LDS_ctry_crop_Yield_Regional_World,
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector")) %>%
      mutate(yield_index_Rel_regional = yield / yield_regional,
             yield_index_Rel_world = yield / yield_world) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector) %>%
      mutate(HA_share_GLU = harvested_area / sum(harvested_area),                                               # Compute the shares of country/crop/GLU within country/crop
             prod_share_GLU = production / sum(production)) %>%
      ungroup() ->
      LDS_ctry_crop_SHARES

    LDS_ctry_crop_SHARES %>% #filter(GCAM_subsector == "CornC4") %>%
      mutate(yield_index_Rel_world = pmin(5, yield_index_Rel_world)) %>%
      ggplot() +
      geom_boxplot(aes(x = GCAM_subsector, y = yield_index_Rel_world,
                       fill = GCAM_subsector)) +
      labs(x = "GCAM crops", y = "Tonne per ha") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 40, hjust = 1),
        legend.position = "none") -> p;p


    gcam.REGION_NUMBER <- iso_GCAM_regID %>% distinct(GCAM_region_ID) %>% nrow
    assertthat::assert_that(unique(LDS_ctry_crop_SHARES$GCAM_region_ID) %>%
                              length == gcam.REGION_NUMBER,
                            msg = "Not all GCAM regions are downscaled to GLU for production and area due to missing share info." )

    # Compute default basin-within-country shares to be used where FAOSTAT has data but LDS/Monfreda does not.
    # These shares are computed from the harvested area of all crops available in Monfreda.
    # Harvested area is used to avoid compositional bias from different crop types in different basins.
    LDS_ctry_crop_SHARES %>%
      group_by(GCAM_region_ID, GLU) %>%
      summarise(harvested_area = sum(harvested_area)) %>%
      ungroup() %>%
      group_by(GCAM_region_ID) %>%
      mutate(default_share_GLU = harvested_area / sum(harvested_area)) %>%
      ungroup() %>%
      select(GCAM_region_ID, GLU, default_share_GLU) ->
      LDS_ctry_SHARES


    # If downscaling production and area respectively, the yield will be the same with LDS source data
    # However, the yield in LDS/Monfreda has low quality and unreasonably large variation
    # We will still use area downscaling method but yield index and correct it first to calculate GLU production

    LDS_ctry_crop_SHARES <-
      select(LDS_ctry_crop_SHARES, GCAM_region_ID, GLU, GCAM_commodity,
             GCAM_subsector, HA_share_GLU, prod_share_GLU, yield_index_Rel_world)

    L100.FAO_PRODSTAT_TO_DOWNSCAL %>%
      mutate(FAOYield_tPerha_iso = if_else(Area_harvested_ha == 0, 0, Prod_t / Area_harvested_ha)) %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(FAOArea_harvested_ha = sum(Area_harvested_ha),
                FAOProd_t = sum(Prod_t),
                FAOYield_tPerha_iso_regionalmax = max(FAOYield_tPerha_iso)) %>%
      mutate(FAOYield_tPerha_regional = FAOProd_t / FAOArea_harvested_ha) %>%
      replace_na(list(FAOYield_tPerha_regional = 0)) %>%
      ungroup() %>%
      group_by(GCAM_commodity, GCAM_subsector, year) %>%
      mutate(FAOYield_tPerha_world = sum(FAOProd_t) / sum(FAOArea_harvested_ha)) %>%
      ungroup() %>%
       # clean outliers in FAOYield_tPerha_iso_regionalmax
       # when iso max is > than 2* regional yield and FAOYield_tPerha_iso_regionalmax > 3* world, use regional
      # FAOYield_Ratio_over_world_mean will be used as ceiling later
      mutate(  FAOYield_tPerha_iso_regionalmax = if_else(
               FAOYield_tPerha_iso_regionalmax > 3 * FAOYield_tPerha_world &
               FAOYield_tPerha_iso_regionalmax > 2 * FAOYield_tPerha_regional,
               FAOYield_tPerha_regional, FAOYield_tPerha_iso_regionalmax),
             FAOYield_Ratio_over_world_mean = FAOYield_tPerha_iso_regionalmax / FAOYield_tPerha_world)  ->
      L100.FAO_PRODSTAT_TO_DOWNSCAL_R

    ### First group: crops and countries in BOTH datasetes (LDS/Monfreda and FAOSTAT) ----


    L100.FAO_PRODSTAT_TO_DOWNSCAL_R %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, FAOProd_t,
             FAOArea_harvested_ha, FAOYield_Ratio_over_world_mean, FAOYield_tPerha_world,
             FAOYield_tPerha_regional) %>%
      # using inner join here instead of right join since LDS_ctry_crop_SHARES has fodder crops not in L100.FAO_PRODSTAT_TO_DOWNSCAL_R
      inner_join(LDS_ctry_crop_SHARES,
                 by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector")) %>%
      # Downscale area using area share
      mutate(GLUArea_harvested_ha = FAOArea_harvested_ha * HA_share_GLU) %>%

      # Update the new method to correct yield distribution
      # a Modify the new method to adjust yield_index_Rel_world to set a max yield ratio to FAOYield_Ratio_over_world_mean
    # and redo the method method calculation above
      mutate(yield_index_Rel_world = pmin(FAOYield_Ratio_over_world_mean, yield_index_Rel_world),
             #yield_index_Rel_world = 1,
             GLUYield_tPerha = FAOYield_tPerha_world * yield_index_Rel_world) %>%
      # trimming regional distribution
      # If GLUYield_tPerha is larger than n * regional average, setting to regional value

     # b.Modify the new method to adjust yield_index_regional to set a max yield ratio to Yield_Ratio_Max_over_Mean
    # E.g., USA corn max county yield is about 20-40% higher than national average

      mutate(GLUYield_tPerha = pmin(FAOYield_tPerha_regional * 1.3, GLUYield_tPerha)) %>%

      # So far, there could still be outliers
      # e.g., one basin in Indonesia had much higher fruit yield than other basins in the region
      # will set outliers to median in a basin
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      mutate(CLU_median = median(GLUYield_tPerha)) %>% ungroup %>%
      group_by(GCAM_commodity, GCAM_subsector, year) %>%
      mutate(outlier_thredshold = quantile(GLUYield_tPerha, 0.75) + 1.5 * IQR(GLUYield_tPerha) ) %>%   #Tukey
      ungroup %>%
      mutate(GLUYield_tPerha = if_else(GLUYield_tPerha > outlier_thredshold,
                                       CLU_median, GLUYield_tPerha),
             GLUProd_t = GLUArea_harvested_ha * GLUYield_tPerha) %>%


      # However, the regional production will possibly be smaller than FAO due to the ceiliing
      # We will need to scale all GLU production (and thus yield) up to mathch FAO
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      mutate(GLUProd_t_regional_to_scale = sum(GLUProd_t),
             RegtionaScaler_FAO_LDS = if_else(GLUProd_t_regional_to_scale == 0, 0,
                                              FAOProd_t / GLUProd_t_regional_to_scale),
             GLUProd_t_ScaleToFAORegional = GLUProd_t * RegtionaScaler_FAO_LDS) %>%
      ungroup() %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year, GLU,
             Area_harvested_ha = GLUArea_harvested_ha,
             Prod_t = GLUProd_t_ScaleToFAORegional) ->
      FAO_PRODSTAT_DOWNSCALED_matches

    # The new production downscaling method using yield info was not applied to the second group here
    # Not many crops are in the second crop and yield is assumed to be the same across a region
    # since only area share was used and applied to both area and production
    ### Second group: country/crop observations missing in LDS/Monfreda where some crops for the country are available----
    L100.FAO_PRODSTAT_TO_DOWNSCAL %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, year) %>%
      summarise(Area_harvested_ha = sum(Area_harvested_ha),
                Prod_t = sum(Prod_t)) %>%
      ungroup() %>%
      anti_join(LDS_ctry_crop_SHARES, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector")) %>%                                        # Filter the dataset to only observations where the country and crop couldn't be matched
       full_join(LDS_ctry_SHARES, by = "GCAM_region_ID") %>%
      drop_na() %>%                                                                                               # Drop places where entire country is not available in LDS/Monfreda data
      mutate(Area_harvested_ha = Area_harvested_ha * default_share_GLU,                                                 # multiply through by shares of GLU within country and crop
             Prod_t = Prod_t * default_share_GLU) %>%
      select(-default_share_GLU) ->
      FAO_PRODSTAT_DOWNSCALED_cropNA

    FAO_PRODSTAT_DOWNSCALED_new <- bind_rows(FAO_PRODSTAT_DOWNSCALED_matches,
                                         FAO_PRODSTAT_DOWNSCALED_cropNA)


    # Assert that FAO_PRODSTAT_DOWNSCALED_new match input data L100.FAO_PRODSTAT_TO_DOWNSCAL ----
    FAO_PRODSTAT_DOWNSCALED_new %>%
      group_by(GCAM_commodity, year) %>%
      summarise(Prod_out_Mt = sum(Prod_t),
                Area_out_Ha = sum(Area_harvested_ha)) %>%
      ungroup() %>%
      left_join(
        L100.FAO_PRODSTAT_TO_DOWNSCAL %>%
          group_by(GCAM_commodity, year) %>%
          summarise(Prod_in_Mt = sum(Prod_t),
                    Area_in_Ha = sum(Area_harvested_ha)) %>%
          ungroup(), by = c("GCAM_commodity", "year")) %>%
      mutate(Prod_diff_Mt = Prod_in_Mt  - Prod_out_Mt,
             Area_diff_Mt = Area_in_Ha  - Area_out_Ha) %>%
      filter(abs(Prod_diff_Mt) > 0.01 | abs(Area_diff_Mt) > 0.01) ->
      in_out_prod_area
    assertthat::assert_that(nrow(in_out_prod_area) == 0, msg = "Check inconsistency in Production downscale to GLU.")


    # Process FAO production data: convert units, aggregate to region, commodity, and GLU
    ##* L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj ----
    FAO_PRODSTAT_DOWNSCALED_new %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, Prod_t) %>%                                                    # Select relevant columns (not harvested_area)
      rename(value = Prod_t) %>%                                                                            # Rename column since tests are expecting "value"                                                                     # Aggregate then map to appropriate data frame
      mutate(value = value * CONV_TON_MEGATON) %>%                                                              # Convert from tons to Mt
      ungroup() ->                                                                                              # Ungroup before complete
      L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj



    # Now, Process FAO harvested area data: convert units, aggregate to region, commodity, and GLU
    ##* L101.ag_HA_bm2_R_C_Y_GLU ----
    FAO_PRODSTAT_DOWNSCALED_new %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, Area_harvested_ha) %>%                                              # Select relevant columns (not production)
      rename(value = Area_harvested_ha) %>%                                                                      # Rename column since tests are expecting "value"
      mutate(value = value * CONV_HA_BM2) %>%                                                                # Convert from hectares to billion m2
      ungroup() ->                                                                                           # Ungroup before complete
      L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj


    # Calculate initial yield estimates in kilograms per square meter by region, crop, year, and GLU
    # Yield in kilograms per square meter
    ##* L101.ag_Yield_kgm2_R_C_Y_GLU ----
    L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj %>% rename(Prod = value) %>%
      left_join(L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj %>% rename(Area = value),
                by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      mutate(value = Prod / Area) %>%
      replace_na(list(value = 0)) %>%
      select(-Prod, -Area) %>%
      arrange(GLU) ->  # so we match old d.s. order
      L101.ag_Yield_kgm2_R_C_Y_GLU_BeforeAdj

    L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj %>%
      add_title("Harvested area by GCAM region, commodity, year, and GLU") %>%
      add_units("billion km2") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region, commodity, and GLU") %>%
      add_comments("Data was also converted from HA to billion km2") %>%
      add_legacy_name("L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj") %>%
      add_precursors("L100.FAO_PRODSTAT_TO_DOWNSCAL", "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_HA_ha", "common/iso_GCAM_regID") ->
      L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj
    L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj %>%
      add_title("Agricultural production by GCAM region, commodity, year, and GLU") %>%
      add_units("Mt/yr") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region, commodity, and GLU") %>%
      add_comments("Data was also converted from tons to Mt") %>%
      add_comments("USA alfalfa production was divided by 4 for consistency with USDA") %>%
      add_comments("Country/crop combinations with zero harvested area were assigned zero production") %>%
      add_legacy_name("L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj") %>%
      add_precursors("L100.FAO_PRODSTAT_TO_DOWNSCAL", "aglu/FAO/FAO_ag_items_PRODSTAT",
                     "L100.LDS_ag_prod_t", "common/iso_GCAM_regID") ->
      L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj

    L101.ag_Yield_kgm2_R_C_Y_GLU_BeforeAdj %>%
      add_title("Unadjusted agronomic yield by GCAM region / commodity / year / GLU") %>%
      add_units("kg/m2") %>%
      add_comments("Agricultural yield computed based on production and harvested area") %>%
      add_legacy_name("L101.ag_Yield_kgm2_R_C_Y_GLU_BeforeAdj") %>%
      same_precursors_as(L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj) ->
      L101.ag_Yield_kgm2_R_C_Y_GLU_BeforeAdj

    # Return data ----
    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
