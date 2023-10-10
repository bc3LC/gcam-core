# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L122.LC_R_Cropland_Yh_GLU
#'
#' Integrate disparate data sources for land cover and harvested area from FAO, Monfreda, and Hyde in
#' order to calculate cropland cover by specific crop type, other arable land, harvested:cropped ratio, economic
#' yield (i.e., production per unit cropland area per year, rather than production per unit area per harvest), and
#' "extra" cropland that needs to be taken from other land use types. All data tables are written out
#' at the GCAM region-GLU level for each historical year, including carbon cycle model spinup years.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L122.ag_HA_bm2_R_Y_GLU_AnnualCHF}, \code{L122.ag_EcYield_kgm2_R_C_Y_GLU},
#'                        \code{L122.LC_bm2_R_OtherArableLand_Yh_GLU}, \code{L122.LC_bm2_R_ExtraCropLand_Yh_GLU},
#'                        \code{L122.LC_bm2_R_HarvCropLand_C_Yh_GLU}, \code{L122.LC_bm2_R_HarvCropLand_Yh_GLU}.
#' The corresponding file in the original data system was \code{LB122.LC_R_Cropland_Yh_GLU.R} (aglu level1).
#' @details First, Hyde cropland cover is aggregated to the region-GLU level in each year, and supplemented with Monfreda
#' harvested area information when necessary. Second, FAO fallowland, cropland, and harvested area are aggregated from
#' the country level to the region level, and used to calculate the "residual" cropland in each land use region:
#' total cropland (Hyde) - known fallow (FAO) - harvested crops (FAO/Monfreda).
#' The harvested area : cropland ratio (HA:CL) is computed as (sum of harvested area) / (cropland - fallow land).
#' Where HA:CL < 1, HA:CL is set to 1, and the balance (the "residual" above) is added to OtherArableLand.
#' Where HA:CL > 3, HA:CL is set to 3, and additional cropland (ExtraCropLand) is written out, to be taken from
#' other land use types (determined in different code chunks).
#' @importFrom assertthat assert_that
#' @importFrom dplyr anti_join arrange bind_rows distinct filter if_else group_by left_join mutate select summarise summarise_all
#' @importFrom tidyr replace_na
#' @author ACS April 2017 XZ 2023
module_aglu_L122.LC_R_Cropland_Yh_GLU <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      "L100.FAO_fallowland_kha",
      "L100.FAO_CL_kha",
      "L120.LC_bm2_R_LT_Yh_GLU",
      "L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj",
      "L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj")

  MODULE_OUTPUTS <-
    c("L101.ag_HA_bm2_R_C_Y_GLU",
      "L101.ag_Prod_Mt_R_C_Y_GLU",
      "L101.ag_HA_bm2_R_C_Y",
      "L101.ag_Prod_Mt_R_C_Y",
      "L122.ag_HA_bm2_R_Y_GLU_AnnualCHF",
      "L122.ag_EcYield_kgm2_R_C_Y_GLU",
      "L122.LC_bm2_R_OtherArableLand_Yh_GLU",
      "L122.LC_bm2_R_ExtraCropLand_Yh_GLU",
      "L122.LC_bm2_R_HarvCropLand_C_Yh_GLU",
      "L122.LC_bm2_R_HarvCropLand_Yh_GLU")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    Land_Type <- year <- . <- GCAM_region_ID <- GLU <- GCAM_commodity <-
      value <- iso <- countries <- cropland <- fallow <- fallow_frac <- cropped <-
      cropped_frac <- uncropped_frac <- nonharvested_frac <-
      GCAM_subsector <- NULL # silence package check.

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Get harvested area ready ----

    # In this process we will keep an eye on Perennial
    # Sugar crops could be included as Perennial here
    Perennial_Identifier <- "Tree|SugarCrop"

    # Aggregate FAO harvested area in GLU, distinguished by Perennial or not
    # We will calculate harvest frequency for Annual as Perennial has a frequency of 1
    L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj %>%
      mutate(type = if_else(grepl(Perennial_Identifier, GCAM_subsector), "Perennial", "Annual")) %>%
      group_by(GCAM_region_ID, type, GLU, year) %>%
      summarise(value = sum(value)) %>% ungroup() %>%
      spread(type, value) %>%
      replace_na(list(Perennial = 0)) ->
      L122.ag_HA_bm2_R_Y_GLU_Perennial_Annual


    # Get cropland cover ready ----
    # take a subset of the land cover table L120.LC_bm2_R_LT_YH_GLU: cropland, and only in aglu historical years
    L120.LC_bm2_R_LT_Yh_GLU %>%
      filter(Land_Type == "Cropland", year %in% aglu.AGLU_HISTORICAL_YEARS) ->
      # store in table Land Cover in bm2 by Region, Year and GLU for Cropland.
      L122.LC_bm2_R_CropLand_Y_GLU

    # The harvested area/production tables L103.ag.X (from Monfreda) may have R_GLUs not in the cropland table
    # L122.LC_bm2_R_CropLand_Y_GLU (from Hyde), and vice versa.
    # Fill out the cropland table to include all R_GLUs in Monfreda:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      # find the region-GLU combos in the Monfreda harvested area table L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj NOT contained
      # in the L122.LC_bm2_R_CropLand_Y_GLU land cover table:
      anti_join(L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj[,c("GCAM_region_ID", "GLU", "year")], .,
                by = c("GCAM_region_ID", "GLU", "year")) %>%
      # save only the unique combinations:
      unique() %>%
      # arrange so rows occur in more sensible order:
      arrange(GCAM_region_ID, GLU, year) %>%
      # add values of 0 everywhere for now:
      mutate(value = 0,
             # add a Land_Type identifier:
             Land_Type = "Cropland") %>%
      # add these rows to the original Land Cover table:
      bind_rows(L122.LC_bm2_R_CropLand_Y_GLU) ->
      # save as the Land C over table:
      L122.LC_bm2_R_CropLand_Y_GLU


    # Get fallow area ready ----
    # Calculate fallow_frac in cropland based on FAO data
    # And apply it to land data (moriai) L122.LC_bm2_R_CropLand_Y_GLU
    # The assumption is that fallow share is that same across GLU

    # Take the FAO cropland table, L100.FAO_CL_kha:
    L100.FAO_CL_kha %>%
      # only include data in the right fallow land year range
      # keep only the iso country and the value for each:
      select(iso, area_code, Arableland = value, year) %>%
      left_join_error_no_match(
        L100.FAO_fallowland_kha %>% rename(fallow = value),
        by = c("iso", "area_code", "year")) %>%
      na.omit() %>%
      select(GCAM_region_ID, Arableland, fallow, year) %>%
      ungroup() %>%
      # aggregate Arableland and fallow values to the GCAM region level:
      group_by(GCAM_region_ID, year) %>%
      summarise_all(sum) %>%
      left_join_error_no_match(
        L122.ag_HA_bm2_R_Y_GLU_Perennial_Annual %>%
          group_by(GCAM_region_ID, year) %>%
          # conv to Kha
          summarize(Annual = sum(Annual) * 100, Perennial = sum(Perennial) * 100),
        by = c("GCAM_region_ID", "year")
      ) %>%
      # calculate the fraction of total land in each GCAM region that is fallow:
      mutate(fallow_frac = fallow / Arableland,
             FAOAnnual = Arableland - fallow - Perennial,
             FAOAnnualCHF = Annual / FAOAnnual) ->
      # store in a table of Arableland, fallow information by region:
      L122.cropland_fallow_FAOCHF_Y_R

    # take cropland table with region, landtype and glu information for each year:
    L122.LC_bm2_R_CropLand_Y_GLU %>%
      left_join_error_no_match(
        L122.cropland_fallow_FAOCHF_Y_R %>% select(GCAM_region_ID, year, fallow_frac),
        by = c("GCAM_region_ID", "year")) %>%
      transmute(GCAM_region_ID, GLU, year,
                value = value * fallow_frac,
                Land_Type = "FallowLand") ->
      L122.LC_bm2_R_FallowLand_Y_GLU




    # Connect data tables ----

    # Connect land data table to include harvested area, fallow and cropland cover
    # Available cropland = totalcropland from L122.LC_bm2_R_CropLand_Y_GLU - fallowland from L122.LC_bm2_R_FallowLand_Y_GLU

    L122.ag_HA_bm2_R_Y_GLU_Perennial_Annual %>%
      left_join_error_no_match(
        L122.LC_bm2_R_CropLand_Y_GLU %>% spread(Land_Type, value),
        by = c("GCAM_region_ID", "GLU", "year")) %>%
      left_join_error_no_match(
        L122.LC_bm2_R_FallowLand_Y_GLU %>% spread(Land_Type, value),
        by = c("GCAM_region_ID", "GLU", "year")) %>%
      mutate(AvailCropland = Cropland - FallowLand) ->
      L122.ag_HA_bm2_R_Y_GLU


    # Compute CHF, HarvCropLand_required, and check ----

    ## Constraints for the minimum and maximum harvested:cropped ratios ----
    # Source: Dalrymple, D.G. 1971, Survey of Multiple Cropping in Less Developed Nations, Foreign Econ. Dev. Serv., U.S. Dep. of Agricul., Washington, D.C.
    # Cited in: Monfreda et al. 2008, Farming the Planet: 2., Global Biogeochemical Cycles 22, GB1022, http://dx.doi.org/10.1029/2007GB002947
    aglu.MAX_HA_TO_CROPLAND_Annual = 2
    aglu.MIN_HA_TO_CROPLAND_Annual = 1


    L122.cropland_fallow_FAOCHF_Y_R %>%
      filter(year == max(MODEL_BASE_YEARS)) %>%
      select(GCAM_region_ID, FAOAnnualCHF) %>%
      mutate(FAOAnnualCHF = if_else(FAOAnnualCHF < 0, aglu.MAX_HA_TO_CROPLAND_Annual, FAOAnnualCHF),
             # Two regions had negative values Indonesia and Colombia, setting to max values
             FAOAnnualCHF = pmax(aglu.MIN_HA_TO_CROPLAND_Annual, FAOAnnualCHF),
             FAOAnnualCHF = pmin(aglu.MAX_HA_TO_CROPLAND_Annual, FAOAnnualCHF)) ->
      L122.FAO_AnnualCrop_CHF_R





    # Note that two regions GCAM_region_ID == 3 & GLU == "GLU087") | (GCAM_region_ID == 30 & GLU == "GLU078"
    # had issues in LB124 "Increase in cropland exceeds available unmanaged land"
    # These two regions have no issues anymore with the new adjustments


     L122.ag_HA_bm2_R_Y_GLU %>%
       # conservative approach of using adjusted FAO CHF as max value
       left_join_error_no_match(L122.FAO_AnnualCrop_CHF_R) %>%
       mutate(Cropland_min = Annual / FAOAnnualCHF + Perennial)  %>%

      # Old approach
      # mutate(Cropland_min = Annual / aglu.MAX_HA_TO_CROPLAND_Annual + Perennial,
      #        Cropland_min = if_else( (GCAM_region_ID == 3 & GLU == "GLU087"), Annual / 1.6 + Perennial, Cropland_min))  %>%

      # Update HarvCropLand_required which should not be smaller than Cropland_min to make sense fo CHF
      # Calculate the harvested to cropped land ratio for all crops, by region, year, and GLU
      # applying minimum and maximum harvested:cropped ratios
      # Maximum harvested:cropped ratio may cause cropland to expand
      # This additional cropland will need to be balanced by a deduction from other land types later on, so it is tracked below
      mutate(HarvCropLand_required = pmax(Cropland_min, AvailCropland),
             AnnualCropHarvestFrequency = Annual / (HarvCropLand_required - Perennial),
             AnnualCropHarvestFrequency = pmax(aglu.MIN_HA_TO_CROPLAND_Annual, AnnualCropHarvestFrequency),
             HarvCropLand = Annual / AnnualCropHarvestFrequency + Perennial) %>%
      select(-Cropland_min, -HarvCropLand_required) ->
      L122.ag_HA_bm2_R_Y_GLU_HarvCropLand

    # Calculating OtherArableLand ----

    # The minimum threshold on HA:CL means that some cropland in Hyde will not be assigned to a crop. This is mapped to "other arable land"
    # The maximum threshold on HA:CL means that cropland in Hyde in some ag regions may be less than cropland in GCAM
    # In the latter case, land needs to be mapped to cropland from other uses.
    # The first step in executing the three above lines of comment is to calculate the residual land cover that is
    # cropland (may be positive or negative): residual = cropland - fallow land - harvested area by crop type

    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand %>%
      mutate(# Calculating unused cropland; this is added with fallow land to calculate other arable land
             # Where residuals are negative, this is "unused" cropland that will be mapped to other arable land
             UnusedCropLand = pmax(0, AvailCropland - HarvCropLand),
             # Calculating extra cropland; this will be balanced by a deduction from unmanaged lands
             # Where residuals are positive, this is "extra" land that will later be deducted from other categories.
             ExtraCropLand = pmax(0, HarvCropLand - AvailCropland),
             OtherArableLand = FallowLand + UnusedCropLand) ->
      L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_0



    # More adjustments to move production and area across basin within a region ----
    # this is because LDS land cover is inconsistent with LDS harvested area
    # CHF could be crazy in some region, e.g., Africa_Northern

    # Here we create method to shift land area and production across basins within a region
    # The key is to maintain total cropland in a basin unchanged by making use UnusedCropLand
    # UnusedCropLand indeed explains such basin-level difference


    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_0 %>%
      # filter relevant basins that will need adjustments
      # when ExtraCropLand == 0 & UnusedCropLand == 0 no adjustment is needed and they will be bind back
      filter(!(ExtraCropLand == 0 & UnusedCropLand == 0)) %>%
      #filter(GCAM_region_ID %in% c(11), year == 2007) %>%
      group_by(year, GCAM_region_ID) %>%
      # Calculate the total areas for potential adjustments
      # Need to consider CHF here for ExtraCropLand
      mutate(totalUnused = sum(UnusedCropLand),
             totalExtraNeeded = sum(ExtraCropLand * AnnualCropHarvestFrequency)) %>%
      # Calculate shares
      mutate(totalUnusedShare = UnusedCropLand / totalUnused,
             totalExtraNeededShare = ExtraCropLand * AnnualCropHarvestFrequency / totalExtraNeeded) %>%
      replace_na(list(totalUnusedShare = 0,
                      totalExtraNeededShare = 0)) %>%
      # min of (totalUnused, totalExtraNeeded) will be adjusted/moved
      # total MoveIn = total MoveOut
      mutate(MoveIn = totalUnusedShare * pmin(totalUnused, totalExtraNeeded),
             MoveOut = totalExtraNeededShare * pmin(totalUnused, totalExtraNeeded),
             AnnualMoveOutShare = MoveOut / Annual,
             AnnualMoveInShare = MoveIn / Annual) %>%
      ungroup() ->
      L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_1

    # Adding a check which should have no concern
    assertthat::assert_that(L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_1 %>%
                              filter(AnnualMoveOutShare >=1) %>% nrow == 0,
                            msg = "Perennial is larger than available cropland")

    # Move the land with in a region across basins
    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_1 %>%
      mutate(Annual = Annual + MoveIn - MoveOut,
             # update key land covers
             UnusedCropLand0 = UnusedCropLand - MoveIn,
             ExtraCropLand0 = ExtraCropLand - MoveOut,
             HarvCropLand = Annual / AnnualCropHarvestFrequency + Perennial,
             UnusedCropLand = pmax(0, AvailCropland - HarvCropLand),
             ExtraCropLand = pmax(0, HarvCropLand - AvailCropland),
             OtherArableLand = FallowLand + UnusedCropLand) %>%
      select(names(L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_0) %>%
               c("AnnualMoveOutShare", "AnnualMoveInShare") )->
      L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_2

    # Bind basins didn't need the adjustments
    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_2 %>%
      bind_rows(
      L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others_adj_0 %>%
        filter((ExtraCropLand == 0 & UnusedCropLand == 0)) %>%
        mutate(AnnualMoveOutShare = 0, AnnualMoveInShare = 0)) ->
    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others


    ## update crop level area here ----
    L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj %>%
      mutate(type = if_else(grepl(Perennial_Identifier, GCAM_subsector),
                            "Perennial", "Annual")) %>%
      filter(type == "Annual") %>%
      left_join_error_no_match(
        L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others %>%
          select(GCAM_region_ID, GLU, year, AnnualMoveOutShare, AnnualMoveInShare) %>%
          mutate(type = "Annual"),
        by = c("GCAM_region_ID", "GLU", "year", "type")
      ) %>%
      # Make the move at the crop level
      mutate(value = value * (1 - AnnualMoveOutShare) * (1 + AnnualMoveInShare)) %>%
      # bind Perennial back
      bind_rows(
        L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj %>%
          mutate(type = if_else(grepl(Perennial_Identifier, GCAM_subsector),
                                "Perennial", "Annual")) %>%
          filter(type == "Perennial")
      ) %>%
      select(names(L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj)) ->
      L101.ag_HA_bm2_R_C_Y_GLU


    ##* L101.ag_HA_bm2_R_C_Y ----
    L101.ag_HA_bm2_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0)) ->                                               # Fill in missing region/commodity combinations with 0
      L101.ag_HA_bm2_R_C_Y


    ## update crop level production here ----
    L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj %>%
      mutate(type = if_else(grepl(Perennial_Identifier, GCAM_subsector),
                            "Perennial", "Annual")) %>%
      filter(type == "Annual") %>%
      left_join_error_no_match(
        L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others %>%
          select(GCAM_region_ID, GLU, year, AnnualMoveOutShare, AnnualMoveInShare) %>%
          mutate(type = "Annual"),
        by = c("GCAM_region_ID", "GLU", "year", "type")
      ) %>%
      # Make the move at the crop level
      mutate(value = value * (1 - AnnualMoveOutShare) * (1 + AnnualMoveInShare)) %>%
      # bind Perennial back
      bind_rows(
        L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj %>%
          mutate(type = if_else(grepl(Perennial_Identifier, GCAM_subsector),
                                "Perennial", "Annual")) %>%
          filter(type == "Perennial")
      ) %>%
      select(names(L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj)) ->
      L101.ag_Prod_Mt_R_C_Y_GLU

    # Also write out the production volumes without basin-level detail, or subsector differentiation (i.e. by region, crop, year)
    ##* L101.ag_Prod_Mt_R_C_Y ----
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, year) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      complete(GCAM_region_ID = unique(iso_GCAM_regID$GCAM_region_ID),
               GCAM_commodity, year, fill = list(value = 0))  ->                                                # Fill in missing region/commodity combinations with 0
      L101.ag_Prod_Mt_R_C_Y


    # Pull data table that will be used later

    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others %>%
      select(GCAM_region_ID, GLU, year, AnnualCropHarvestFrequency) ->
      L122.ag_HA_bm2_R_Y_GLU_AnnualCHF

    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others %>%
      select(GCAM_region_ID, GLU, year, ExtraCropLand) %>%
      gather(Land_Type, value, ExtraCropLand) ->
      L122.LC_bm2_R_ExtraCropLand_Y_GLU


    # Update OtherArableLand ----
    # Deal with area that had land cover but no crops
    # setting entirely to OtherArableLand
    L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others %>%
      select(GCAM_region_ID, GLU, year, OtherArableLand) %>%
      # join the cropland info so that can handle NA's
      right_join(L122.LC_bm2_R_CropLand_Y_GLU %>% spread(Land_Type, value),
                by = c("GCAM_region_ID", "GLU", "year")) %>%
      # Assigning cropland to other arable land wherever harvested area is zero
      # If there are any land use regions with 0 harvested area (Monfreda) but positive cropland cover (Hyde), these are missing
      # values in the above table, and all of this cropland should be assigned to other arable land.
      mutate(OtherArableLand = if_else(is.na(OtherArableLand), Cropland, OtherArableLand)) %>%
      select(-Cropland) %>%
      gather(Land_Type, value, OtherArableLand) ->
      L122.LC_bm2_R_OtherArableLand_Y_GLU


    # FAO harvested area for all crops is adjusted to cover here ----

    # The ag_HA_to_CropLand ratio is assumed to be a property of the region and GLU (not specific to individual crops)
    # Calculate cropland requirements of each crop as harvested area divided by regional ag_HA_to_CropLand ratio

    L101.ag_HA_bm2_R_C_Y_GLU %>%
        mutate(type = if_else(grepl(Perennial_Identifier, GCAM_subsector), "Perennial", "Annual")) %>%
      # join the harvested area to cropland ratios, L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others:
      left_join_error_no_match(L122.ag_HA_bm2_R_Y_GLU_HarvCropLand_Others %>%
                                 select(GCAM_region_ID, GLU, year, CropHarvestFrequency = AnnualCropHarvestFrequency),
                               by = c("GCAM_region_ID", "GLU", "year") ) %>%
      # replace CropHarvestFrequency for Perennial with 1
      mutate(CropHarvestFrequency = if_else(type == "Perennial", 1, CropHarvestFrequency),
             # calculate the harvested cropland for each commodity as value = value.x/value.y:
             value = value / CropHarvestFrequency,
             # add a Land_Type identifier:
             Land_Type = "HarvCropLand") ->
      # store in a table of Harvested Cropland by region-commodity-glu-year:
      L122.LC_bm2_R_HarvCropLand_C_Y_GLU


    # Aggregate across crops to get harvested cropland area, by region/GLU/year
    # The harvested cropland area by region-commodity-glu-year table, L122.LC_bm2_R_HarvCropLand_C_Y_GLU,
    # is used for other calculations below. That is why this is not part of the previous pipeline
    # Take harvested cropland area by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      group_by(GCAM_region_ID, GLU, Land_Type, year) %>%
      summarise(value = sum(value)) ->
      # store in a table of Harvested cropland by region-glu-year:
      L122.LC_bm2_R_HarvCropLand_Y_GLU


    # Update "yield" wrt land cover accordingly ----
    ## Not used later as it is updated again later ----
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>% rename(cover = value) %>%
      select(-Land_Type) %>%
      # join the production by region-glu-commodity-year information:
      left_join_error_no_match(L101.ag_Prod_Mt_R_C_Y_GLU %>% rename(prod = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # Calculate yield by region-commodity-glu-year as value = prod/cover:
      mutate(value = prod / cover) %>%
      select(-prod, -cover) %>%
      # set any NA values to 0 yield:
      replace_na(list(value = 0)) ->
      # store in a table of Economic Yield by region-commodity-glu-year:
      L122.ag_EcYield_kgm2_R_C_Y_GLU



    # Land use history ----
    # Lines 181 - 200 in original file
    # Cropland quantities prior to the first AGLU historical year, for spinup of simple carbon cycle model
    # NOTE: Simply assigning this to other arable land
    # This method differs from prior versions of GCAM, where 1971 land cover quantities were rolled back on the basis of the cropland ratios
    # between each land history year and 1971. The problem with this method is that zero values in 1971 can not match non-zero values in prior years.
    # The current method instead will have apparent land use change in going from the land history years to the model base years, but due to equal
    # soil carbon contents, and similar/small vegetative carbon contents, the net emissions signal should be negligible.
    # First, make a table with cropland in the pre-aglu years
    L120.LC_bm2_R_LT_Yh_GLU %>%
      ungroup %>%
      # only save the years in pre-AGLU years:
      filter(year %in% aglu.PREAGLU_YEARS) %>% ungroup() %>%
      # insure that there is cropland for each GCAM region-glu that appear L122.LC_bm2_R_CropLand_Y_GLU:
      tidyr::complete(Land_Type = unique(L122.LC_bm2_R_CropLand_Y_GLU[['Land_Type']]),
                      tidyr::nesting(GCAM_region_ID, GLU, year), fill = list(value = NA)) %>%
      unique() %>%
      # join this historical cropland information to the region-glu-landtypes of L122.LC_bm2_R_CropLand_Y_GLU, preserving
      # NAs as in old DS:
      left_join(
        unique(select(L122.LC_bm2_R_CropLand_Y_GLU, GCAM_region_ID, GLU,
                      Land_Type)),
        .,
        by = c("GCAM_region_ID", "GLU", "Land_Type")) %>%
      # missing values are overwritten to 0:
      replace_na(list(value = 0)) %>%
      # bind to the table of OtherArableLand information by region-glu-year:
      bind_rows(L122.LC_bm2_R_OtherArableLand_Y_GLU, .) %>%
      # update the landtype identifier
      mutate(Land_Type = "OtherArableLand") ->
      # store in a table of OtherArableLand by region-glu-year, including historical years:
      L122.LC_bm2_R_OtherArableLand_Yh_GLU


    # Lines 202-205 in original file
    # All other cropland uses are zero in the pre-aglu years
    # ExtraCropLand is expanded to include historical years in each region-glu, with a value of 0.
    # Take ExtraCropLand by region-glu-year:
    L122.LC_bm2_R_ExtraCropLand_Y_GLU %>%
      # expand to include history years and fill in those values to be 0:
      tidyr::complete(year = c(aglu.PREAGLU_YEARS, aglu.AGLU_HISTORICAL_YEARS),
                      nesting(GCAM_region_ID, GLU, Land_Type),
                      fill = list(value = 0)) ->
      # store in a table of ExtraCropland by region-glu-year, including historical years:
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU


    # Lines 207-210 in original file
    # Harvested cropland history
    # HarvCropLand is expanded to include historical years for each region-glu-commodity, with a value of 0.
    # Take HarvCropLand by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Y_GLU %>%
      ungroup %>%
      # expand to include history years and fill in those values to be 0:
      tidyr::complete(year = c(aglu.PREAGLU_YEARS, aglu.AGLU_HISTORICAL_YEARS),
                      nesting(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, Land_Type),
                      fill = list(value = 0)) ->
      # store in a table of HarvCropland by region-commodity-glu-year, including historical years:
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    # Lines 212-217 in original file
    # Combine crop types to get land use history by all harvested cropland.
    # Use the table of HarvCropland by region-commodity-glu-year, including historical years, L122.LC_bm2_R_HarvCropLand_C_Yh_GLU,
    # and aggregate across commodity to arrive at a table of All Harvested Cropland by region-glu-year, including historical years.
    # Take HarvCropland by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      # remove commodity because that is what aggregate over:
      select(-GCAM_commodity) %>%
      # group by region-glu-year to aggregate:
      group_by(GCAM_region_ID, GLU, Land_Type, year) %>%
      # aggregate:
      summarise(value = sum(value)) ->
      # store in a table of all HarvCropland by region-glu-year, including historical years:
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    # remove the now-unneeded Land_Type identifier from HarvCropland by region-commodity-glu-year:
    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      select(-Land_Type) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU


    # Force-Convert years and GCAM_region_IDs in some problematic outputs to integers:
    L122.LC_bm2_R_ExtraCropLand_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_Yh_GLU %>%
      ungroup() %>%
      mutate(year = as.integer(year),
             GCAM_region_ID = as.integer(GCAM_region_ID)) ->
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    # Produce outputs ----

    # Update L101 files here after the Adj

    L101.ag_HA_bm2_R_C_Y_GLU %>%
      add_title("Harvested area by GCAM region, commodity, year, and GLU") %>%
      add_units("billion km2") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region, commodity, and GLU") %>%
      add_comments("Additional adjustments are made in zaglu_L122") %>%
      add_legacy_name("L101.ag_HA_bm2_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L120.LC_bm2_R_LT_Yh_GLU",
                     "L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj",
                     "L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj") ->
      L101.ag_HA_bm2_R_C_Y_GLU
    L101.ag_HA_bm2_R_C_Y %>%
      add_title("Harvested area by GCAM region, commodity, and year") %>%
      add_units("billion km2") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region and commodity") %>%
      add_comments("Additional adjustments are made in zaglu_L122") %>%
      add_legacy_name("L101.ag_HA_bm2_R_C_Y") %>%
      same_precursors_as(L101.ag_HA_bm2_R_C_Y_GLU) ->
      L101.ag_HA_bm2_R_C_Y
    L101.ag_Prod_Mt_R_C_Y_GLU %>%
      add_title("Agricultural production by GCAM region, commodity, year, and GLU") %>%
      add_units("Mt/yr") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region, commodity, and GLU") %>%
      add_comments("Additional adjustments are made in zaglu_L122") %>%
      add_legacy_name("L101.ag_Prod_Mt_R_C_Y_GLU") %>%
      same_precursors_as(L101.ag_HA_bm2_R_C_Y_GLU) ->
      L101.ag_Prod_Mt_R_C_Y_GLU
    L101.ag_Prod_Mt_R_C_Y %>%
      add_title("Agricultural production by GCAM region, commodity, and year") %>%
      add_units("Mt/yr") %>%
      add_comments("FAO data downscaled to GLU then aggregated by GCAM region and commodity") %>%
      add_comments("Additional adjustments are made in zaglu_L122") %>%
      add_legacy_name("L101.ag_Prod_Mt_R_C_Y") %>%
      same_precursors_as(L101.ag_HA_bm2_R_C_Y_GLU) ->
      L101.ag_Prod_Mt_R_C_Y


    L122.ag_HA_bm2_R_Y_GLU_AnnualCHF %>%
      add_title("Harvested area to cropland for annual crops ratio by GCAM region / year / GLU") %>%
      add_units("Unitless ratio") %>%
      add_comments("FAO harvested area, fallow area and land cover area, with Monfreda harvested area and Hyde") %>%
      add_comments("land cover data are used to calculate harvested area:cropland for every GCAM region-GLU") %>%
      add_legacy_name("L122.ag_HA_bm2_R_Y_GLU_AnnualCHF") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.ag_HA_bm2_R_Y_GLU_AnnualCHF

    L122.ag_EcYield_kgm2_R_C_Y_GLU %>%
      add_title("Economic yield by GCAM region / commodity / year / GLU") %>%
      add_units("kilogram per meter squared (kg.m2)") %>%
      add_comments("Monfreda agricultural production data is combined with aggregated harvested area data at the") %>%
      add_comments("region-GLU-commodity level to calculate economic yield.") %>%
      add_legacy_name("L122.ag_EcYield_kgm2_R_C_Y_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj",
                     "L101.ag_Prod_Mt_R_C_Y_GLU_BeforeAdj",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.ag_EcYield_kgm2_R_C_Y_GLU

    L122.LC_bm2_R_OtherArableLand_Yh_GLU %>%
      add_title("Other arable land cover by GCAM region / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Other Arable Land cover is calculated at the region-GLU level by using FAO fallow land data and ") %>%
      add_comments("adding the difference Harvested Cropland - Available Cropland. ") %>%
      add_legacy_name("L122.LC_bm2_R_OtherArableLand_Yh_GLU") %>%
      add_precursors("common/iso_GCAM_regID",
                     "L100.FAO_fallowland_kha",
                     "L100.FAO_CL_kha",
                     "L101.ag_HA_bm2_R_C_Y_GLU_BeforeAdj",
                     "L120.LC_bm2_R_LT_Yh_GLU") ->
      L122.LC_bm2_R_OtherArableLand_Yh_GLU

    L122.LC_bm2_R_ExtraCropLand_Yh_GLU %>%
      add_title("Extra cropland cover by GCAM region / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Any land at the region-GLU level for which Harvested Cropland - Available Cropland is positive, ") %>%
      add_comments("making it extra.") %>%
      add_legacy_name("L122.LC_bm2_R_ExtraCropLand_Yh_GLU") %>%
      same_precursors_as(L122.LC_bm2_R_OtherArableLand_Yh_GLU)  ->
      L122.LC_bm2_R_ExtraCropLand_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_C_Yh_GLU %>%
      add_title("Harvested cropland cover by GCAM region / commodity / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Harvested cropland at the region-GLU-commodity level is calculated by dividing Monfreda harvested area") %>%
      add_comments("values by the HarvestedArea:CropLand ratio calculated from Monfreda, FAO, and Hyde land cover and ") %>%
      add_comments("harvested area data.") %>%
      add_legacy_name("L122.LC_bm2_R_HarvCropLand_C_Yh_GLU") %>%
      same_precursors_as(L122.LC_bm2_R_OtherArableLand_Yh_GLU)  ->
      L122.LC_bm2_R_HarvCropLand_C_Yh_GLU

    L122.LC_bm2_R_HarvCropLand_Yh_GLU %>%
      add_title("Harvested cropland cover by GCAM region / year / GLU") %>%
      add_units("billion meters squared (bm2)") %>%
      add_comments("Harvested cropland at the region-GLU-commodity level is calculated by dividing Monfreda harvested area") %>%
      add_comments("values by the HarvestedArea:CropLand ratio calculated from Monfreda, FAO, and Hyde land cover and ") %>%
      add_comments("harvested area data. This commodity information is then aggregated to the region-GLU level.") %>%
      add_legacy_name("L122.LC_bm2_R_HarvCropLand_Yh_GLU") %>%
      same_precursors_as(L122.LC_bm2_R_OtherArableLand_Yh_GLU)  ->
      L122.LC_bm2_R_HarvCropLand_Yh_GLU

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
