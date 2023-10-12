# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L161.ag_R_C_Y_GLU_irr
#'
#' Calculates irrigated and rainfed agriculture production, harvested area and yields by GCAM region / commodity / GLU / year.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L161.ag_irrProd_Mt_R_C_Y_GLU}, \code{L161.ag_rfdProd_Mt_R_C_Y_GLU}, \code{L161.ag_irrHA_bm2_R_C_Y_GLU}, \code{L161.ag_rfdHA_bm2_R_C_Y_GLU}, \code{L161.ag_irrYield_kgm2_R_C_Y_GLU}, \code{L161.ag_rfdYield_kgm2_R_C_Y_GLU}, \code{L161.ag_irrHA_frac_R_C_GLU}. The corresponding file in the
#' original data system was \code{LB161.ag_R_C_Y_GLU_irr.R} (aglu level1).
#' @details This chunk combines FAO annual data and GTAP disaggregated irrigated vs. rainfed data to compute irrigated and rainfed
#' agriculture production, harvested area and yields by GCAM region / commodity / GLU / year. The same irrigated and rainfed fraction
#' is applied to all historical years for each commodity, region and GLU.
#' @importFrom assertthat assert_that
#' @importFrom dplyr if_else left_join mutate right_join select
#' @importFrom tidyr replace_na
#' @author RC August 2017 XZ 2023
module_aglu_L161.ag_R_C_Y_GLU_irr <- function(command, ...) {

  MODULE_INPUTS <-
    c("L101.ag_Prod_Mt_R_C_Y_GLU",
      "L101.ag_HA_bm2_R_C_Y_GLU",
      "L152.ag_irrProd_Mt_R_C_GLU",
      "L152.ag_irrHA_bm2_R_C_GLU",
      "L152.ag_rfdProd_Mt_R_C_GLU",
      "L152.ag_rfdHA_bm2_R_C_GLU")

  MODULE_OUTPUTS <-
    c("L161.ag_irrProd_Mt_R_C_Y_GLU",
      "L161.ag_rfdProd_Mt_R_C_Y_GLU",
      "L161.ag_irrHA_bm2_R_C_Y_GLU",
      "L161.ag_rfdHA_bm2_R_C_Y_GLU",
      "L161.ag_irrYield_kgm2_R_C_Y_GLU",
      "L161.ag_rfdYield_kgm2_R_C_Y_GLU",
      "L161.ag_irrHA_frac_R_C_GLU")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- GCAM_region_ID <- GCAM_commodity <- GLU <- value <- irrProd <- rfdProd <- irrProd_frac <-
      irrHA <- rfdHA <- irrHA_frac <- irrYield <- rfdYield <- MgdFor_adj <- GCAM_subsector <- NULL # silence package check.

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # The downscaling/sharing method in this module is updated to prioritize yield consistency ----
    # E.g., using a similar method created in zaglu_L101.ag_FAO_R_C_Y

    # Step 1: Downscale area using irr area ratio implied by L152 data----

    # Compute irrigated and rainfed harvested area by GCAM commodity / region / GLU / year
    # Combine FAO annual data and GTAP irrigated vs rainfed disaggregated data
    # Multiply total annual harvested area and a constant irrigated vs rainfed fraction for each GCAM region / GLU / commodity
    L152.ag_irrHA_bm2_R_C_GLU %>%
      # Combine GTAP irrigated and rainfed harvested area
      left_join_error_no_match(L152.ag_rfdHA_bm2_R_C_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      # Compute fraction of harvested area that is irrigated for each GCAM region / GLU / commodity
      mutate(irrHA_frac = irrHA / (irrHA + rfdHA)) ->
      # Save the irrigated harvested area fraction table as an output
      L161.ag_irrHA_frac_R_C_GLU

    L161.ag_irrHA_frac_R_C_GLU %>%
      # Repeat the same irrigated fraction to all historical years
      repeat_add_columns(tibble(year = HISTORICAL_YEARS)) %>%
      # Match to FAO annual total harvested area
      right_join(L101.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # Calculate irrigated production by multiplying total by fraction irrigated
      # For islands that are included in the FAO data but not the MIRCA inventory, irrHA_frac will be a missing value after the join. Re-set to 0 (assume all rainfed)
      mutate(irrHA_frac = if_else(is.na(irrHA_frac), 0, irrHA_frac),
             irrHA = value * irrHA_frac,
             # Calculate rainfed production by multiplying total by fraction rainfed
             rfdHA = value * (1 - irrHA_frac)) %>%
      select(-irrHA_frac, -value) ->
      L161.ag_HA_bm2_R_C_Y_GLU

    # Step 2: get yield ratio (IRR vs. RFD) ready at the GLU level ----

    L152.ag_irrProd_Mt_R_C_GLU %>% rename(Prod = irrProd) %>% mutate(IRR = "irr") %>%
      bind_rows(L152.ag_rfdProd_Mt_R_C_GLU %>%
                  rename(Prod = rfdProd) %>% mutate(IRR = "rfd")) %>%
      left_join_error_no_match(
        L152.ag_irrHA_bm2_R_C_GLU %>% rename(HA = irrHA) %>% mutate(IRR = "irr") %>%
          bind_rows(L152.ag_rfdHA_bm2_R_C_GLU %>%
                      rename(HA = rfdHA) %>% mutate(IRR = "rfd")),
        by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "IRR")) ->
      L161.ag_ProdHA_R_C_GLU

    # Compute a default yield ratio at the region level between IRR and RFD
    L161.ag_ProdHA_R_C_GLU %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector, IRR) %>%
      summarize(Yield_Region = sum(Prod) / sum(HA), Prod = sum(Prod), HA = sum(HA)) %>%
      ungroup() %>%
      group_by(GCAM_commodity, IRR) %>%
      mutate(Yield_World = sum(Prod) / sum(HA)) %>%
      ungroup() %>%
      group_by(GCAM_region_ID, GCAM_commodity, GCAM_subsector) %>%
      mutate(YieldRatio_IRR_RFD_Region = Yield_Region / Yield_Region[IRR == "rfd"],
             YieldRatio_IRR_RFD_World = Yield_World / Yield_World[IRR == "rfd"]) %>%
      ungroup() %>%
      filter(IRR == "irr") %>%
      select(-IRR) %>%
      mutate(DefaultYieldRatio_IRR_RFD = if_else(is.na(YieldRatio_IRR_RFD_Region),
                                          YieldRatio_IRR_RFD_World, YieldRatio_IRR_RFD_Region)) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, DefaultYieldRatio_IRR_RFD, YieldRatio_IRR_RFD_World) ->
      L161.ag_DefaultYieldRatio_R_C


    L161.ag_ProdHA_R_C_GLU %>%
      mutate(Yield = Prod/HA) %>%
      select(-HA, -Prod) %>%
      spread(IRR, Yield) %>%
      mutate(YieldRatio_IRR_RFD = irr / rfd) %>%
      left_join_error_no_match(L161.ag_DefaultYieldRatio_R_C,
                              by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector")) %>%
      transmute(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU,
                YieldRatio_IRR_RFD = if_else(is.na(YieldRatio_IRR_RFD), DefaultYieldRatio_IRR_RFD, YieldRatio_IRR_RFD)) ->
      L161.ag_YieldRatio_R_C_GLU




    # library(ggplot2)
    # L161.ag_YieldRatio_R_C_GLU %>%
    #   ggplot() +
    #   geom_boxplot(aes(x = GCAM_subsector, y = YieldRatio_IRR_RFD))+
    #   labs(title = "MIRCA yield ratio between irrigated and rainfed, at the basin level (zagluL161)")+
    #   geom_hline(yintercept = 1, color = "red") + theme_bw() +
    #   theme(text = element_text(size = 14), axis.text.x =element_text(angle = 40, hjust = 1)  )-> p;p
    # ggsave("MIRCA_YieldRatio_IRR_RFD_basin.png", p, width = 14, height = 14)

    # Step 3: improve yield ratio ----
    # set a floor for the yield ratio between IRR and RFD in a GLU
    # IRR should >= RFD yield
    YieldRatio_IRR_RFD_Floor = 1
    Max_YieldRatio_relative_to_Mean = 1
    Ceiling_YieldRatio_IRR_RFE = 4

    L161.ag_YieldRatio_R_C_GLU %>%
      left_join_error_no_match(
        L161.ag_DefaultYieldRatio_R_C %>%
          mutate(DefaultYieldRatio_IRR_RFD = pmax(DefaultYieldRatio_IRR_RFD, YieldRatio_IRR_RFD_Floor),
                 YieldRatio_IRR_RFD_World = pmax(YieldRatio_IRR_RFD_World, YieldRatio_IRR_RFD_Floor)),
        by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector")) %>%
      mutate(YieldRatio_IRR_RFD = pmax(YieldRatio_IRR_RFD, YieldRatio_IRR_RFD_Floor),
             YieldRatio_IRR_RFD = pmin(YieldRatio_IRR_RFD, DefaultYieldRatio_IRR_RFD * Max_YieldRatio_relative_to_Mean),
             YieldRatio_IRR_RFD = pmin(YieldRatio_IRR_RFD, Ceiling_YieldRatio_IRR_RFE)) %>%
      select(names(L161.ag_YieldRatio_R_C_GLU))->
      L161.ag_YieldRatio_R_C_GLU_Updated

    L161.ag_YieldRatio_R_C_GLU %>%
      mutate(YieldRatio_IRR_RFD = pmax(YieldRatio_IRR_RFD, YieldRatio_IRR_RFD_Floor)) %>%
      group_by(GCAM_commodity, GCAM_subsector) %>%
      mutate(outlier_thredshold = quantile(YieldRatio_IRR_RFD, 0.75) + 1.5 * IQR(YieldRatio_IRR_RFD)) %>%
      ungroup %>%
      left_join_error_no_match(
        L161.ag_DefaultYieldRatio_R_C %>%
          mutate(DefaultYieldRatio_IRR_RFD = pmax(DefaultYieldRatio_IRR_RFD, YieldRatio_IRR_RFD_Floor)),
        by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector")) %>%
      mutate(YieldRatio_IRR_RFD =
               if_else(YieldRatio_IRR_RFD <= outlier_thredshold,
                       YieldRatio_IRR_RFD,
                       pmin(outlier_thredshold, DefaultYieldRatio_IRR_RFD)) ) ->
      L161.ag_YieldRatio_R_C_GLU_Updated


    # L161.ag_YieldRatio_R_C_GLU_Updated %>%
    #   ggplot() +
    #   geom_boxplot(aes(x = GCAM_subsector, y = YieldRatio_IRR_RFD))+
    #   labs(title = "MIRCA yield ratio between irrigated and rainfed, at the basin level (zagluL161)")+
    #   geom_hline(yintercept = 1, color = "red") + theme_bw() +
    #   theme(text = element_text(size = 14), axis.text.x =element_text(angle = 40, hjust = 1)  )-> p;p
    # ggsave("MIRCA_YieldRatio_IRR_RFD_basin_updated.png", p, width = 14, height = 14)


    # Step 4: apply yield ratio (IRR vs. RFD) the GLU area ----
    L161.ag_HA_bm2_R_C_Y_GLU %>%
      left_join(L161.ag_YieldRatio_R_C_GLU_Updated,
                by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU")) %>%
      replace_na(list(YieldRatio_IRR_RFD = 1)) %>%
      # relative yield is 1 for rfd and YieldRatio_IRR_RFD for irr
      mutate(ProdBeforeScale = rfdHA * 1 + irrHA * YieldRatio_IRR_RFD) %>%
      left_join_error_no_match(L101.ag_Prod_Mt_R_C_Y_GLU %>% rename(BasinProd = value),
                               by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # calculate scaler
      mutate(ProdScaler = BasinProd / ProdBeforeScale) %>%
      replace_na(list(ProdScaler = 1)) %>%
      # apply scaler to calculate areas
      mutate(irrProd = irrHA * ProdScaler * YieldRatio_IRR_RFD,  rfdProd = rfdHA * ProdScaler * 1) %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, irrProd, rfdProd) ->
      L161.ag_Prod_Mt_R_C_Y_GLU


    # Compute irrigated and rainfed yields (kg/m2) by GCAM region / commodity / GLU / year
    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      # Match proudction and harvested area
      left_join_error_no_match(L161.ag_HA_bm2_R_C_Y_GLU, by = c("GCAM_region_ID", "GCAM_commodity", "GCAM_subsector", "GLU", "year")) %>%
      # Calculate irrigated yields as irrigated production divided by irrigated harvestest area
      mutate(irrYield = irrProd / irrHA,
             # Calculate rainfed yields as rainfed production divided by rainfed harvestest area
             rfdYield = rfdProd / rfdHA) %>%
      # Replace missing value with zero
      replace_na(list(irrYield = 0, rfdYield = 0)) %>%
      select(-irrProd, -rfdProd, -irrHA, -rfdHA) ->
      L161.ag_Yield_kgm2_R_C_Y_GLU

    # Produce outputs
    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = irrProd) %>%
      add_title("Irrigated production by GCAM region / commodity / GLU / year") %>%
      add_units("Mt") %>%
      add_comments("Combine FAO annual production data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total production and a constant irrigated fraction across all histrical years") %>%
      add_legacy_name("L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      add_precursors("L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L152.ag_irrProd_Mt_R_C_GLU") ->
      L161.ag_irrProd_Mt_R_C_Y_GLU

    L161.ag_Prod_Mt_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = rfdProd) %>%
      add_title("Rainfed production by GCAM region / commodity / GLU / year") %>%
      add_units("Mt") %>%
      add_comments("Combine FAO annual production data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total production and a constant rainfed fraction across all histrical years") %>%
      add_legacy_name("L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      add_precursors("L101.ag_Prod_Mt_R_C_Y_GLU",
                     "L152.ag_rfdProd_Mt_R_C_GLU") ->
      L161.ag_rfdProd_Mt_R_C_Y_GLU

    L161.ag_HA_bm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = irrHA) %>%
      add_title("Irrigated harvested area by GCAM region / commodity / GLU / year") %>%
      add_units("bm2") %>%
      add_comments("Combine FAO annual harvested area data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total harvested area and a constant irrigated fraction across all histrical years") %>%
      add_legacy_name("L161.ag_irrHA_bm2_R_C_Y_GLU") %>%
      add_precursors("L101.ag_HA_bm2_R_C_Y_GLU",
                     "L152.ag_irrHA_bm2_R_C_GLU") ->
      L161.ag_irrHA_bm2_R_C_Y_GLU

    L161.ag_HA_bm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = rfdHA) %>%
      add_title("Rainfed harvested area by GCAM region / GLU / commodity / year") %>%
      add_units("bm2") %>%
      add_comments("Combine FAO annual harvested area data and GTAP irrigated vs rainfed disaggregated data by region / commodity / GLU") %>%
      add_comments("Multiply annual total harvested area and a constant rainfed fraction across all histrical years") %>%
      add_legacy_name("L161.ag_rfdHA_bm2_R_C_Y_GLU") %>%
      add_precursors("L101.ag_HA_bm2_R_C_Y_GLU",
                     "L152.ag_rfdHA_bm2_R_C_GLU") ->
      L161.ag_rfdHA_bm2_R_C_Y_GLU

    L161.ag_Yield_kgm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = irrYield) %>%
      add_title("Unadjusted irrigated agronomic yield by GCAM region / commodity / GLU / year") %>%
      add_units("kg/m2") %>%
      add_comments("Divide irrigated production by irrigated harvested area for each region, commodity, GLU and year") %>%
      add_comments("Replace missing value with zero") %>%
      add_legacy_name("L161.ag_irrYield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_irrProd_Mt_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_irrHA_bm2_R_C_Y_GLU") ->
      L161.ag_irrYield_kgm2_R_C_Y_GLU

    L161.ag_Yield_kgm2_R_C_Y_GLU %>%
      select(GCAM_region_ID, GCAM_commodity, GCAM_subsector, GLU, year, value = rfdYield) %>%
      add_title("Unadjusted rainfed agronomic yield by GCAM region / commodity / GLU / year") %>%
      add_units("kg/m2") %>%
      add_comments("Divide rainfed production by rainfed harvested area for each region, commodity, GLU and year") %>%
      add_comments("Replace missing value with zero") %>%
      add_legacy_name("L161.ag_rfdYield_kgm2_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_rfdProd_Mt_R_C_Y_GLU") %>%
      same_precursors_as("L161.ag_rfdHA_bm2_R_C_Y_GLU")  ->
      L161.ag_rfdYield_kgm2_R_C_Y_GLU

    L161.ag_irrHA_frac_R_C_GLU %>%
      add_title("Fraction of harvested area that is irigated by GCAM region / commodity / GLU") %>%
      add_units("Unitless") %>%
      add_comments("Divide irrigated harvested area by the sum of irrigated and rainfed harvest area for each region, commodity and GLU") %>%
      add_legacy_name("L161.ag_irrHA_frac_R_C_GLU") %>%
      add_precursors("L152.ag_irrHA_bm2_R_C_GLU",
                     "L152.ag_rfdHA_bm2_R_C_GLU") ->
      L161.ag_irrHA_frac_R_C_GLU

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
