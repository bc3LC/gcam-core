# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_energy_transportation_downscaling_xml
#'
#' Construct XML data structure for \code{transportation_UCD_*.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{transportation_UCD_*.xml}. The corresponding file in the
#' original data system was \code{batch_transportation_UCD_CORE.xml} (energy XML).
module_energy_transportation_downscaling_xml <- function(command, ...) {
  # The below variable (trn_SPP) controls which scenario to run, as only one scenario can be run at a time.
  # This is a special case, and the way this is executed will likely change in the future.


  if(command == driver.DECLARE_INPUTS) {
    return(c("L254.Supplysector_trn",
             "L254.FinalEnergyKeyword_trn",
             "L254.tranSubsectorLogit",
             "L254.tranSubsectorShrwtFllt",
             "L254.tranSubsectorInterp",
             "L254.tranSubsectorSpeed",
             "L254.tranSubsectorSpeed_passthru",
             "L254.tranSubsectorSpeed_noVOTT",
             "L254.tranSubsectorSpeed_nonmotor",
             "L254.tranSubsectorVOTT",
             "L254.tranSubsectorFuelPref",
             "L254.StubTranTech",
             "L254.StubTech_passthru",
             "L254.StubTech_nonmotor",
             "L254.GlobalTechShrwt_passthru",
             "L254.GlobalTechShrwt_nonmotor",
             "L254.GlobalTechCoef_passthru",
             "L254.GlobalRenewTech_nonmotor",
             "L254.GlobalTranTechInterp",
             "L254.GlobalTranTechShrwt",
             "L254.GlobalTranTechSCurve",
             "L254.StubTranTechCalInput",
             "L254.StubTranTechLoadFactor",
             "L254.StubTranTechCost",
             "L254.StubTechTrackCapital",
             "L254.StubTranTechCoef",
             "L254.StubTechCalInput_passthru",
             "L254.StubTechProd_nonmotor",
             "L254.PerCapitaBased_trn",
             "L254.PriceElasticity_trn",
             "L254.IncomeElasticity_trn",
             "L254.BaseService_trn"))
  } else if(command == driver.DECLARE_OUTPUTS) {
    #xml_files<- c("transportation_UCD_CORE.xml","transportation_UCD_SSP1.xml","transportation_UCD_SSP3.xml","transportation_UCD_SSP5.xml","transportation_UCD_highEV.xml")
    xml_files<- c("transportation_UCD_add_CORE.xml","transportation_UCD_add_SSP1.xml","transportation_UCD_add_SSP3.xml","transportation_UCD_add_SSP5.xml")
    names(xml_files) <- rep("XML", length(xml_files))
    return(xml_files)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs
    L254.StubTranTechCost <- get_data(all_data, "L254.StubTranTechCost")
    L254.tranSubsectorVOTT <- get_data(all_data, "L254.tranSubsectorVOTT")

    modes <- c("Heavy truck","Light truck","Medium truck",
               "Large Car and Truck","Car","Mini Car","2W and 3W")

    ratio <- L254.StubTranTechCost |> filter(tranSubsector %in% modes,stub.technology %in% c("BEV","Liquids")) |>
      pivot_wider(names_from = stub.technology,values_from = input.cost) |> mutate(value=BEV/Liquids)


library(ggplot2)
    ggplot()+
      geom_line(data=ratio,aes(x=year,y=value,linetype=tranSubsector,color=sce))+
      facet_wrap(~region,nrow=5) + scale_y_continuous(limits=c(0.5,2))

    ggplot()+
      geom_line(data=ratio,aes(x=year,y=value,linetype=sce,color=tranSubsector))+
      facet_wrap(~region,nrow=5) + scale_y_continuous(limits=c(0.5,2))

    ggplot()+
      geom_line(data=ratio |> filter(sce=="CORE"),aes(x=year,y=value,linetype=sce,color=tranSubsector))+
      facet_wrap(~region,nrow=5) + scale_y_continuous(limits=c(0.5,2))


    map <- read.csv("inst/extdata/energy/mappings/ATB_UCD_mapping.csv")
    atb <- read.csv("inst/extdata/energy/ATB_vehicles.csv")
    atb_ratio <- atb |> filter(metric=="Modeled Vehicle Price (2022$)")

    atb_rat <- NULL
    for (i in seq(1,dim(map)[1])){
      tmp <- atb_ratio |> filter(scenario=="Advanced",vehicle_powertrain==map[i,4],vehicle_detail==map[i,5],fuel_category==map[i,6],vehicle_class==map[i,8])|>
        select(year,value) |> left_join(atb_ratio |> filter(scenario=="Advanced",vehicle_powertrain==map[i,9],vehicle_detail==map[i,10],fuel_category==map[i,11],vehicle_class==map[i,8])|>
                                          select(year,value)|>rename(denominator=value)) |> mutate(value=value/denominator)
      atb_rat <- atb_rat |> rbind(tmp |> select(-denominator) |> mutate(scenario="Advanced",tranSubsector=map[i,1],region=map[i,2]))
    }

    for (i in seq(1,dim(map)[1])){
      tmp <- atb_ratio |> filter(scenario=="Mid",vehicle_powertrain==map[i,4],vehicle_detail==map[i,5],fuel_category==map[i,6],vehicle_class==map[i,8])|>
        select(year,value) |> left_join(atb_ratio |> filter(scenario=="Mid",vehicle_powertrain==map[i,9],vehicle_detail==map[i,10],fuel_category==map[i,11],vehicle_class==map[i,8])|>
                                          select(year,value)|>rename(denominator=value)) |> mutate(value=value/denominator)
      atb_rat <- atb_rat |> rbind(tmp |> select(-denominator) |> mutate(scenario="Mid",tranSubsector=map[i,1],region=map[i,2]))
    }

    for (i in seq(1,dim(map)[1])){
      tmp <- atb_ratio |> filter(scenario=="Conservative",vehicle_powertrain==map[i,4],vehicle_detail==map[i,5],fuel_category==map[i,6],vehicle_class==map[i,8])|>
        select(year,value) |> left_join(atb_ratio |> filter(scenario=="Conservative",vehicle_powertrain==map[i,9],vehicle_detail==map[i,10],fuel_category==map[i,11],vehicle_class==map[i,8])|>
                                          select(year,value)|>rename(denominator=value)) |> mutate(value=value/denominator)
      atb_rat <- atb_rat |> rbind(tmp |> select(-denominator) |> mutate(scenario="Conservative",tranSubsector=map[i,1],region=map[i,2]))
    }

 #extrapolate ratio values to close gap to 0.65 frontier partially by 2100
    atb_rat <- atb_rat |> rbind(atb_rat |> filter(year==2050) |> mutate(year=2100,value=(value-0.65)/2+0.65))
    library(zoo)
    atb_rat <- atb_rat |>
    group_by(scenario,tranSubsector,region) |>
      complete(year = seq(2050,2100,5)) |>
      mutate(value = na.approx(value)) |>
      ungroup()

#make mapping of GCAM regions to 3 groups of cost assumptions
    map_reg <- data.frame(gcam = c(c("USA"),
                                    c("Canada","Russia","EU-12","EU-15","European Free Trade Association")
                                    ),
                          region = c(c("High"),
                                   rep("Medium",5)))

    map_reg <- map_reg |> rbind(data.frame(gcam = setdiff(unique(ratio$region),unique(map_reg$gcam)),
                                           region = rep("Low",26)))
    ggplot()+
      geom_line(data=ratio |> filter(region=="USA",sce=="CORE"),aes(x=year,y=value,linetype=tranSubsector,color=sce))+
      geom_line(data=atb_rat |> filter(region=="High",scenario %in% c("Mid","Advanced")),aes(x=year,y=value,linetype=tranSubsector,color=scenario))+
      scale_y_continuous(limits=c(0.5,2))

    ggplot()+
      geom_line(data=ratio |> filter(region=="USA",sce %in% c("CORE","SSP3")),aes(x=year,y=value,linetype=tranSubsector,color=sce))+
      # geom_line(data=atb_rat |> filter(region=="High",scenario %in% c("Advanced","Mid","Conservative")),aes(x=year,y=value,linetype=tranSubsector,color=scenario))+
      scale_y_continuous(limits=c(0.5,4))

    atb_rat <- left_join(map_reg,atb_rat)
    atb_rat <- atb_rat |> rename(group=region,region=gcam)
    ggplot()+
      geom_line(data=ratio,aes(x=year,y=value,linetype=sce,color=tranSubsector))+
      facet_wrap(~region,nrow=5) + scale_y_continuous(limits=c(0.5,2))

    ggplot()+
      geom_line(data=ratio |> filter(sce=="CORE"),aes(x=year,y=value,linetype=sce,color=tranSubsector))+
      geom_line(data=atb_rat |> filter(scenario %in% c("Mid","Advanced")),aes(x=year,y=value,linetype=tranSubsector,color=scenario))+
      facet_wrap(~region,nrow=5) + scale_y_continuous(limits=c(0.5,2))

    #adjust atb to have all scenarios existing in L254, and
    # rename atb scenario to sce to match with L254.Stub
    atb_rat <- atb_rat |> filter(scenario %in% c("Mid","Advanced")) |>rbind(
      atb_rat |> filter(scenario =="Advanced") |> mutate(scenario="SSP3",value =100)) |> rbind(
        atb_rat |> filter(scenario =="Conservative") |> mutate(scenario="SSP5")
      ) |> mutate(scenario = case_when(
        scenario=="Advanced" ~ "SSP1",
        scenario=="Mid" ~ "CORE",
        .default = scenario
      )) |> rename(sce=scenario,ratio=value)

    library(dplyr)

    #prepare ratio to be used for calculation of new values
    ratio <- ratio |> select(-value)
    ratio <- ratio |> filter(sce=="CORE") |>
            rbind(ratio |> filter(sce=="CORE") |> mutate(sce="SSP1"))|>
      rbind(ratio |> filter(sce=="CORE") |> mutate(sce="SSP3"))|>
      rbind(ratio |> filter(sce=="CORE") |> mutate(sce="SSP5"))

    # make atb ratio be minimum of atb_rat and the ratio before
    atb_rat <- left_join(ratio |> filter(year>2025),atb_rat |> filter(year>2025)) |>
      rowwise()|>
      # mutate(BEV=min(BEV,ratio*Liquids)) |>
      mutate(BEV=ratio*Liquids) |>
      ungroup()



    ggplot()+
      geom_line(data=ratio,aes(x=year,y=BEV,linetype=tranSubsector,color=sce),alpha=0.5)+
      geom_line(data=atb_rat,aes(x=year,y=BEV,linetype=tranSubsector,color=sce))+
      facet_wrap(~region,nrow=5) #+ scale_y_continuous(limits=c(0.5,2))

    ggplot()+
      geom_line(data=ratio|>filter(region=="USA"),aes(x=year,y=BEV,linetype=tranSubsector,color=sce),alpha=0.2)+
      geom_line(data=atb_rat|>filter(region=="USA"),aes(x=year,y=BEV,linetype=tranSubsector,color=sce))+
      facet_wrap(~region,nrow=5) #+ scale_y_continuous(limits=c(0.5,2))

    #prepare data to add for add-on files
    L254.StubTranTechCost <- L254.StubTranTechCost |> filter(stub.technology=="Liquids") |>rbind(
      atb_rat |> select(-Liquids,-group,-ratio) |> mutate(stub.technology="BEV") |> rename(input.cost=BEV)
    ) |> filter(stub.technology=="BEV")

    # ratio <- L254.StubTranTechCost |> filter(tranSubsector %in% modes,stub.technology %in% c("BEV","Liquids")) |>
    #   pivot_wider(names_from = stub.technology,values_from = input.cost) |> mutate(value=BEV/Liquids)

    ret_data <- c()
    curr_env <- environment()

    # for (i in c("CORE","SSP1","SSP3","SSP5")){
      for (i in c("SSP5")){
      xml_name <- paste0("transportation_UCD_add_", i, ".xml")
      #Read SSP specific data
      L254.StubTranTechCost_SSP <- L254.StubTranTechCost %>%  filter(sce== i)%>% filter(year>MODEL_FIRST_FUTURE_YEAR)
      # L254.tranSubsectorVOTT <- L254.tranSubsectorVOTT |> filter(sce==1)


      #Create xmls
      create_xml(xml_name) %>%
        add_xml_data(L254.StubTranTechCost_SSP, "StubTranTechCost") %>%
        add_precursors("L254.StubTranTechCost")  %>%
        assign(xml_name, ., envir = curr_env)


      ret_data <- c(ret_data, xml_name)
      }



    # write_csv(atb_rat, "atb_rat.csv")


    #Return all xmls
    ret_data %>%
      paste(collapse = ", ") %>%
      paste0("return_data(", ., ")") %>%
      parse(text = .) %>%
      eval()


  } else {
    stop("Unknown command")
  }
}
