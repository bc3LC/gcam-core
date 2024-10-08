% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zwater_L210.water_demand_primary.R
\name{module_water_L210.water_demand_primary}
\alias{module_water_L210.water_demand_primary}
\title{module_water_L210.water_demand_primary}
\usage{
module_water_L210.water_demand_primary(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L210.TechCoef}. The corresponding file in the
original data system was \code{L210.water_demand_primary.R} (water level2).
}
\description{
Generate (water demand) technology coefficients for each GCAM region for base years and future years.
}
\details{
This chunk generate (water demand) technology coefficients for each GCAM region for base years and future years,
with information of supplysector, subsector, technology, minicam.energy.input and market.name.
}
\author{
YL May 2017
}
