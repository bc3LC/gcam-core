% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zenergy_L1011.en_bal_adj.R
\name{module_energy_L1011.en_bal_adj}
\alias{module_energy_L1011.en_bal_adj}
\title{module_energy_L1011.en_bal_adj}
\usage{
module_energy_L1011.en_bal_adj(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L1011.en_bal_EJ_R_Si_Fi_Yh}, \code{L1011.in_EJ_ctry_intlship_TOT_Yh}. The
corresponding file in the original data system was \code{LA1011.en_bal_adj.R} (energy level1).
}
\description{
Adjustments to the IEA energy balance for shipping fuel consumption, Russia, and natural
gas total primary energy supply (TPES; i.e., consumption).
}
\details{
This chunk replaces IEA international shipping fuel consumption estimates with EIA estimates,
remaps USSR data to Russia, and removes coal-to-gas from natural gas TPES.
}
\author{
JDH July 2017
}
