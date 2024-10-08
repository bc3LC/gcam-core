% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zenergy_xml_electricity.R
\name{module_energy_electricity_xml}
\alias{module_energy_electricity_xml}
\title{module_energy_electricity_xml}
\usage{
module_energy_electricity_xml(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{electricity.xml}. The corresponding file in the
original data system was \code{batch_electricity.xml.R} (energy XML).
}
\description{
Construct XML data structure for \code{electricity.xml}.
}
