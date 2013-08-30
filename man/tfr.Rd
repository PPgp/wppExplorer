\name{tfr}
\alias{tfr}
\alias{tfr_supplemental}
\alias{tfrprojMed}
\alias{tfrprojHigh}
\alias{tfrprojLow}
\docType{data}
\title{
United Nations Time Series of Total Fertility Rate
}
\description{
Datasets containing the United Nations time series of the total fertility rate (TFR) for all countries of the world as available in 2010. Dataset \code{tfr} contains estimates of the historical TFR starting with 1950; \code{tfr_supplemental} contains a subset of countries for which data prior 1950 are available. Datasets \code{tfrprojMed}, \code{tfrprojHigh} and \code{tfrprojLow} contain median, high and low projections, respectively.
}
\usage{
data(tfr)
data(tfr_supplemental)
data(tfrprojMed)
data(tfrprojHigh)
data(tfrprojLow)
}

\format{
  The datasets contain one record per country or region. It contains the following variables:
  \describe{
    \item{\code{country}}{Name of country or region (following ISO 3166 official short names in English - see \url{http://www.iso.org/iso/country_codes/iso_3166_code_lists/english_country_names_and_code_elements.htm} and United Nations Multilingual Terminology Database - see \url{http://unterm.un.org}).}
    \item{\code{country_code}}{Numerical Location Code (3-digit codes following ISO 3166-1 numeric standard) - see \url{http://en.wikipedia.org/wiki/ISO_3166-1_numeric}.}
    \item{\code{1950-1955}, \code{1955-1960}, \dots}{TFR in various five-year time intervals. The \code{tfrproj*} datasets start at \code{2010-2015}. The \code{tfr_supplemental} datasets start at \code{1740-1745}. Missing data have \code{NA} values.}
  }
}
\source{
 These datasets are based on estimates and projections of United Nations, Department of Economic and Social Affairs, Population Division (2011). 
 }
 
 \references{
 World Population Prospects: The 2010 Revision. (\url{http://esa.un.org/unpd/wpp}) Special Tabulations.
}

\examples{
data(tfr)
head(tfr)

data(tfrprojMed)
str(tfrprojMed)
}
\keyword{datasets}
