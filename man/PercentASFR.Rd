\name{percentASFR}
\alias{percentASFR}
\docType{data}
\title{
Datasets on Age-specific Distribution of Fertility Rates
}
\description{
Datasets giving the percentage of fertility rates over ages 15-50. 
}
\usage{
    data(percentASFR)
}
\format{
  A data frame with one row per country and age group. For each country there are seven age groups. It contains columns \code{country}, \code{country_code}, \code{age} and one columns per time interval.
}

\source{
 This dataset is based on estimates and projections of United Nations, Department of Economic and Social Affairs, Population Division (2011). 
 }
 
 \references{
 World Population Prospects: The 2010 Revision. (\url{http://esa.un.org/unpd/wpp}) Special Tabulations.
}


\examples{
data(percentASFR)
str(percentASFR)
}
\keyword{datasets}
