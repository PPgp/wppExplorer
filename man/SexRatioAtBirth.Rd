\name{sexRatio}
\alias{sexRatio}

\docType{data}
\title{
Sex Ratio at Birth
}
\description{
Estimates and projections of the sex ratio at birth derived as the number of female divided by the number of male.
}
\usage{
    data(sexRatio)
}
\format{
  A data frame with one record per country. It contains columns \code{country}, \code{country_code}, and one columns per time interval.
}
\source{
 This dataset is based on estimates and projections of United Nations, Department of Economic and Social Affairs, Population Division (2011). 
 }
 
 \references{
 World Population Prospects: The 2010 Revision. (\url{http://esa.un.org/unpd/wpp}) Special Tabulations.
}

\examples{
data(sexRatio)
str(sexRatio)
}
\keyword{datasets}
