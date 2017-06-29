\name{iso3166}
\alias{iso3166}
\alias{iso3166ud}
\docType{data}
\title{
ISO 3166 Dataset
}
\description{
Countries with their alpha-2, alpha-3 and numerical ISO3166-1 code. It includes aggregated regions with user-assigned codes. 
}
\usage{
	data(iso3166)
	data(iso3166ud)
}
\format{
  A data frame containing 5 variables:
  \describe{
    \item{\code{name}}{Country name}
    \item{\code{charcode}}{alpha-2 code}
    \item{\code{charcode3}}{alpha-3 code}
    \item{\code{uncode}}{UN numeric code}
    \item{\code{is.country}}{Logical. It is \code{TRUE} for all countries included in the official ISO3166-1 dataset and \code{FALSE} for user-assigned codes, i.e. aggregations.} 
  }
}
\details{
Dataset \code{iso3166} includes both, countries and aggregations; dataset \code{iso3166ud} includes aggregations only. 
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
\url{https://www.iso.org/iso/home/standards/country_codes.htm}
}
\examples{
data(iso3166)
}
\keyword{datasets}
