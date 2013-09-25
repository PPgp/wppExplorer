\name{wpp.by.country}
\alias{wpp.by.country}
\alias{wpp.by.countries}
\alias{wpp.by.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selection by Country and Year
}
\description{
Filtering indicators by given countries and year.
}
\usage{
wpp.by.country(data, country)

wpp.by.countries(data, countries)

wpp.by.year(data, year)
}
\arguments{
  \item{data}{Data frame as returned by \code{\link{wpp.indicator}}.}
  \item{country}{alpha-2 ISO 3166 country code.}
  \item{countries}{Vector of alpha-2 ISO 3166 country codes.}
  \item{year}{Year. For vital rates with 5-years time period, currently it is the end year of the time period.}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame filtered by the input arguments.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{wpp.indicator}}
}
\examples{
tfr <- wpp.indicator("fert")
tfr.germany.2010 <- wpp.by.country(wpp.by.year(tfr, 2010), 'DE')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

