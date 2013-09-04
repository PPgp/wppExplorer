\name{wpp.indicator}
\alias{wpp.indicator}
\title{
Accessing a WPP Indicator
}
\description{
An interface for accessing a WPP indicator, where datasets with observed and predicted data are merged. The resulting data frame is in a "melted" format.
}
\usage{
wpp.indicator(what, \dots)
}

\arguments{
  \item{what}{Name of indicator. Possible values are:
  	\describe{
  		\item{fert}{Total fertililty rate. Merges datasets \code{\link[wpp2012:tfr]{tfr}} and \code{\link[wpp2012:tfr]{tfrprojMed}}.} 
  		\item{leF}{Female life expectancy. Merges datasets \code{\link[wpp2012:e0F]{e0F}} and \code{\link[wpp2012:e0F]{e0Fproj}}.}
  		\item{leM}{Male life expectancy. Merges datasets \code{\link[wpp2012:e0M]{e0M}} and \code{\link[wpp2012:e0M]{e0Mproj}}.}
  		\item{tpop}{Total population. Uses datasets with age- and sex-specific population counts and agregates over sexes and ages.}
  		\item{tpopF}{Total female population. Uses datasets \code{\link[wpp2012:popF]{popF}} and \code{\link[wpp2012:popF]{popFprojMed}} and aggregates over ages.}
  		\item{tpopM}{Total male population. Uses datasets \code{\link[wpp2012:popM]{popM}} and \code{\link[wpp2012:popM]{popMprojMed}} and aggregates over ages.}
  		\item{mig}{Total net migration. Aggregates datasets \code{\link[wpp2012:migrationF]{migrationF}} and \code{\link[wpp2012:migrationM]{migrationM}} over ages.}
  		\item{popagesex}{Population by sex and age. Uses datasets \code{\link[wpp2012:popM]{popM}} and \code{\link[wpp2012:popM]{popMprojMed}}. It requires two arguments in \dots, namely \code{sexm=c("F", "M")} and \code{agem=c("0-4", "5-9", ..., "95-99", "100+")}. The function aggregates population counts over the given sex and age groups.}
  		\item{mortagesex}{Mortality by sex and age. Uses datasets \code{\link[wpp2012:mxF]{mxF}} and \code{\link[wpp2012:mxM]{mxM}}. It requires two atguments in \dots, namely \code{sex} which is either "F" or "M", and \code{age} which is one of ("0", "1", "5", "10", "15", "20", ... "95", "100+").}
  		\item{fertage}{Age-specific fertility rate. Uses datasets \code{\link[wpp2012:tfr]{tfr}} and \code{\link[wpp2012:tfr]{tfrprojMed}} which are merged together and dataset \code{\link[wpp2012:percentASFR]{percentASFR}} to derive age-specific rates. It requires one argument in \dots, namely \code{age} which is one of ("15-19", "20-24", ..., "45-49").}
  		\item{fert.ci}{Lower or upper bound of total fertility rate. Requires one argument in \dots, namely \code{ci} which is either \dQuote{low} or \dQuote{high}.}
  		\item{popagesex.ci}{Lower or upper bound of age- and sex-specific population. Requires argument \code{ci} as above, and arguments \code{age} and \code{sex} which take one of the values described under \bold{popagesex}.}
  	}
}
  \item{\dots}{Arguments passed to the underlying functions as described above.
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Hana Sevcikova
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{wpp.by.country}}, \code{\link{wpp.by.year}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
