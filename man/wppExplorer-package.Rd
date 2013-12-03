\name{wppExplorer-package}
\alias{wppExplorer-package}
\alias{wppExplorer}
\docType{package}
\title{
Explorer of World Population Prospects
}

\description{
The package allows to interactively explore data from World Population Prospects, contained in packages \pkg{wpp2008}, \pkg{wpp2010} and \pkg{wpp2012}. It is based on the \pkg{shiny} package.
}
\details{
\tabular{ll}{
Package: \tab wppExplorer\cr
Version: \tab 1.1-2\cr
Date: \tab 2013-12-03\cr
Depends: \tab R (>= 3.0.0)\cr
License: \tab GPL (>= 2)
}

The main function of the packge is \code{\link{wpp.explore}} which opens an interactive interface in a web browser. A few helper functions are available for a fast access to the (possibly filtered) data, e.g. \code{\link{wpp.indicator}}.
}

 \author{
Hana Sevcikova <hanas@uw.edu>, used code from a Joe Cheng's shiny example

Maintainer: Hana Sevcikova <hanas@uw.edu>
}

\seealso{
\code{\link[wpp2008:wpp2008-package]{wpp2008}}, \code{\link[wpp2010:wpp2010-package]{wpp2010}}, \code{\link[wpp2012:wpp2012-package]{wpp2012}}, \code{\link[shiny:shiny-package]{shiny}}, \code{\link{wpp.explore}}, \code{\link{wpp.indicator}}
}

\keyword{ package }