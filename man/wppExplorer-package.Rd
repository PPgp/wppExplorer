\name{wppExplorer-package}
\alias{wppExplorer-package}
\alias{wppExplorer}
\docType{package}
\title{
Explorer of the World Population Prospects
}

\description{
Allows to interactively explore data from the World Population Prospects, contained in packages \pkg{wpp2008}, \pkg{wpp2010} and \pkg{wpp2012}. It is based on the \pkg{shiny} package.
}
\details{
\tabular{ll}{
Package: \tab wppExplorer\cr
Version: \tab 1.6-1\cr
Date: \tab 2015-4-10\cr
Depends: \tab R (>= 3.0.0)\cr
License: \tab GPL (>= 2)
}

The main function of the packge is \code{\link{wpp.explore}} which opens an interactive interface in a web browser. A few helper functions are available for a fast access to the (possibly filtered) data, e.g. \code{\link{wpp.indicator}}.

The interface can be also accessed online: \url{https://rstudio.stat.washington.edu/shiny/wppExplorer/inst/explore/}
}

\source{
 The underlying datasets are based on estimates and projections of the United Nations, Department of Economic and Social Affairs, Population Division (2009, 2011, 2013). The probabilistic projections were produced with the method of Raftery et al. (2012).
 }
 
 \references{
 World Population Prospects: The 2012 Revision. (\url{http://esa.un.org/unpd/wpp}) Special Tabulations.
 
 Probabilistic projections: \url{http://esa.un.org/unpd/ppp}
 
 A. E. Raftery,  N. Li, H. Sevcikova , P. Gerland, G. K. Heilig (2012). Bayesian probabilistic population projections for all countries. Proceedings of the National Academy of Sciences 109:13915-13921.
 
 P. Gerland, A. E. Raftery, H. Sevcikova, N. Li, D. Gu, T. Spoorenberg, L. Alkema, B. K. Fosdick, J. L. Chunn, N. Lalic, G. Bay, T. Buettner, G. K. Heilig,  J. Wilmoth (2014). World Population Stabilization Unlikely This Century. Science 346:234-237.
}



 \author{
Hana Sevcikova <hanas@uw.edu>, used code from a Joe Cheng's shiny example

Maintainer: Hana Sevcikova <hanas@uw.edu>
}

\seealso{
\code{\link[wpp2012:wpp2012-package]{wpp2012}}, \code{\link[wpp2010:wpp2010-package]{wpp2010}}, \code{\link[wpp2008:wpp2008-package]{wpp2008}},  \code{\link[shiny:shiny-package]{shiny}}, \code{\link{wpp.explore}}, \code{\link{wpp.indicator}}
}

\keyword{ package }