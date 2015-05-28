\name{wpp.explore}
\alias{wpp.explore}

\title{
Launching WPP Explorer
}
\description{
Main function of the package. It opens an interactive (shiny) interface in a browser. To exit the browser view, hit the Escape key.
}
\usage{
wpp.explore(wpp.year = NULL)
}

\arguments{
  \item{wpp.year}{Determines which \pkg{wpp} package is to be explored. Currently available are 2008, 2010 and 2012 (default).
}
}
\details{
	The time slider in the left pane defines the year or time period for which data are shown. Note that for vital rates indicators, such as fertility, mortality, migration and sex ratio at birth, the time shown on the slider is the end year of the time period of the data shown in the right pane.
	
	The uncertainty selection is only used for indicators and years for which the corresponding lower and upper bounds are available, and for the appropriate visualization types. These are country plots that appear after clicking on the map, table in the \dQuote{Sortable Data} tab, and \dQuote{Probabilistic Trends} and \dQuote{Pyramids}. In \pkg{wpp2012}, the 80 and 95\% intervals are available for the fertility, life expectancy and total population indicators; The +-1/2child intervals are available for the fertility and population indicators. 
	
	The interface can be also accessed online: \url{https://rstudio.stat.washington.edu/shiny/wppExplorer/inst/explore/}
}
%\value{
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Hana Sevcikova, based on code from a Joe Cheng's shiny example. Used comments from Patrick Gerland.
}
\note{
If you have any troubles to see the data, please check that the \pkg{\link[shiny:shiny-package]{shiny}} package is working in your environment. In particular, check that examples in \url{http://rstudio.github.com/shiny/tutorial} work. On Windows, you might check this:
\url{https://groups.google.com/forum/#!topic/shiny-discuss/tqeURyiPmFI}
}

\seealso{
\code{\link[wpp2008:wpp2008-package]{wpp2008}}, \code{\link[wpp2010:wpp2010-package]{wpp2010}}, \code{\link[wpp2012:wpp2012-package]{wpp2012}}, \code{\link{wpp.indicator}}
}
\examples{
\dontrun{
wpp.explore()}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ programming }

