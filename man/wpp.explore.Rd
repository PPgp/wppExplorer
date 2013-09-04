\name{wpp.explore}
\alias{wpp.explore}

\title{
Launching WPP Explorer
}
\description{
Main function of the package. It opens an interactive (shiny) interface in a browser.
}
\usage{
wpp.explore(wpp.year = 2012)
}

\arguments{
  \item{wpp.year}{Determines which \pkg{wpp} package is to be explored. Currently available are 2008, 2010 and 2012.
}
}
%\details{
%}
%\value{
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Hana Sevcikova, used code from a Joe Cheng's shiny example
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link[wpp2008:wpp2008-package]{wpp2008}}, \code{\link[wpp2010:wpp2010-package]{wpp2010}}, \code{\link[wpp2012:wpp2012-package]{wpp2012}}
}
\examples{
\dontrun{
wpp.explore()}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ programming }

