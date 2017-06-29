\name{set.wpp.year}
\alias{set.wpp.year}
\alias{get.wpp.year}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Edit/Retrieve WPP Year.
}
\description{
Change/get the WPP year which determines from which \pkg{wpp} package data are retrieved.
}
\usage{
set.wpp.year(wpp.year)

get.wpp.year()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{wpp.year}{Possible values are 2017 (default) 2015, 2012, and 2010.}
}

\value{
\code{get.wpp.year} returns the wpp year used through out the package.
}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
get.wpp.year() # by default the wpp2017 is used
set.wpp.year(2012) # any subsequent operations use the wpp2012 package
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }

