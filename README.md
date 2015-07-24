# wppExplorer

[![Travis-CI Build Status](https://travis-ci.org/PPgp/wppExplorer.svg?branch=master)](https://travis-ci.org/PPgp/wppExplorer)

R package to view the World Population Prospects (WPP) published by the United Nations Population Division. 
It is an application based on the shiny R package. 

To start a session, from R do 
```R
library(wppExplorer)
wpp.explore()
```
This will open the interface in your default web browser. To end the session, press the Esc key.

The package also offers various functions to retrieve the WPP indicators in R as data frames. See the help file for `?wpp.indicator`.

The application can be also viewed [online](https://rstudio.stat.washington.edu/shiny/wppExplorer/inst/explore/).
