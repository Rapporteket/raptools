
<!-- badges: start -->
[![Travis build status](https://travis-ci.org/Rapporteket/raptools.svg?branch=master)](https://travis-ci.org/Rapporteket/raptools)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Rapporteket/raptools?branch=master&svg=true)](https://ci.appveyor.com/project/Rapporteket/raptools)
[![Codecov test coverage](https://codecov.io/gh/Rapporteket/raptools/branch/master/graph/badge.svg)](https://codecov.io/gh/Rapporteket/raptools?branch=master)
<!-- badges: end -->

Package containing tools for development at Rapporteket

## Install
From within R:
```r
devtools::install_github("Rapporteket/raptools")
```


## Use

### Simulate url for Shiny Server app

```r
rapbase::runShinyApp(appName = "makeUrl", appsDirectoryName = "shinyApps", packageName = "raptools")
```
