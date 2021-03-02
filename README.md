# raptools <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->
[![Version](https://img.shields.io/github/v/release/rapporteket/raptools?sort=semver)](https://github.com/rapporteket/raptools/releases)
[![R build status](https://github.com/Rapporteket/raptools/workflows/R-CMD-check/badge.svg)](https://github.com/Rapporteket/raptools/actions)
[![Codecov test coverage](https://codecov.io/gh/Rapporteket/raptools/branch/main/graph/badge.svg)](https://codecov.io/gh/Rapporteket/raptools?branch=main)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![GitHub open issues](https://img.shields.io/github/issues/rapporteket/raptools.svg)](https://github.com/rapporteket/raptools/issues)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Doc](https://img.shields.io/badge/Doc--grey.svg)](https://rapporteket.github.io/raptools/)
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
