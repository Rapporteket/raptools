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
