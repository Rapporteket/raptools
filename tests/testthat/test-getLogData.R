context("Test getLogData")

currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

Sys.setenv(R_RAP_CONFIG_PATH="")

test_that("the function produces the correct errors", {
  expect_error(
    raptools::getLogData("appp"),
    'log == "report" | log == "app" is not TRUE'
    )
  expect_error(
    raptools::getLogData("app"),
    "cant find R_RAP_CONFIG_PATH"
    )
  expect_error(
    raptools::getLogData("report"),
    "cant find R_RAP_CONFIG_PATH"
    )
})

Sys.setenv(R_RAP_CONFIG_PATH="something")
test_that("the function produces the correct errors", {
  expect_error(
    raptools::getLogData("app"),
    "cant find the log"
    )
  expect_error(
    raptools::getLogData("report"),
    "cant find the log"
    )
})

Sys.setenv(R_RAP_CONFIG_PATH=currentConfigPath)
