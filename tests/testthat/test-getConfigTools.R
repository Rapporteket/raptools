context("Test getConfigTools")

currentConfigPath <- Sys.getenv("R_RAP_CONFIG_PATH")

Sys.setenv(R_RAP_CONFIG_PATH="")

test_that("the function produces the correct errors", {
  expect_error(
    raptools::getConfigTools("appp"),
    'fileName == "rapbaseConfig" is not TRUE'
  )
  expect_error(
    raptools::getConfigTools("rapbaseConfig"),
    "cant find R_RAP_CONFIG_PATH"
  )
})

Sys.setenv(R_RAP_CONFIG_PATH="something")
test_that("the function produces the correct errors", {
  expect_error(
    raptools::getConfigTools("rapbaseConfig"),
    "cant find the Config file"
  )
})

Sys.setenv(R_RAP_CONFIG_PATH=currentConfigPath)
