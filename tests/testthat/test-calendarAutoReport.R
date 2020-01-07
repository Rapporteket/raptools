testthat::context("test calendarAutoReport")

test_that("class of the output object is ggplot", {
  expect_equal(class(raptools::calendarAutoReport(1:10)),c("gg","ggplot"))
})

test_that("an error is produced for wrong input types", {
  expect_error(
    raptools::calendarAutoReport(
      runDayOfYear = "error")
    )
  expect_error(
    raptools::calendarAutoReport(
      runDayOfYear = 1:10,
      pointRangeMax = "error"
      )
    )
})
