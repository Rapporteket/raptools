test_that("Function return one element atomic char vector", {
  story <- "Whole story so far."
  event <- "New part of story."
  expect_equal(length(makeMessage(story, event)), 1)
  expect_true(is.atomic(makeMessage(story, event)))
  expect_true(is.character(makeMessage(story, event, newPara = TRUE)))
})
