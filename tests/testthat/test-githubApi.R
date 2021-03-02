test_that("githubApi is class githubApi", {
  expect_equal(
    class(githubApi("repos/Rapporteket/raptools/branches", proxyUrl = "")),
    "githubApi"
  )
})

test_that("githubApi fun provides an error for none existing endpount", {
  expect_error(githubApi("none/existent/endpoint"), regexp = "404")
})
