test_that("githubApi is class githubApi", {
  expect_equal(
    class(githubApi("repos/Rapporteket/raptools/branches", proxyUrl = "")),
    "githubApi"
  )
})
