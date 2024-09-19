
test_that("mixr is in style compliance", {
  skip_on_jenkins()  # don't run on Jenkins
  skip_on_check()    # don't run on check()
  skip_on_covr()     # don't run if in 'covr'
  skip_if_not_installed("lintr")
  skip_if_not(packageVersion("lintr") >= "3.0.2")
  skip_if_not_installed("somaverse")

  # linters and exclusions are controlled by
  # the .lintr file in pkg root
  tLints <- withr::with_dir(
    ifelse(is_testing(), ".", "tests/testthat"),
    lintr::lint_dir(pattern = "^test-")
  )
  rLints <- withr::with_dir(
    ifelse(is_testing(), "../../R", "R"),
    lintr::lint_dir(pattern = "[.][Rr]$")
  )
  expect_length(tLints, 0L)
  expect_length(rLints, 0L)
})
