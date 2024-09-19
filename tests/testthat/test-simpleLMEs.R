
# Note: actual fitted values are tested in `test-simulateLongData.R`
# Here the print methods serve as rough proxy testing of output values

# Reduce digits for S3 print methods
# b/c fits across OS may differ at floating point decimals
withr::local_options(c(digits = 5, cli.num_colors = 1L))

# Setup ----
long  <- simulateLongData(r.seed = 1, beta1 = 50)

# Testing ----
test_that("simpleLME returns correct model fixed slope", {
  model <- simpleLMEs(long, "yij", "time", "pid")
  expect_s3_class(model, "simpleLMEs")
  expect_equal(model$data, long)
  expect_false(model$compare_data)
  expect_equal(model$response, "yij")
  expect_equal(model$fixed, "time")
  expect_equal(as.character(model$random), c("~", "time | pid"))
  expect_equal(model$grouping, "pid")
  # Print method
  expect_snapshot_output(model)
})

test_that("simpleLME returns correct model random slope", {
  model <- simpleLMEs(long, "yij", "time", "pid", random.slope = FALSE)
  expect_s3_class(model, "simpleLMEs")
  expect_equal(model$data, long)
  expect_false(model$compare_data)
  expect_equal(model$response, "yij")
  expect_equal(model$fixed, "time")
  expect_equal(as.character(model$random), c("~", "1 | pid"))
  expect_equal(model$grouping, "pid")
  # Print method
  expect_snapshot_output(model)
})

# Compare 2 data sets -------
test_that("simpleLME returns model when comparing 2 data sets", {
  long2 <- simulateLongData(r.seed = 405, beta1 = 10)     # smaller beta1
  model <- simpleLMEs(list(A = long, B = long2), "yij", "time", "pid")
  expect_s3_class(model, "simpleLMEs")
  expect_equal(model$data, list(A = long, B = long2))
  expect_true(model$compare_data)
  expect_equal(model$response, "yij")
  expect_equal(model$fixed, "time")
  expect_equal(as.character(model$random), c("~", "time | pid"))
  expect_equal(model$grouping, "pid")
  # Print method
  expect_snapshot_output(model)
})

test_that("simpleLME trips error when data list is not named", {
  long2 <- simulateLongData(r.seed = 405, beta1 = 10)     # smaller beta1
  expect_error(
    simpleLMEs(list(long, long2), "yij", "time", "pid"),
    "If `data =` argument is a list of data frames, it *must* be named.",
    fixed = TRUE
  )
})
