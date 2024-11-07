
# Setup ----
withr::local_options(list(signal.quiet = TRUE))

apts   <- get_analytes(lme_data)
models <- fit_mixed_effects_models(lme_data, fixed = "TimePoint * Response",
                                   random = "~ 1 | Pop")

# Testing ----
test_that("`fit_mixed_effects_models()` function creates correct objects", {
  expect_type(models, "list")
  expect_named(models, apts)
  expect_equal(lengths(models), c("seq.1234.56" = 19L, "seq.6969.4" = 19L))
  expect_equal(models$seq.1234.56$sigma, 3.44788438164353)
  expect_equal(models$seq.6969.4$sigma, 1.35583393839979)
  expect_true(all(vapply(models, function(.x) .x$converged, NA)))
  expect_equal(sum(models$seq.1234.56$coefficients$fixed), 19.7747181279165)
  expect_equal(sum(models$seq.1234.56$coefficients$random$Pop), -1.11752617743797e-19)
  expect_equal(sum(models$seq.6969.4$coefficients$fixed), 25.4136051500514)
  expect_equal(sum(models$seq.6969.4$coefficients$random$Pop), 9.58955137519979e-15)
})

test_that("table throws an error if fixed effect absent from attributes", {
  attributes(models)$fixed <- NULL
  expect_error(
    create_mixed_effects_tbl(models),
    "Fixed effects string missing. Was the model list created using"
  )
})

test_that("spaces in the fixed= argument doesn't cause trouble with whitespace", {
  expect_equal(
    fit_mixed_effects_models(lme_data, fixed = "TimePoint*Response",
                             random = "~ 1 | Pop"),
    fit_mixed_effects_models(lme_data, fixed = "TimePoint * Response",
                             random = "~ 1 | Pop")
  )
})
