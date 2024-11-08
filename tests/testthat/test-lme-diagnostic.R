
# Setup ----
# simulate longitudinal data with group-specific response
p1 <- list(nsubj = 15, beta0 = 700, beta1 = 200, max_obs = 10, r_seed = 123,
           sd_pars = list(sigma = 250), auto_cor = 0.1, group = "cat")

p2 <- list(nsubj = 15, beta0 = 700, beta1 = 0, max_obs = 10, r_seed = 99,
           sd_pars = list(sigma = 250), auto_cor = 0.1, group = "dog")

long_data <- withr::with_options(list(warn = -1), create_long_data(p1, p2))
fit <- fit_lme_safely(yij ~ time, random = ~1 | pid, data = long_data)


# Testing ----
test_that("`lme_diagnostic()` generates expected ggplot object without grouping", {
  expect_snapshot_plot(
    lme_diagnostic(fit, long_data, group_by = NULL),
    "plot-lme-diag-no-group"
  )
})

test_that("`lme_diagnostic()` generates expected ggplot object with grouping", {
  # silence notch warning in boxplots
  withr::local_options(list(rlib_message_verbosity = "quiet"))
  expect_snapshot_plot(
    lme_diagnostic(fit, long_data, group_by = "Group"),
    "plot-lme-diag-group"
  )
})
