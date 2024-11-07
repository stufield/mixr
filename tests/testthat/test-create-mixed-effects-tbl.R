
# Setup ----
withr::local_options(list(signal.quiet = TRUE))
lme_tab <- fit_mixed_effects_models(lme_data,
                                    fixed = "TimePoint*Response",
                                    random = "~ 1 | Pop") |>
  create_mixed_effects_tbl()

# Testing ----
test_that("`fit_mixed_effects_models()` function creates correct values", {
  expect_s3_class(lme_tab, "stat_table")
  expect_s3_class(lme_tab, "mixed_effects_table")
  expect_named(lme_tab, c("stat_table", "test", "call", "method", "fixed",
                          "random", "log", "nModels", "data_dim",
                          "observations", "PIDfield", "subjects"))
  true <- data.frame(
    Response_F.value           = c(2.47804648450652, 1.62977099950533), # nolint
    Response_p.value           = c(0.129101969131925, 0.214474488613522),
    TimePoint_F.value          = c(0.454271626829525, 0.944505240664199),
    TimePoint_p.value          = c(0.716800017255019, 0.435388366502506),
    TimePoint.Response_F.value = c(1.57142383466812, 0.319818850470389),
    TimePoint.Response_p.value = c(0.223382264537352, 0.810932660428589),
    converged                  = c(TRUE, TRUE),
    fdr                        = c(0.446764529074703, 0.810932660428589),
    p_bonferroni               = c(0.446764529074703, 1),
    rank                       = c(1L, 2L),
    row.names                  = c("seq.1234.56", "seq.6969.4")
  )
  expect_equal(lme_tab$test, "Linear Mixed-effects Model")
  expect_equal(as.character(lme_tab$call),
               c("lme.formula", "frm", "as.formula(random)", "data"))
  expect_equal(lme_tab$method, "REML")
  expect_equal(lme_tab$fixed, "TimePoint*Response")
  expect_equal(lme_tab$random, "~ 1 | Pop")
  expect_false(lme_tab$log)
  expect_equal(lme_tab$nModels, 2)
  expect_equal(lme_tab$data_dim, c(40L, 5L))
  expect_equal(lme_tab$observations, 40)
  expect_equal(lme_tab$PIDfield, "Pop")
  expect_equal(lme_tab$subjects, 10)

  # Print method
  withr::with_options(list(signal.quiet = FALSE), expect_snapshot_output(lme_tab))
})
