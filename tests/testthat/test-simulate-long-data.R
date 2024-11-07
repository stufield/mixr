
# Setup ----
long <- simulate_long_data(r_seed = 1L)


# Testing Single Group simulation -----
test_that("the defaults generate the expected simulated data", {
  expect_s3_class(long, "tbl_df")
  expect_named(long, c("pid", "time", "eij", "yij"))
  expect_equal(dim(long), c(118, 4))
  expect_equal(colSums(long),
               c(pid  = 1288.0,
                 time = 615.0,
                 eij  = 149.7,
                 yij  = 124480.2))
})

test_that("the n subjects generate the expected dimensions", {
  long2 <- simulate_long_data(nsubj = 45, r_seed = 2)
  expect_s3_class(long2, "tbl_df")
  expect_equal(dim(long2), c(313, 4))
  expect_equal(colSums(long2),
               c(pid  = 7362.0,
                 time = 1661.0,
                 eij  = 9613.9,
                 yij  = 339206.0))
})

test_that("the `group =` argument adds the proper variable", {
  sim <- simulate_long_data(r_seed = 1, group = "A")
  expect_named(sim, c("pid", "time", "eij", "yij", "Group"))
  expect_equal(dplyr::select(long, -pid),          # minus grouping column
               dplyr::select(sim, -Group, -pid))   # should be identical
})

# Two group simulation ----
test_that("simulating 2 groups generates expected data", {
  p1 <- list(nsubj = 20, beta1 = 0, max_obs = 10, r_seed = 10,
             sd_pars = list(sigma = 150), auto_cor = 0.1)
  p2 <- list(nsubj = 20, beta1 = 400, max_obs = 10, r_seed = 101,
             sd_pars = list(sigma = 350), auto_cor = 0.1)

  # Warning is thrown 2x, so 2 expectations will be required to catch both
  msg <- paste("The `sd.pars` argument list is missing these parameter(s):",
               "'tau0', 'tau1', 'tau01'.\nUsing default parameters: 2, 2, 0.5.")
  expect_warning(sim_group <- create_long_data(control = p1, treatment = p2),
                 msg, fixed = TRUE) |>
    expect_warning(msg, fixed = TRUE)

  expect_named(sim_group, c("pid", "time", "eij", "yij", "Group"))
  expect_equal(vapply(sim_group, typeof, character(1)),
               c(pid   = "character",
                 time  = "double",
                 eij   = "double",
                 yij   = "double",
                 Group = "character"))
  expect_equal(colSums(dplyr::select_if(sim_group, is.numeric)),
               c(time = 1424.0, eij = -6405.3, yij = 527023.5))

})
