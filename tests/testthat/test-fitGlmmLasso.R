skip("fix when SomaClassify conflict is resolved; see /inst")

# Setup ----
log_data <- log10(splyr::sim_test_data)
# bootstrap re-sample: n = 300
log_data <- withr::with_seed(101, slice_sample(log_data, n = 300, replace = TRUE))
apts     <- SomaDataIO::getAnalytes(log_data)
myclassform <- createFormula("status", apts)
myregform   <- createFormula("reg_response", apts)
log_data$SiteId <- factor(log_data$SiteId)
myrandomform <- list(SiteId = ~ 1)


# Testing ----
# Test classification ----
test_that("`fitGlmmLasso()` for classifcation returns the correct values", {
  fitclass <- crossValidateGlmmLasso(
    data               = log_data,
    fixed              = myclassform,
    random             = myrandomform,
    family             = stats::binomial(link = "logit"),
    folds              = 2,
    .repeats           = 2,
    .lambda            = c(1, 10),
    r.seed             = 10
    ) |>
    fitGlmmLasso(data = log_data, metric = "auc", lambda = 1)

  expect_s3_class(fitclass, "glmmLasso")
  expect_type(fitclass$fitted.values, "double")
  expect_equal(dim(fitclass$fitted.values), c(300, 1))
  expect_equal(c(summary(fitclass$fitted.values[, 1L])),
               c(`Min.`    = 0.00001868725927,
                 `1st Qu.` = 0.21467323082561,
                 `Median`  = 0.82180179362199,
                 `Mean`    = 0.63318446037124,
                 `3rd Qu.` = 0.98962205642175,
                 `Max.`    = 0.99999999987258)
  )

  expect_type(fitclass$coefficients, "double")
  expect_length(fitclass$coefficients, length(apts) + 1L)
  expect_equal(head(fitclass$coefficients, 10),
               c("(Intercept)" = -324.03844669189374,
                 "seq.2802.68" = 14.62895334373222,
                 "seq.9251.29" = 1.00050559682544,
                 "seq.1942.70" = -1.59458495239543,
                 "seq.5751.80" = 15.97178954991275,
                 "seq.9608.12" = -3.75504809027948,
                 "seq.3459.49" = 11.73286725448433,
                 "seq.3865.56" = -8.84589755829534,
                 "seq.3363.21" = 9.17693160022888,
                 "seq.4487.88" = 11.21167745590251)
  )
})

# Test regression ----
test_that("`fitGlmmLasso()` for regression returns the correct values", {
  fitreg <- crossValidateGlmmLasso(
    data               = log_data,
    fixed              = myregform,
    random             = myrandomform,
    family             = stats::gaussian(link = "identity"),
    folds              = 2,
    .repeats           = 2,
    .lambda            = c(1, 10),
    r.seed             = 10
    ) |>
    fitGlmmLasso(data = log_data, metric = "rsqTrad", lambda = 1)

  expect_s3_class(fitreg, "glmmLasso")
  expect_type(fitreg$fitted.values, "double")
  expect_equal(dim(fitreg$fitted.values), c(300, 1))
  expect_equal(c(summary(fitreg$fitted.values[, 1L])),
               c(`Min.`    = -764.362542404710,
                 `1st Qu.` = -116.771885647259,
                 `Median`  = 224.383072176842,
                 `Mean`    = 172.746993502741,
                 `3rd Qu.` = 473.723125367711,
                 `Max.`    = 1312.723858604274)
  )

  expect_type(fitreg$coefficients, "double")
  expect_length(fitreg$coefficients, length(apts) + 1L)
  expect_equal(head(fitreg$coefficients, 10),
               c("(Intercept)" = -21422.241845799821,
                 "seq.2802.68" = 319.947205130940,
                 "seq.9251.29" = 665.038211911655,
                 "seq.1942.70" = -980.112180575365,
                 "seq.5751.80" = 882.046887863538,
                 "seq.9608.12" = -166.447502330345,
                 "seq.3459.49" = 1381.378975321715,
                 "seq.3865.56" = 581.991314988336,
                 "seq.3363.21" = 1350.461858003682,
                 "seq.4487.88" = 696.944465701534)
  )
})
