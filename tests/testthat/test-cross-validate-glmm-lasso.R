
skip("fix when libml conflict is resolved; see /inst")

# Setup ----
log_data <- log_rfu(wranglr::simdata)
# bootstrap re-sample: n = 300
log_data <- withr::with_seed(101, slice_sample(log_data, n = 300, replace = TRUE))
apts     <- get_analytes(log_data)
myclassform <- create_form("status", apts)
myregform   <- create_form("reg_response", apts)
log_data$SiteId <- factor(log_data$SiteId)
myrandomform <- list(SiteId = ~ 1)


# Testing ----
# Test random seed ----
test_that("the 'random.seed' error catch is functioning", {
  expect_error(
    cross_validate_glmm_lasso(myformula, random.seed = 101),
    paste("Please pass 'r_seed' argument as an integer.",
          "You have passed 'random.seed' with value 101."), fixed = TRUE
  )
})

# Test classification ----
test_that("`cross_validate_glmm_lasso()` for classifcation returns as expected", {
  cvclass <- cross_validate_glmm_lasso(
    data               = log_data,
    fixed              = myclassform,
    random             = myrandomform,
    family             = stats::binomial(link = "logit"),
    decision.threshold = 0.5,
    folds              = 2,
    .repeats           = 2,
    .lambda            = c(1, 10),
    r_seed             = 10,
    save.fits          = FALSE,
    save.splits        = FALSE
  )

  expect_s3_class(cvclass, "cvGlmmLasso")
  expect_equal(cvclass$random, myrandomform)
  expect_equal(cvclass$model_type, "classification")
  expect_equal(cvclass$random_seed, 10)
  expect_equal(cvclass$metrics,
               tibble::tibble(
                           id = c("Repeat1", "Repeat1", "Repeat2", "Repeat2",
                                  "Repeat1", "Repeat1", "Repeat2", "Repeat2"),
                          id2 = c("Fold1", "Fold2", "Fold1", "Fold2",
                                  "Fold1", "Fold2", "Fold1", "Fold2"),
           .lambda = c(1, 1, 1, 1, 10, 10, 10, 10),
                  Sensitivity = c(0.905263157894737, 0.947368421052632,
                                  0.715789473684211, 0.884210526315789,
                                  0.810526315789474, 0.831578947368421,
                                  0.694736842105263, 0.705263157894737),
                  Specificity = c(0.727272727272727, 0.727272727272727,
                                  0.890909090909091, 0.709090909090909,
                                  0.636363636363636, 0.490909090909091,
                                  0.454545454545455, 0.600000000000000),
                          auc = c(0.859904306220095, 0.892248803827751,
                                  0.858755980861244, 0.785645933014354,
                                  0.806124401913876, 0.739904306220095,
                                  0.697799043062201, 0.684210526315790))
  )
  # summary
  summ <- summary(cvclass)
  expect_s3_class(summ, "summary_cvGlmmLasso")
  expect_named(summ, c("auc", "Sensitivity", "Specificity"))

  # AUC
  expect_s3_class(summ$auc, "tbl_df")
  expect_equal(summ$auc,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.849138755980861, 0.732009569377990),
                      sd = c(0.0450858546921709, 0.0548040263122273),
                  median = c(0.859330143540670, 0.718851674641148),
                     min = c(0.785645933014354, 0.684210526315790),
                     max = c(0.892248803827751, 0.806124401913876),
                      lb = c(0.791129186602871, 0.685229665071770),
                      ub = c(0.889822966507177, 0.801157894736842),
                       n = c(4L, 4L))
  )

  # Sensitivity
  expect_s3_class(summ$Sensitivity, "tbl_df")
  expect_equal(summ$Sensitivity,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.863157894736842, 0.760526315789474),
                      sd = c(0.1016938719030838, 0.0705472605715196),
                  median = c(0.894736842105263, 0.757894736842105),
                     min = c(0.715789473684211, 0.694736842105263),
                     max = c(0.947368421052632, 0.831578947368421),
                      lb = c(0.728421052631579, 0.695526315789474),
                      ub = c(0.94421052631579, 0.83000000000000),
                       n = c(4L, 4L))
  )

  # Specificity
  expect_s3_class(summ$Specificity, "tbl_df")
  expect_equal(summ$Specificity,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.763636363636364, 0.545454545454545),
                      sd = c(0.0852802865422441, 0.0865627688308224),
                  median = c(0.727272727272727, 0.545454545454545),
                     min = c(0.709090909090909, 0.454545454545455),
                     max = c(0.890909090909091, 0.636363636363636),
                      lb = c(0.710454545454546, 0.457272727272727),
                      ub = c(0.878636363636364, 0.633636363636364),
                       n = c(4L, 4L))
  )
})

# Test regression ----
test_that("`cross_validate_glmm_lasso()` for regression returns as expected", {
  cvreg <- cross_validate_glmm_lasso(
    data               = log_data,
    fixed              = myregform,
    random             = myrandomform,
    family             = stats::gaussian(link = "identity"),
    decision_threshold = 0.5,
    folds              = 2,
    .repeats           = 2,
    .lambda            = c(1, 10),
    r_seed             = 10,
    save_fits          = FALSE,
    save_splits        = FALSE
  )

  expect_s3_class(cvreg, "cvGlmmLasso")
  expect_equal(cvreg$random, myrandomform)
  expect_equal(cvreg$model_type, "regression")
  expect_equal(cvreg$random_seed, 10)
  expect_equal(cvreg$metrics,
         tibble::tibble(
                   id = c("Repeat1", "Repeat1", "Repeat2", "Repeat2",
                          "Repeat1", "Repeat1", "Repeat2", "Repeat2"),
                  id2 = c("Fold1", "Fold2", "Fold1", "Fold2", "Fold1",
                          "Fold2", "Fold1", "Fold2"),
           .lambda = c(1, 1, 1, 1, 10, 10, 10, 10),
              rsqTrad = c(0.664054033251612,
                          0.627244721648572, 0.684163742190838, 0.692049617785175,
                          0.664633782152785, 0.627420275433845, 0.684194968923284,
                          0.688489759886076),
            rsqAdj = c(0, 0, 0, 0, 0, 0, 0, 0),
              rsqPred = c(0.692055217195337,
                          0.627618803015662, 0.694093550359917, 0.711782083341488,
                          0.692370401306606, 0.627815423878544, 0.694026767261078,
                          0.709495722015926))
  )


  # summary
  summ <- summary(cvreg)
  expect_s3_class(summ, "summary_cvGlmmLasso")
  expect_named(summ, c("rsqAdj", "rsqTrad", "rsqPred"))

  # Rsq Trad
  expect_s3_class(summ$rsqTrad, "tbl_df")
  expect_equal(summ$rsqTrad,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.666878028719049, 0.666184696598998),
                      sd = c(0.0289319821079573, 0.0278506220237202),
                  median = c(0.674108887721225, 0.674414375538035),
                     min = c(0.627244721648572, 0.627420275433845),
                     max = c(0.692049617785175, 0.688489759886076),
                      lb = c(0.630005420018800, 0.630211288437765),
                      ub = c(0.691458177115600, 0.688167650563867),
                       n = c(4L, 4L))
  )

  # R squared adjusted
  expect_s3_class(summ$rsqAdj, "tbl_df")
  expect_equal(summ$rsqAdj,
               tibble::tibble(
                  .lambda = c(1, 10),
                     mean = c(0, 0),
                       sd = c(0, 0),
                   median = c(0, 0),
                      min = c(0, 0),
                      max = c(0, 0),
                       lb = c(0, 0),
                       ub = c(0, 0),
                        n = c(4L, 4L))
  )

  # R squared pred
  expect_s3_class(summ$rsqPred, "tbl_df")
  expect_equal(summ$rsqPred,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.681387413478101, 0.680927078615539),
                      sd = c(0.0369240092522227, 0.0362379484861848),
                  median = c(0.693074383777627, 0.693198584283842),
                     min = c(0.627618803015662, 0.627815423878544),
                     max = c(0.711782083341488, 0.709495722015926),
                      lb = c(0.6324515340791383, 0.632657047185649),
                      ub = c(0.710455443367870, 0.708335550409313),
                       n = c(4L, 4L))
  )
})

# Test down-sampling ----
paste("`cross_validate_glmm_lasso()` classifcation with",
      "down-sampling returns expected output") |>
test_that({
  cvclass_down <- cross_validate_glmm_lasso(
    data               = log_data,
    fixed              = myclassform,
    random             = myrandomform,
    family             = stats::binomial(link = "logit"),
    decision_threshold = 0.5,
    subsample          = "down",
    folds              = 2,
    .repeats           = 2,
    .lambda            = c(1, 10),
    r_seed             = 10
    )

  expect_s3_class(cvclass_down, "cvGlmmLasso")
  expect_equal(cvclass_down$random, myrandomform)
  expect_equal(cvclass_down$model_type, "classification")
  expect_equal(cvclass_down$random_seed, 10)
  expect_equal(cvclass_down$metrics,
               tibble::tibble(
                   id = c("Repeat1", "Repeat1", "Repeat2", "Repeat2",
                          "Repeat1", "Repeat1", "Repeat2", "Repeat2"),
                  id2 = c("Fold1", "Fold2", "Fold1", "Fold2",
                          "Fold1", "Fold2", "Fold1", "Fold2"),
           .lambda = c(1, 1, 1, 1, 10, 10, 10, 10),
          Sensitivity = c(0.768421052631579, 0.789473684210526, 0.705263157894737,
                          0.684210526315789, 0.663157894736842, 0.6,
                          0.557894736842105, 0.589473684210526),
          Specificity = c(0.854545454545454,
                          0.727272727272727, 0.945454545454545, 0.763636363636364,
                          0.563636363636364, 0.654545454545455, 0.745454545454545,
                          0.672727272727273),
                  auc = c(0.818755980861244,
                          0.761148325358851, 0.834066985645933, 0.75043062200957,
                          0.703349282296651, 0.671196172248804, 0.670239234449761,
                          0.659712918660287))
   )

  summ <- summary(cvclass_down)
  expect_s3_class(summ, "summary_cvGlmmLasso")
  expect_named(summ, c("auc", "Sensitivity", "Specificity"))

  # AUC
  expect_s3_class(summ$auc, "tbl_df")
  expect_equal(summ$auc,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.7911004784689, 0.676124401913876),
                      sd = c(0.0414813863956568, 0.0188807974597789),
                  median = c(0.789952153110048, 0.670717703349282),
                     min = c(0.75043062200957, 0.659712918660287),
                     max = c(0.834066985645933, 0.703349282296651),
                      lb = c(0.751234449760766, 0.660502392344498),
                      ub = c(0.832918660287082, 0.700937799043062),
                       n = c(4L, 4L))
  )

  # Sensitivity
  expect_s3_class(summ$Sensitivity, "tbl_df")
  expect_equal(summ$Sensitivity,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.736842105263158, 0.602631578947368),
                      sd = c(0.0501152872178446, 0.0441394583347689),
                  median = c(0.736842105263158, 0.594736842105263),
                     min = c(0.684210526315789, 0.557894736842105),
                     max = c(0.789473684210526, 0.663157894736842),
                      lb = c(0.685789473684211, 0.560263157894737),
                      ub = c(0.787894736842105, 0.658421052631579),
                       n = c(4L, 4L))
  )

  # Specificity
  expect_s3_class(summ$Specificity, "tbl_df")
  expect_equal(summ$Specificity,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.822727272727273, 0.659090909090909),
                      sd = c(0.097771307908495, 0.0747815919954732),
                  median = c(0.809090909090909, 0.663636363636364),
                     min = c(0.727272727272727, 0.563636363636364),
                     max = c(0.945454545454545, 0.745454545454545),
                      lb = c(0.73, 0.570454545454545),
                      ub = c(0.938636363636364, 0.74),
                       n = c(4L, 4L))
  )
})

# Test up-sampling ----
test_that("`cross_validate_glmm_lasso()` classifcation up-sampling returns as expected", {
  cvclass_up <- cross_validate_glmm_lasso(
    data               = log_data,
    fixed              = myclassform,
    random             = myrandomform,
    family             = stats::binomial(link = "logit"),
    decision_threshold = 0.5,
    subsample          = "up",
    folds              = 2,
    .repeats           = 2,
    .lambda            = c(1, 10),
    r_seed             = 10
  )

  expect_s3_class(cvclass_up, "cvGlmmLasso")
  expect_equal(cvclass_up$random, myrandomform)
  expect_equal(cvclass_up$model_type, "classification")
  expect_equal(cvclass_up$random_seed, 10)
  expect_equal(cvclass_up$metrics,
               tibble::tibble(
                   id = c("Repeat1", "Repeat1", "Repeat2", "Repeat2", "Repeat1",
                          "Repeat1", "Repeat2", "Repeat2"),
                  id2 = c("Fold1", "Fold2", "Fold1", "Fold2",
                          "Fold1", "Fold2", "Fold1", "Fold2"),
           .lambda = c(1, 1, 1, 1, 10, 10, 10, 10),
          Sensitivity = c(0.884210526315789,
                          0.831578947368421, 0.694736842105263, 0.736842105263158,
                          0.663157894736842, 0.715789473684211, 0.621052631578947,
                          0.705263157894737),
          Specificity = c(0.727272727272727,
                          0.818181818181818, 0.909090909090909, 0.6, 0.654545454545455,
                          0.581818181818182, 0.8, 0.636363636363636),
                  auc = c(0.750813397129186,
                          0.901818181818181, 0.831770334928229, 0.640765550239234,
                          0.69933014354067, 0.74066985645933, 0.734545454545454,
                          0.674832535885167))
   )


  summ <- summary(cvclass_up)
  expect_s3_class(summ, "summary_cvGlmmLasso")
  expect_named(summ, c("auc", "Sensitivity", "Specificity"))

  # AUC
  expect_s3_class(summ$auc, "tbl_df")
  expect_equal(summ$auc,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.781291866028708, 0.712344497607655),
                      sd = c(0.112177316655442, 0.0309393445690135),
                  median = c(0.791291866028708, 0.716937799043062),
                     min = c(0.640765550239234, 0.674832535885167),
                     max = c(0.901818181818181, 0.74066985645933),
                      lb = c(0.649019138755981, 0.67666985645933),
                      ub = c(0.896564593301435, 0.740210526315789),
                       n = c(4L, 4L))
  )

  # Sensitivity
  expect_s3_class(summ$Sensitivity, "tbl_df")
  expect_equal(summ$Sensitivity,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.786842105263158, 0.676315789473684),
                      sd = c(0.0865358768781651, 0.043294605892116),
                  median = c(0.784210526315789, 0.684210526315789),
                     min = c(0.694736842105263, 0.621052631578947),
                     max = c(0.884210526315789, 0.715789473684211),
                      lb = c(0.697894736842105, 0.624210526315789),
                      ub = c(0.880263157894737, 0.715),
                       n = c(4L, 4L))
  )

  # Specificity
  expect_s3_class(summ$Specificity, "tbl_df")
  expect_equal(summ$Specificity,
               tibble::tibble(
                 .lambda = c(1, 10),
                    mean = c(0.763636363636364, 0.668181818181818),
                      sd = c(0.131948733679256, 0.09315409787236),
                  median = c(0.772727272727273, 0.645454545454546),
                     min = c(0.6, 0.581818181818182),
                     max = c(0.909090909090909, 0.8),
                      lb = c(0.609545454545455, 0.585909090909091),
                      ub = c(0.902272727272727, 0.789090909090909),
                       n = c(4L, 4L))
  )
})

# Test errors ----
test_that("`cross_validate_glmm_lasso()` throws error when wrong parameters passed", {
  expect_error(
    cross_validate_glmm_lasso(
      data               = log_data,
      fixed              = myregform,
      random             = myrandomform,
      family             = stats::gaussian(link = "identity"),
      .lambda            = -10),
    "The elastic net values for lambda must be >= 0"
  )
  expect_error(
    cross_validate_glmm_lasso(
      fixed              = myregform,
      random             = myrandomform,
      family             = stats::gaussian(link = "identity"),
      .lambda            = 1),
    "Data set is needed for cross-validation"
  )
})
