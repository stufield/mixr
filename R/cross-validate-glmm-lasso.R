#' Preform Cross-Validated `glmm` Lasso
#'
#' Fit cross-validated models for [glmmLasso::glmmLasso()].
#'
#' @family GLMMlasso
#'
#' @param data The `data.frame` containing analysis data as columns.
#' @param fixed The formula for the fixed effect. For
#'   classification problems, the LHS of the fixed formula (endpoint) needs to be
#'   re-coded to 1 or 0 (not strings or factors).
#' @param random List. Indicating the full formula for the random
#'   effect. For `glmmLasso` the formula  should be of the format
#'   `list(randomEffectTerm = ~1)`.
#' @param family `character(1)`. Which family of models to fit to the
#'   generalized linear mixed model? This should be a `glm` family.
#'   If no family is provided, a linear mixed
#'   model is fit instead of a generalized linear mixed model.
#' @param folds `integer(1)`. Number of folds to perform (k-fold cross-validation).
#' @param .repeats `integer(1)`. Number of repeats to perform as part of
#'   repeated k-fold cross-validation. Default is 1 (no repeats).
#' @param .lambda The values for lambda in the lasso:
#'   default is \verb{[0, 0.01, 0.1, 1, 10, 100, 1000]}.
#' @param decision_threshold `numeric(1)`. The decision threshold
#'   to use for classification.
#' @param subsample `character(1)`. The type of sub-sampling to perform.
#'   See [splyr::rebalance()] `method =` argument.
#'   Defaults to "none" (`NULL`).
#' @param r_seed `integer(1)`. Value to set the random seed.
#' @param save_fits `logical(1)`. Should individual model fits for each
#'   `<repeat/fold/tuning parameter>` iteration be saved?
#' @param save_splits `logical(1)`. Should individual split
#'   data sets for each `<repeat/fold/tuning parameter>`
#'   iteration be saved? Is the output of [rsample::vfold_cv()]
#'   such that the splits need to be passed to
#'   [rsample::analysis()] or [rsample::assessment()] to be accessed?
#' @param ... Additional arguments passed to the underlying
#'   [glmmLasso::glmmLasso()] model fitting function.
#'
#' @return A `cv_glmm_lasso` class object with components:
#'   \item{metrics}{A `tibble` of cross-validation metrics.}
#'   \item{formula}{The mixed-effects model formula.}
#'   \item{model_type}{Either `classification` or `regression`.}
#'   \item{random_seed}{Random seed for the cross-validation folds.}
#'
#' The `metrics` entry is a `tibble` where the number of rows is equal
#' to the cross of `.repeats`, `folds`, and `.lambda` and
#' contains the following columns:
#'   * `repeats` (id)
#'   * `folds`   (id2)
#'   * `.lambda`
#'   * `Sensitivity`
#'   * `Specificity`
#'   * `auc`
#'   * `.split` (optional)
#'     + A list column of `tibbles` of the data splits: `"vfold_cv"`, `"rsplit"`
#'   * `fit` (optional)
#'     + A list column of model fit objects (`glmmLasso`)
#'
#' @author Gargi Datta & Stu Field
#' @seealso [fit_glmm_lasso()]
#'
#' @importFrom purrr pmap
#' @importFrom dplyr select filter bind_rows starts_with all_of
#' @importFrom tidyr unnest
#' @importFrom splyr rebalance
#' @importFrom libml calc_confusion calc_emp_auc
#' @importFrom stats cor
#' @importFrom tibble tibble
#' @export
cross_validate_glmm_lasso <- function(data,
                                      fixed,
                                      random,
                                      family,
                                      folds = 10L,
                                      .repeats = 1,
                                      .lambda,
                                      decision_threshold = 0.5,
                                      subsample = NULL,
                                      r_seed = 1234,
                                      save_fits = FALSE,
                                      save_splits = FALSE, ...) {

  dots  <- list(...)

  if ( "random.seed" %in% names(dots) ) {
    stop(
      "Please pass 'r_seed' argument as an integer. ",
      "You have passed 'random.seed' with value ",
      value(dots$random.seed), ".", call. = FALSE
    )
  }

  if ( missing(data) ) {
    stop("Data set is needed for cross-validation.", call. = FALSE)
  }

  # pre-proccessing ----
  #   Find intersection of all variables and predictors
  #   and subset the unique ones to get the LHS of
  #   the formula (response variables)
  stopifnot(inherits(fixed, "formula"), length(fixed) == 3L)
  responses <- all.vars(fixed[[2L]])

  # used later in `rebalance()`
  event <- responses[length(responses)]

  # check if this is classification or regression
  if ( all(unique(data[[event]]) %in% c(0,  1) ) || inherits(data[[event]], "factor") ) {
    model_type <- "classification"
  } else {
    model_type <- "regression"
  }

  if ( missing(.lambda) ) {
    signal_info(
      "The `.lambda` argument is missing, setting to:",
      "c(0, 0.01, 0.1, 1.0, 10, 10, 1000)"
    )
    .lambda <- c(0, 10^(-2:3))    # nolint
  } else if ( any(.lambda < 0) ) {
    stop("The elastic net values for lambda must be >= 0.", call. = FALSE)
  }

  dots$fixed  <- fixed
  dots$random <- random
  dots$family <- family
  # end of pre-proccessing ----


  mod_fit <- function(.data, fixed, random, family, ...) {
    environment(fixed) <- environment()
    tryCatch(glmmLasso::glmmLasso(fix = fixed, rnd = random,
                                  family = family, data = .data, ...),
             error = function(e) NULL, silent = FALSE)
  }

  # silence "Algorithm did not converge!" cat
  quiet_glmm <- be_quiet(mod_fit)

  # set up metric functions ----
  # Internal closures
  get_classification_metrics <- function(split, mod, probs, event,
                                         decision_threshold, ...) {
    auc <- calc_emp_auc(truth = split[[event]], # 0/1 integer
                        predicted = probs,
                        pos.class = 1L)
    conf <- summary(
      calc_confusion(
        truth     = factor(split[[event]]), # nolint: indentation_linter.
        predicted = probs,
        cutoff    = decision_threshold,
        pos.class = 1L)
    )$metrics

    tibble(
      Sensitivity = dplyr::filter(conf, metric == "Sensitivity")$estimate,
      Specificity = dplyr::filter(conf, metric == "Specificity")$estimate,
      auc         = auc
    )
  }

  get_regression_metrics <- function(split, mod, probs, event, ...) {
    p       <- length(mod$coefficients[mod$coefficients != 0])
    n       <- length(split)
    yhat    <- probs
    yobs    <- split[[event]]
    rsqTrad <- 1 - sum((yhat - yobs)^2) / sum((yobs - mean(yobs))^2)
    # r-squared is adjusted for number of observations and features, but
    # clipped to be greater than or equal to 0
    rsqAdj  <- (1 - (1 - rsqTrad) * (n - 1) / (n - p - 1)) |> max(0)
    rsqPred <- stats::cor(yhat, yobs)^2
    tibble(rsqTrad = rsqTrad, rsqAdj = rsqAdj, rsqPred = rsqPred)
  }

  .fun <- switch(model_type,
                 classification = get_classification_metrics,
                 regression     = get_regression_metrics)

  # CV actually happens -----
  data_sample <- withr::with_seed(r_seed,
    rsample::vfold_cv(data, v = folds,
                      repeats = .repeats,
                      strata = all_of(event))
  )
  rep_length  <- length(.lambda)

  # Start building CV elements ----
  metrics <- .expand_grid(.split  = data_sample$splits,
                          .lambda = .lambda)

  # add analysis/assessment columns
  metrics$.anal  <- lapply(metrics$.split,
                           function(.x) data.frame(rsample::analysis(.x)))
  metrics$.asses <- lapply(metrics$.split,
                           function(.x) data.frame(rsample::assessment(.x)))

  # Fit models ----
  metrics$fit <- purrr::pmap(metrics, function(.anal, .lambda, ...) {
    if ( !is.null(subsample) ) {
      .anal <- withr::with_seed(
        r_seed + 1, splyr::rebalance(.anal, event, subsample)
      )
    }
    dots$.data  <- .anal
    dots$lambda <- if ( is.na(.lambda) ) NULL else .lambda  # nolint
    do.call(quiet_glmm, dots)$result
  })

  # Calculate Metrics ----
  metrics$metrics <- purrr::pmap(metrics, function(.asses, .lambda, fit, ...) {
    if ( is.null(fit) ) return(NA)     # did not converge
    dots$split <- .asses
    dots$mod   <- fit
    dots$event <- event
    dots$probs <- predict(fit, .asses)
    dots$decision_threshold <- decision_threshold
    dots$lambda <- if ( is.na(.lambda) ) NULL else .lambda # nolint
    do.call(.fun, dots)
  })

  # Add repeat + fold info for reference ----
  fold_rep <- dplyr::select(data_sample, starts_with("id"))
  bind_met <- dplyr::bind_rows(replicate(rep_length, list(fold_rep)))

  metrics <- dplyr::mutate_at(metrics, ".lambda", unlist) |> # rm .lambda if all NAs
    dplyr::select(-.anal, -.asses)
  metrics <- dplyr::bind_cols(bind_met, metrics) |> unnest(metrics)

  if ( !save_splits ) {
    metrics <- dplyr::select(metrics, -.split)
  }

  if ( !save_fits ) {
    metrics <- dplyr::select(metrics, -fit)
  }

  out <- list()
  out$metrics     <- metrics
  out$fixed       <- fixed
  out$random      <- random
  out$family      <- family
  out$model_type  <- model_type
  out$folds       <- folds
  out$.repeats    <- .repeats
  out$random_seed <- r_seed
  out$weights     <- dots$weights
  add_class(out, "cv_glmm_lasso")
}


#' Check for `cv_glmm_lasso` object
#'
#' For `is.cvGlmmLasso`: A logical test for objects of class `cv_glmm_lasso`.
#'
#' @rdname cross_validate_glmm_lasso
#' @param x An object to be tested for class `cv_glmm_lasso`.
#'
#' @export
is_cv_glmm_lasso <- function(x) inherits(x, "cv_glmm_lasso")


#' @noRd
#' @export
print.cv_glmm_lasso <- function(x, ...) {
  signal_rule("Cross-validated Glmm Lasso", lty = "double", line_col = "blue")
  left <- c("Response",
            "Random effect",
            "Family",
            "Folds",
            "Repeats",
            "Model Type",
            "Random Seed") |> pad(17)
  right <- list(as.character(x$fixed)[2L],
                sprintf("%s = %s", names(x$random), as.character(x$random)),
                c(x$family$family, x$family$link),
                x$folds, x$.repeats,
                x$model_type, x$random_seed)
  .todo <- function(.x, .y) signal_todo(.x, value(.y))
  invisible(liter(left, right, .todo))
  cat("\n")
  signal_rule("Metrics", line_col = "blue")
  print(x$metrics)
  signal_rule(lty = "double", line_col = "green")
  invisible(x)
}


#' @describeIn cross_validate_glmm_lasso
#'   S3 summary method for class `cv_glmm_lasso`.
#'
#' @param object A `cv_glmm_lasso` class object.
#' @param ci_alpha `double(1)`. Significance level of the confidence
#'   interval for `alpha`.
#'
#' @importFrom dplyr select filter n any_of
#' @importFrom rlang syms
#' @importFrom tidyr drop_na unnest
#' @importFrom stats quantile setNames
#' @export
summary.cv_glmm_lasso <- function(object, ci_alpha = 0.05, ...) {

  object$metrics$.row <- seq_len(nrow(object$metrics))   # add row id

  if ( object$model_type == "classification" ) {
    modl_metr <- c("Sensitivity", "auc", "Specificity")
  } else {
    modl_metr <- c("rsqTrad", "rsqAdj", "rsqPred")
  }

  if ( !all(modl_metr %in% names(object$metrics)) ) {
    out <- as.list(rep(NA_real_, length(modl_metr))) |> setNames(modl_metr)
  } else {
    metrics <- drop_na(object$metrics, any_of(modl_metr))
    if ( nrow(object$metrics) > nrow(metrics) ) {
      bad_rows <- setdiff(object$metrics$.row, metrics$.row)   # nolint; var used
      signal_info(
        paste0(
          "Some combinations of .lambda did not converge ",
          "resulting in NAs for that combination.\n",
          "They were not summarized.\n",
          "Please see -> object$metrics$metrics [ c(", value(bad_rows), "), ]"
        )
      )
    }
  }

  # manage edge-case for all NAs within-group;
  #   avoids warning for `max(NA, na.rm = TRUE)` and breaking `dplyr::summarize()`
  #   x = double; y = function returning scalar
  `%na%` <- function(x, y) {
    if ( all(is.na(x)) ) NA_real_ else y(x, na.rm = TRUE)
  }
  upper_ <- be_hard(stats::quantile, probs = (1 - ci_alpha / 2), names = FALSE)
  lower_ <- be_hard(stats::quantile, probs = (ci_alpha / 2), names = FALSE)

  .perf_metric <- function(.v) {   # .v a  vector to summarize
    data.frame(
      mean   = .v %na% base::mean,
      sd     = .v %na% stats::sd,
      median = .v %na% stats::median,
      min    = .v %na% base::min,
      max    = .v %na% base::max,
      lb     = .v %na% lower_,
      ub     = .v %na% upper_,
      n      = dplyr::n()
    )
  }

  # --- Create summary list ----
  out <- list()

  if ( ".lambda" %in% names(metrics) ) {
    groupCI   <- ".lambda"
    groupREST <- ".lambda"
  } else {
    groupCI   <- NULL
    groupREST <- NULL
  }
  groupCI   <- rlang::syms(groupCI)
  groupREST <- rlang::syms(groupREST)

  if ( "auc" %in% names(metrics) ) {
    out$auc <- metrics |>
      unnest(auc) |>
      dplyr::group_by(!!!groupREST) |>
      dplyr::summarise(.perf_metric(auc)) |>
      dplyr::ungroup()
  }

  if ( "Sensitivity" %in% names(metrics) ) {
    out$Sensitivity <- metrics |>
      unnest(Sensitivity) |>
      dplyr::group_by(!!!groupREST) |>
      dplyr::summarise(.perf_metric(Sensitivity)) |>
      dplyr::ungroup()
  }

  if ( "Specificity" %in% names(metrics) ) {
    out$Specificity <- metrics |>
      unnest(Specificity) |>
      dplyr::group_by(!!!groupREST) |>
      dplyr::summarise(.perf_metric(Specificity)) |>
      dplyr::ungroup()
  }

  if ( "rsqAdj" %in% names(metrics) ) {
    out$rsqAdj <- metrics |>
      unnest(rsqAdj) |>
      dplyr::group_by(!!!groupREST) |>
      dplyr::summarise(.perf_metric(rsqAdj)) |>
      dplyr::ungroup()
  }

  if ( "rsqTrad" %in% names(metrics) ) {
    out$rsqTrad <- metrics |>
      unnest(rsqTrad) |>
      dplyr::group_by(!!!groupREST) |>
      dplyr::summarise(.perf_metric(rsqTrad)) |>
      dplyr::ungroup()
  }

  if ( "rsqPred" %in% names(metrics) ) {
    out$rsqPred <- metrics |>
      unnest(rsqPred) |>
      dplyr::group_by(!!!groupREST) |>
      dplyr::summarise(.perf_metric(rsqPred)) |>
      dplyr::ungroup()
  }

  add_class(out, "summary_cvGlmmLasso")
}
