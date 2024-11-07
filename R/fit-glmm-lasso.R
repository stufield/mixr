#' Fit a `glmmLasso` object from a `cv_glmm_lasso` object
#'
#' From a `cv_glmm_lasso` object, find the best parameters from the cross-
#'   validation and re-fit via [glmmLasso::glmmLasso()].
#'   See `[cross_validate_glmm_lasso()]`.
#'
#' @family GLMMlasso
#'
#' @param object A `cv_glmm_lasso` class object to use to fit
#'   a `glmm_lasso` object.
#' @param data Data to fit model with. Typically a `data.frame` object.
#' @param metric Which performance metric to use to choose
#'   lambda for lasso models.
#' @param ... Additional parameters passed to [glmmLasso::glmmLasso()]
#'   model fitting function.
#' @param verbose `logical(1)`. Toggle verbosity.
#'
#' @return A `glmmLasso` class object. See [glmmLasso::glmmLasso()].
#'
#' @importFrom dplyr filter select sample_n
#' @importFrom rlang exec
#' @export
fit_glmm_lasso <- function(object, data,
                           metric = c("auc", "sensitivity", "specificity",
                                      "rsqTrad", "rsqAdj", "rsqPred"),
                           verbose = interactive(), ...) {

  # check is_cv_glmm_lasso class
  if ( !is_cv_glmm_lasso(object) ) {
    stop(
      "Your object is not the output of `cross_validate_glmm_lasso()`. ",
      "Please check your input!", call. = FALSE
    )
  }

  # capture and set dots
  dots        <- list(...)
  dots$fix    <- object$fixed
  dots$rnd    <- object$random
  dots$family <- object$family
  dots$data   <- data.frame(data)

  # set lambda based on metric if not specified ---
  if ( !"lambda" %in% names(dots) ) {
    .metric     <- match.arg(metric)
    tune_params <- summary(object)[[.metric]] |>
      dplyr::filter(mean == max(mean)) |>
      dplyr::filter(median == max(median)) |>
      # what happens if there's still a tie?
      # should mostly happen when lambda == 0, so alpha doesn't matter
      dplyr::sample_n(1L) |>
      dplyr::select(.lambda)
    if ( verbose ) {
      signal_info(
        "Lambda was not specified, therefore they will be taken from the ",
        "optimal value of",  value(.metric), ":\n",
        "\tlambda =", value(round(tune_params$.lambda, 3L))
      )
    }
    dots$lambda <- tune_params$.lambda
  }

  # fit model
  safe_mod_fit <- be_safe(glmmLasso::glmmLasso)
  mod <- rlang::exec(safe_mod_fit, !!!dots, .env = environment())

  if ( is.null(mod$error) ) {
    mod$result
  } else {
    mod$error
  }
}
