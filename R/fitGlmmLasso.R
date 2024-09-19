#' Fit a `glmmLasso` object from a `cvGlmmLasso` object
#'
#' From a `cvGlmmLasso` object, find the best parameters from the cross-
#' validation and re-fit via [glmmLasso::glmmLasso()]. See `[crossValidateGlmmLasso()]`.
#'
#' @family GLMMlasso
#' @param object A `cvGlmmLasso` class object to use to fit a `glmmLasso` object.
#' @param data Data to fit model with. Typically a `soma_adat` class object.
#' @param metric Which performance metric to use to choose lambda for lasso models.
#' @param ... Additional parameters passed to [glmmLasso::glmmLasso()]
#' model fitting function.
#' @param verbose Logical. Toggle verbosity.
#' @return A `glmmLasso` class object.
#' @importFrom dplyr filter select sample_n
#' @importFrom rlang exec
#' @export
fitGlmmLasso <- function(object, data,
                         metric = c("auc", "sensitivity",
                                    "specificity", "rsqTrad",
                                    "rsqAdj", "rsqPred"),
                         verbose = interactive(), ...) {

  # check is.cvGlmmLasso class
  if ( !is.cvGlmmLasso(object) ) {
    stop(
      "Your object is not the output of `crossValidateGlmmLasso()`. ",
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
