#' Fit Linear Mixed-Effect Safely
#'
#' In multidimensional data analysis, fitting thousands of identical
#' mixed models often results in at least *some* models failing
#' to converge on a final solution for the coefficients. This function
#' provides a convenient manner in which to bypass such cases and
#' continue fitting the remainder of the response covariates without
#' becoming hung up on convergence issues.
#'
#' This function attempts to fit a linear mixed-effects model specified by
#' the argument `formula` via a specific sequence of methods:
#'
#' \enumerate{
#'   \item use the default options of [nlme::lme()], if fails ...
#'   \item use `method = "ML"` (Maximum Likelihood) option, if fails ...
#'   \item use `method = "REML"`, restricted expectation maximum likelihood;
#'   the default but with `control = lmeControl(opt = "optim")` option
#'   (the old optimizer), if fails ...
#'   \item use both `method = "ML"` and `control = lmeControl(opt = "optim")`
#'   options, if fails ...
#'   \item suppress warnings and simply return the *non-converged*
#'   object. Beware of this feature! Non-converged models should *not*
#'   be used in analyses. To identify these cases, a boolean entry named
#'   "converged" is added to the model object indicating the convergence
#'   status of the model.
#' }
#'
#' @param formula The model specification to fit in "formula" syntax,
#' according to [lme()].
#' @param ... Additional arguments passed to [lme()].
#' @return A linear mixed-effects model.
#' @note The data used to fit the model is *not* included
#' in the final model, as specified by the `keep.data = FALSE`
#' argument in [lme()].
#' @author Stu Field
#' @seealso [lme()]
#' @examples
#' long <- simulateLongData(r.seed = 101)
#' fit  <- fit_lme_safely(yij ~ time, random = ~1|pid, data = long)
#' summary(fit)
#' anova(fit)
#' @importFrom nlme lme lmeControl lme.formula
#' @export
fit_lme_safely <- function(formula, ...) {

  .call        <- match.call(expand.dots = TRUE)
  .call[[1]]   <- as.name("lme.formula")
  names(.call) <- gsub("^formula$", "fixed", names(.call))

  formals(lme.formula)$keep.data <- FALSE         # hijack default
  lme_safe <- be_safe(nlme::lme)

  model <- lme_safe(formula, ..., method = "REML",   # using the defaults
                    control = lmeControl(opt = "nlminb"))$result

  apt <- as.character(formula[[2L]])[1L]

  if ( is.null(model) ) {
    signal_done("Refitting with new defaults ...",  value(apt))
    model <- lme_safe(formula, ..., method = "ML",     # use ML
                      control = lmeControl(opt = "nlminb"))$result
  }

  if ( is.null(model) ) {
    signal_done("Refitting with new defaults ...",  value(apt))
    model <- lme_safe(formula, ..., method = "REML", # use old optimizer with REML
                      control = lmeControl(opt = "optim"))$result
  }

  if ( is.null(model) ) {
    signal_done("Refitting with new defaults ...",  value(apt))
    model <- lme_safe(formula, ..., method = "ML",  # use old optimizer with ML
                      control = lmeControl(opt = "optim"))$result
  }

  if ( is.null(model) ) {
    signal_oops("No convergence for ...", value(apt))
    model <- withr::with_options(
      c(warn = -1),                 # give up and return non-converged object
      nlme::lme(formula, ..., control = lmeControl(returnObject = TRUE))
    )
    model$converged <- FALSE
  } else {
    model$converged <- TRUE
  }
  model$call <- .call
  model
}
