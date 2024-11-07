#' Simulate Longitudinal Data in R
#'
#' This function creates simulated longitudinal data that is useful
#'   for fitting of mixed-effects modeling, general longitudinal
#'   visualization, and feature analogues to serially dependent data.
#'   In general, this function is intended to be used in support
#'   of [create_long_data()], which wraps this the simulation aspect
#'   into an overall data set.
#'
#' @param nsubj `integer(1)`. Number of subjects.
#' @param beta0 `numeric(1)`. The intercept coefficient.
#' @param beta1 `numeric(1)`. The slope coefficient.
#' @param max_obs `integer(1)`. Number of serial (longitudinal)
#'   observations per subject.
#' @param group `character(1)`. String to name the subject group type.
#' @param auto_cor `numeric(1)`. The coefficient for the serial
#'   autocorrelation in \verb{[0, 1]}.
#' @param sd_pars A list (all `numeric(1)`) containing:
#' \describe{
#'   \item{sigma:}{true error SD.}
#'   \item{tau0:}{intercept SD.}
#'   \item{tau1:}{slope SD.}
#'   \item{tau01:}{intercept-slope correlation.}
#' }
#' @param r_seed `integer(1)`. Random number generator for
#'   reproducibility of serial sample draws.
#'
#' @seealso [MASS::mvrnorm()]
#' @references
#' \url{http://stats.stackexchange.com/questions/76999/simulating-longitudinal-lognormal-data-in-r}
#'
#' @return A `tibble` of simulated data.
#'
#' @examples
#' long <- simulate_long_data()  # default pars
#' dim(long)
#' head(long)
#' @importFrom stats arima.sim
#' @importFrom tibble tibble
#' @export
simulate_long_data <- function(nsubj = 20L, beta0 = 1000,
                               beta1 = 10, max_obs = 10,
                               group = NULL, auto_cor = 0.5,
                               sd_pars = list(sigma = beta0 / 4,
                                              tau0  = 2.0,
                                              tau1  = 2.0,
                                              tau01 = 0.5),
                               r_seed = 1234L) {

  # assign elements of sd_pars to current
  # environment for downstream use
  sigma <- sd_pars$sigma
  tau0  <- sd_pars$tau0
  tau1  <- sd_pars$tau1
  tau01 <- sd_pars$tau01
  pars  <- vapply(list(sigma = sigma, tau0 = tau0, tau1 = tau1, tau01 = tau01),
                  is.null, NA)

  # If user missed some pars, replace with defaults with a warning
  if ( any(pars) ) {
    if ( pars["sigma"] ) sigma <- beta0 / 4
    if ( pars["tau0"] ) tau0   <- 2.0
    if ( pars["tau1"] ) tau1   <- tau0
    if ( pars["tau01"] ) tau01 <- 0.5
    missing  <- names(pars)[pars]
    def_pars <- vapply(missing, get, envir = environment(), double(1))  # nolint
    warning(
      "The `sd.pars` argument list is missing these parameter(s): ",
      value(missing), ".\nUsing default parameters: ", value(def_pars), ".",
      call. = FALSE
    )
  }

  # simulate number of observations for each individual
  get_timecourse <- function() {
    p <- sample(3:max_obs, nsubj, replace = TRUE)   # min longit. samples = 3
    # simulate observation moments (assume everybody has 1st obs)
    times <- lapply(p, function(.x) c(1, sort(sample(2:max_obs, .x - 1))))
    list(p = p, times = unlist(times))
  }

  tseries <- withr::with_seed(r_seed, get_timecourse())

  # set up data frame ----
  df <- tibble(pid = rep(1:nsubj, times = tseries$p), time = tseries$times)

  # simulate (correlated) random effects for intercepts and slopes
  mu  <- numeric(2L)
  S   <- matrix(c(1, tau01, tau01, 1), nrow = 2)   # correlation matrix
  tau <- c(tau0, tau1)
  S   <- diag(tau) %*% S %*% diag(tau)          # convert to covariance matrix
  U   <- withr::with_seed(r_seed, MASS::mvrnorm(nsubj, mu = mu, Sigma = S))

  # simulate AR(1) errors and then the actual outcomes
  # note: use arima.sim(model=list(ar=ar.val), n=x) * sqrt(1-ar.val^2) * sigma
  # construction, so that the true error SD is equal to sigma
  eij <- withr::with_seed(
    r_seed,
    lapply(tseries$p, function(.x) {
      stats::arima.sim(model = list(ar = auto_cor), n = .x) *
        sqrt(1 - auto_cor^2) * sigma
    })
  )
  df$eij <- round(unlist(eij, use.names = FALSE), 1L)

  df$yij <- ((beta0 + rep(U[, 1L], times = tseries$p)) +
               (beta1 + rep(U[, 2L], times = tseries$p)) * # nolint
               df$time + df$eij) |>                        # nolint
    round(1L)

  if ( !is.null(group) ) {
    df$Group <- group
    df$pid   <- sprintf("%s_%03i", df$Group, df$pid)
  }
  df
}


#' @describeIn simulate_long_data
#'   To generate longitudinal data there is a wrapper around [simulate_long_data()]
#'   that creates full data set with serial dependence for analysis, see examples.
#'
#' @param ... Arguments containing a list of parameters to be passed
#'   to [simulate_long_data()].
#' @return A `tibble` with a response and time/grouping fields.
#' @author Stu Field
#' @examples
#' # single group: time response
#' p <- list(nsubj = 20, beta0 = 1000, beta1 = 400, max_obs = 10, r_seed = 101,
#'           sd_pars = list(sigma = 250), auto_cor = 0.1)
#' longData <- create_long_data(subject = p)
#' head(longData)
#' table(longData$pid)
#'
#' # simulate longitudinal data with group-specific response
#' p1 <- list(nsubj = 20, beta1 = 0, max_obs = 10, r_seed = 10,
#'            sd_pars = list(sigma = 150), auto_cor = 0.1)
#' p2 <- list(nsubj = 20, beta1 = 400, max_obs = 10, r_seed = 101,
#'            sd_pars = list(sigma = 350), auto_cor = 0.1)
#' groupResponseData <- createLongData(p1, p2)
#' head(groupResponseData)
#'
#' # There are 2 ways to rename the group labels:
#' # rename via the '...'
#' groupResponseData <- create_long_data(control = p1, treatment = p2)
#' head(groupResponseData)
#'
#' # rename via parameters list
#' p1 <- c(p1, group = "control")
#' p2 <- c(p2, group = "treatiment")
#' groupResponseData <- create_long_data(p1, p2)
#' head(groupResponseData)
#' @export
create_long_data <- function(...) {
  call <- match.call(expand.dots = TRUE)
  args <- list(...)
  if ( is.null(names(args)) ) {
    names(args) <- vapply(as.list(call), deparse, "")[-1L]
  }
  arg_list <- liter(args, .f = function(.x, .y) {
    .x$group <- .x$group %||% .y
    do.call(simulate_long_data, .x)
  })
  arg_list$make.row.names <- FALSE
  structure(
    do.call(rbind, arg_list),
    call = call
  )
}

# nolint start
# source example
# create grouped data object
# dat     <- groupedData(yij ~ time | pid, data = dat)
# lm.list <- lmList(yij ~ time | pid, data = dat)
# model   <- lme(yij ~ log(time), random = ~ log(time) | pid,
#                correlation = corAR1(form = ~ 1 | pid), data = dat)
# nolint end
