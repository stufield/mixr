#' Create Simple lme Models
#'
#' Builds a series linear mixed-effects models of the form `Response ~
#' Fixed Covariate | Grouping`. A single model or a set of models is built
#' for each response in `responses`. The input data is stored along
#' with names of the fixed covariate and grouping variables.
#'
#' @param data Either a data frame or a *named* list of
#'   `data.frame` objects. If a data frame is provided then a single lme model is
#'   built for each response in `responses`. If a named list is provided
#'   then a distinct model is built for each `data.frame x response` combination.
#' @param response Character. A vector with the names of the response
#'   variables.
#' @param fixed Character. The name of the fixed covariate variable
#'   (e.g. `TimePoint`), typically the slope component of the model.
#' @param grouping Character. The name of the variable indicating the groups
#'   (e.g. `pid` or `subjectID`).
#' @param random.slope Logical. Indicating whether subject specific slopes
#'   should be fit. Alternatively, subject specific offsets (intercept) are fit.
#' @param ... Additional arguments passed to [fit_lme_safely()]
#'   or is via the S3 plot method, passed to
#'   \code{graphics::\link[graphics]{plot}}.
#' @return A list with an element for each response in `responses` as well
#'   as the fixed and grouping variables. If `data` is a data.frame or a
#'   groupedData object then each of the response elements will be an lme model.
#'   Otherwise if a list was provided for the `data` argument then the
#'   response elements will be a list with an lme model for each data set in the
#'   `data` list.
#' @author Michael R. Mehan
#' @seealso [fit_lme_safely()]
#' @examples
#' long  <- simulateLongData(r.seed = 101, beta1 = 50)
#' model <- simpleLMEs(long, "yij", "time", "pid")
#' model   # S3 print method
#'
#' # random offset
#' model2 <- simpleLMEs(long, "yij", "time", "pid", random.slope = FALSE)
#'
#' # 2 data set option
#' long2  <- simulateLongData(r.seed = 405, beta1 = 10)     # smaller beta1
#' model3 <- simpleLMEs(list(A = long, B = long2), "yij", "time", "pid")
#'
#' # S3 print method
#' model3
#'
#' @importFrom stats setNames
#' @importFrom SomaDataIO addClass
#' @export
simpleLMEs <- function(data, response, fixed, grouping, random.slope = TRUE, ...) {

  stopifnot(inherits(data, c("data.frame", "list")))

  if ( inherits(data, "list") && is.null(names(data)) ) {
    stop(
      "If `data =` argument is a list of data frames, it *must* be named.",
      call. = FALSE
    )
  }

  compare_data <- (inherits(data, "list") && length(data) == 2L)
  fixed_form   <- as.formula(sprintf("%s ~ %s", response, fixed))
  random_form  <- as.formula(sprintf("~ %s | %s",
                                     ifelse(random.slope, fixed, 1),
                                     grouping))

  if ( compare_data ) {
    fit <- lapply(setNames(data, names(data)), function(i) {
      modl <- fit_lme_safely(formula = fixed_form,
                             random = random_form,
                             data = i, ...)
      modl$call$fixed <- call("as.formula",
                              sprintf("%s ~ %s", response, fixed))
      modl
    })
  } else {
    fit <- fit_lme_safely(formula = fixed_form,
                          random = random_form,
                          data = data, ...)
    fit$call$fixed <- call("as.formula", sprintf("%s ~ %s", response, fixed))
  }

  ret <- list()
  ret[[response]]  <- fit
  ret$data         <- data
  ret$compare_data <- compare_data
  ret$response     <- response
  ret$fixed        <- fixed
  ret$random       <- random_form
  ret$grouping     <- grouping
  addClass(ret, "simpleLMEs")
}


# nocov start

# Plots a Random Effect from a simpleLME
#' @describeIn simpleLMEs
#' The S3 method plots the points and fit for each of the
#' random effects in a [simpleLMEs()] model object.
#' @param x An object of class [simpleLMEs()].
#' @param col The color of the lines. See [ggplot2::geom_smooth()].
#' @param pt.col The color of the points. See [ggplot2::geom_point()].
#' @param groups Numeric. Indices indicating the *optional* subset of
#' groups (e.g. subjects) to be plotted for the random effects.
#' @author Michael R. Mehan, Stu Field
#' @examples
#' # S3 plot method
#' plot(model)
#'
#' # plot only certain subjects/groups
#' plot(model, groups = seq(1, 11, by = 2), pt.col = "purple")
#' @importFrom stats predict
#' @importFrom rlang sym
#' @export
plot.simpleLMEs <- function(x, col = "blue", pt.col = "black",
                            groups = NULL, ...) {

  if ( x$compare_data ) {
    stop(
      "Not appropriate to plot 2 dataset comparsion objects ... ",
      "data: ", value(names(x$data)), call. = FALSE
    )
  }

  if ( is.null(x$response) ) {
    signal_oops("Unable to get response ...")
  }

  data     <- x$data
  grouping <- rlang::sym(x$grouping)
  response <- rlang::sym(x$response)
  fixed    <- rlang::sym(x$fixed)

  if ( !is.null(groups) ) {
    data <- dplyr::filter(data, !! rlang::sym(grouping) %in% groups)
  }

  p <- data |>
    ggplot2::ggplot(ggplot2::aes(x = !! fixed, y = !! response)) +
    ggplot2::geom_point(alpha = 0.5, colour = pt.col, size = 3) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = col) +
    ggplot2::facet_wrap(ggplot2::vars(!!grouping))

  p
}


# Print method for simpleLME
#' @describeIn simpleLMEs
#' The S3 method print method for `simpleLMEs` model object.
#' @export
print.simpleLMEs <- function(x, ...) {

  getN <- function(x) {
    dd <- x$dims
    list(Nobs = dd$N, N_Grps = unname(dd$ngrps[1:dd$Q]))
  }

  respY <- x$response

  if ( x$compare_data ) {
    writeLines(
      signal_rule("Simple LMEs ... 2 data set model comparison",
                  line_col = "blue", lty = "double")
    )
    nm <- names(x$data)
    n  <- lapply(x[[respY]], getN)
    comp <- paste(nm, collapse = " vs. ")
    print(do.call(rbind, lapply(x[[respY]], function(.x) .x$coefficients$fixed)))
    cat("\n")
    key <- c("Compared Data",
             "Fixed effects",
             "Random effects",
             "No. Observations",
             "No. Groups") |> pad(17)
    obs <- liter(n, .f = function(.x, .y) sprintf("%s = %i", .y, .x$Nobs))
    grps <- liter(n, .f = function(.x, .y) sprintf("%s = %i", .y, .x$N_Grps))
    value <- c(
      add_style$red(comp),
      add_style$green(paste(respY, "~", x$fixed)),
      add_style$cyan(deparse(x$random)),
      value(unlist(obs)),
      value(unlist(grps))
    )
    writeLines(
      paste0(key, "    ", value)
    )
  } else {
    writeLines(
      signal_rule("Simple LMEs standard model", line_col = "blue", lty = "double")
    )
    .n     <- getN(x[[respY]])
    smry   <- summary(x[[respY]])
    values <- c(
      "LME fit method" = smry$method,
      "Fixed effects"  = deparse(smry$terms),
      "Random effects" = deparse(x$random),
      round(smry$coefficients$fixed, 2),   # already named
      unlist(.n),                          # already named
      AIC              = round(smry$AIC, 2),
      BIC              = round(smry$BIC, 2)
    )
    writeLines(paste(" ", pad(names(values), 22), values))
    writeLines(signal_rule("Summary t-table", line_col = "blue"))
    print(smry$tTable)
  }
  writeLines(signal_rule(line_col = "green", lty = "double"))
  invisible(x)
}
# nocov end
