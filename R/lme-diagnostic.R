#' Diagnostics of Linear Mixed-Effects Models
#'
#' A diagnostic wrapper for linear mixed-effects models. Generally for
#'   individual models fit at the command line, it produces the individual
#'   subject-specific (group-specific) slopes and intercepts (offset) for serially
#'   dependent (longitudinal) data. Two independent plots are generated:
#'   \describe{
#'     \item{Intervals}{A plot of the 95% confidence interval
#'       for each parameter estimate by subject.}
#'     \item{Boxplots}{Boxplots of the estimated offsets (bottom) and
#'       slopes (top). Optionally, the boxplots cad be split by a grouping
#'       variable either passed as a vector, or included as part of the data.}
#'   }
#'
#' @param model A `lme` model object, typically fit via [fit_lme_safely()].
#' @param data Optionally, if the data used in the lme fit is not
#'   included with the model, this must be passed here.
#' @param group_by Optional. If passed, either a vector indicating the
#'   split in the subjects (i.e. groups) or a character field string
#'   indicating the column in `data` containing the split information.
#' @param ... Additional arguments passed to \code{nlme::\link[nlme]{lmList}}.
#'
#' @return Diagnostic plots (see description) showing the subject specific
#'   linear model coefficients (slope and intercept).
#' @seealso [lmList()], [intervals()], [fit_lme_safely()]
#'
#' @examples
#' p1 <- list(beta1 = 50, r_seed = 101)
#' p2 <- list(beta1 = 10, r_seed = 405)
#' longData  <- createLongData(A = p1, B = p2)
#' fit <- fit_lme_safely(yij ~ time, random = ~1|pid, data = longData)
#' lme_diagnostic(fit, longData)  # all groups/pids together
#'
#' # Group boxplots by sub-group
#' lme_diagnostic(fit, longData, group.by = "Group")  # slope A > B
#'
#' # Group by external random vector (3 levels)
#' group_vec <- withr::with_seed(5, sample(1:3, nrow(longData), replace = TRUE))
#' lme_diagnostic(fit, longData, group_by = group_vec)
#' @importFrom nlme intervals lmList
#' @importFrom stats coefficients as.formula
#' @importFrom tidyr drop_na pivot_longer
#' @export
lme_diagnostic <- function(model, data = NULL, group_by = NULL, ...) {

  fixed  <- gsub("\\*.*$", "", deparse(model$call$fixed)) |> trimws()
  random <- gsub("^~.*\\| *", "", deparse(model$call$random))
  form   <- as.formula(sprintf("%s | %s", fixed, random))

  if ( is.null(model$data) && is.null(data) ) {
    stop(
      "You must pass a `data =` argument containing the ",
      "data used to fit the model.", call. = FALSE
    )
  } else if ( !is.null(model$data) ) {
    data <- model$data
  }

  lm_fits <- nlme::lmList(form, data = data, ...)
  lm_intr <- nlme::intervals(lm_fits)
  p1 <- plot(lm_intr, main = sprintf("Subject Specific Coefficients | %s", fixed))

  if ( !is.null(group_by) ) {
    if ( is.character(group_by) && length(group_by) == 1L && group_by %in% names(data) ) {
      data <- split(data, data[[group_by]])
    } else if ( length(group_by) == nrow(data) ) {
      data <- split(data, group_by)
    }
    lm_fits <- lapply(data, function(.x) nlme::lmList(form, data = .x, ...))
  } else {
    lm_fits <- list(lm_fits)
  }

  id_data <- lapply(lm_fits, stats::coefficients) |>
    bind_rows(.id = "Group") |>
    drop_na(time) |>
    dplyr::rename(Intercept = "(Intercept)") |>
    pivot_longer(c(time, Intercept), names_to = "coef")

  p2 <- id_data |>
    ggplot2::ggplot(ggplot2::aes(x = coef, y = value, fill = Group)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.size = 0, notch = TRUE) +
    SomaPlotr::scale_fill_soma() +
    ggplot2::geom_point(pch = 21, alpha = 0.5, size = 2.5,
                        position = ggplot2::position_jitterdodge(jitter.width = 0.05,
                                                                 seed = 1)) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::coord_flip() +
    SomaPlotr::theme_soma()

  gridExtra::grid.arrange(p1, p2, ncol = 2L)
}
