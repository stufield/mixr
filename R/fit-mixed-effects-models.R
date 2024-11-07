#' Fit Mixed-Effects Models
#'
#' Fit an identical series of mixed-effects models according
#'   for each analyte in an ADAT.
#'
#' By default, random intercept models (subject specific offsets)
#'   are fit with a fixed effect for slope: `random = ~ 1 | group`.
#'   This means that group/subject specific slopes are not fit, and all
#'   fits will be parallel with respect to slope. This may or may not
#'   be what you intend but for most of SomaScan data this is the
#'   desired default.
#'
#' @order 1
#' @param data The `data.frame` containing data as columns.
#' @param fixed `character(1)`. Indicating the right-hand-side of the
#'   formula for the fixed effect.
#' @param random `character(1)`. Indicating the full formula for the random
#'   effect. The default fits subject specific intercepts. Typically "group"
#'   field indicator (e.g. subjects) are to the right side of the "|" and this
#'   field *must* be a column name in `data`.
#' @return A list (invisibly) of fitted linear mixed-effects response models
#'   (`class = lme`) with extra attributes attached for print methods.
#' @seealso [fit_lme_safely()]
#' @examples
#' df <- data.frame(
#'   pid       = 1041:1080,
#'   Pop       = rep(utils::head(LETTERS, 10), 4),
#'   TimePoint = rep(c("baseline", "6 months", "12 months", "24 months"), each = 10),
#'   seq.1234.56 = stats::rnorm(40, mean = 25, sd = 3.5),
#'   seq.6969.4  = stats::rnorm(40, mean = 25, sd = 1.5),
#'   Response  = factor(sample(c("Control", "Disease"), 40, replace = TRUE))
#' )
#' df$TimePoint <- factor(df$TimePoint, levels = c("baseline", "6 months",
#'                                                 "12 months", "24 months"))
#'
#' models <- df[, c("TimePoint", "Response", "Pop", mixr:::get_analytes(df))] |>
#'   fit_mixed_effects_models(fixed = "TimePoint*Response", random = "~ 1 | Pop")
#' lapply(models, summary)
#'
#' @importFrom stats as.formula setNames
#' @export
fit_mixed_effects_models <- function(data, fixed = "TimePoint*SampleGroup",
                                     random = "~ 1 | pid", do.log) {

  fixed <- gsub("[[:space:]]", "", trimws(fixed))  # rm whitespace

  out <- setNames(get_analytes(data), get_analytes(data)) |>
    lapply(function(.apt) {
      signal_done("Fitting ...", value(.apt))
      frm <- as.formula(sprintf("%s ~ %s", .apt, fixed))
      fit_lme_safely(formula = frm, random  = as.formula(random), data = data)
    })
  structure(out,
            class    = c("mixr_lme", "list"),
            fixed    = fixed,
            random   = random,
            PIDfield = gsub("^~.*\\| *", "", random),
            log      = is_logspace(data),
            data.dim = dim(data)) |> invisible()
}
