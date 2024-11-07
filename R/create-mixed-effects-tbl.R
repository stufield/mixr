#' Generates a table of mixed effects
#'   models from a list of models generated
#'   by a call to [fit_mixed_effects_models()].
#'
#' The ordering of the table is determined by the fixed effects
#'   argument passed to [fit_mixed_effects_models()]. Generally,
#'   if there is an interaction term, that p-value is used, otherwise
#'   the other fixed effects, usually `TimePoint`.
#'
#' @order 2
#' @rdname fit_mixed_effects_models
#' @param x A list of mixed-effects models calculated
#'   using [fit_mixed_effects_models()]. Alternatively, an object of
#'   class `"mixed_effects_table"` for the S3 print method.
#'
#' @return A list object of class `mixed_effects_table` with the
#' following components:
#' \item{stat_table}{A data frame (table) of the results:
#' \describe{
#'   \item{Time effect:}{Both F-values and p-values for TimePoint}
#'   \item{Group effect:}{Both F-values and p-values for Group}
#'   \item{p_value:}{Unadjusted p-values for the KS-distances}
#'   \item{fdr:}{FDR adjustment to p-values}
#'   \item{p_bonferroni:}{Bonferroni corrected p-value}
#'   \item{rank:}{Ranking by the test significance.}
#' }}
#' \item{test}{The name of the model fit, "Linear Mixed-effects Model".}
#' \item{call}{The direct call made the [fit_mixed_effects_models()].}
#' \item{data_dim}{The dimensions of the data frame.}
#' \item{method}{The fitting method used by `lme`.}
#' \item{fixed}{The fixed-effects.}
#' \item{observations}{The number of total observations.}
#' \item{PIDfield}{The field name identifying the subjects in the data frame.}
#' \item{subjects}{The number of subjects.}
#'
#' @author Stu Field
#'
#' @examples
#' class(models)
#'
#' lme_tab <- create_mixed_effects_tbl(models)
#' @importFrom tibble as_tibble deframe
#' @importFrom tidyr pivot_longer unite
#' @importFrom stats p.adjust anova runif setNames
#' @importFrom dplyr sym mutate arrange row_number select everything
#' @importFrom dplyr ends_with starts_with filter row_number bind_rows
#' @export
create_mixed_effects_tbl <- function(x) {

  stopifnot(
    inherits(x, "mixr_lme"), # our own object `fit_mixed_effects_models()`
    !is.null(names(x))       # must be a named list
  )

  if ( !"fixed" %in% names(attributes(x)) ) {
    stop(
      "Fixed effects string missing. ",
      "Was the model list created using `fit_mixed_effects_models()`?",
      call. = FALSE
    )
  }

  stab <- lapply(x, function(.x) {
    aov_tab <- stats::anova(.x) |>
      data.frame() |>
      dplyr::select(-ends_with("DF")) |>
      as_tibble(rownames = "metric") |>
      dplyr::filter(row_number() != 1)  # 1st row is `Intercept`
    # don't want `Parameter` name
    pivot_longer(aov_tab, -metric) |>
      unite(col = "name", -value) |>
      deframe() |> as.list()  |> data.frame() |>
      dplyr::select(starts_with("Response_"),
                    starts_with("TimePoint_"),
                    everything()) |>   # reorder to group F/p-vals
      cbind(converged = .x$converged)  # add converged bool
  }) |>
    dplyr::bind_rows(.id = "Feature")

  p_col <- gsub("\\*", ".", attr(x, "fixed")) |> # replace '*' with '.'
    paste0("_p.value$") |>                  # paste to end with p-value
    grep(names(stab), value = TRUE) |>      # search `stab` for a match
    dplyr::sym()                            # convert to symbol for !! below

  stab <- dplyr::mutate(stab,
                        fdr          = p.adjust(!!p_col, method = "fdr"), # nolint
                        p.bonferroni = p.adjust(!!p_col, method = "bonferroni")
  ) |>
    dplyr::arrange(!!p_col) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    col2rn("Feature")

  ret_list              <- list()
  ret_list$stat_table   <- stab
  ret_list$test         <- "Linear Mixed-effects Model"
  ret_list$call         <- x[[1L]]$call
  ret_list$method       <- x[[1L]]$method
  ret_list$fixed        <- attributes(x)$fixed
  ret_list$random       <- attributes(x)$random
  ret_list$log          <- attributes(x)$log
  ret_list$nModels      <- length(x)
  ret_list$data_dim     <- attributes(x)$data.dim
  dims                  <- x[[1L]]$dims
  ret_list$observations <- dims$N
  pids                  <- dims$ngrps[dims$Q[1L]]
  ret_list$PIDfield     <- attributes(x)$PIDfield
  ret_list$subjects     <- unname(pids)
  if ( withr::with_preserve_seed(runif(1) < 0.25) ) give_praise()
  ret_list |>
    add_class(c("stat_table", "mixed_effects_table"))
}

#' @order 3
#' @describeIn fit_mixed_effects_models
#'   S3 print method for `mixed_effects_table` class.
#'
#' @param n `integer(1)`The number of rows to
#'   show in the S3 print method.
#' @param ... Additional arguments as required
#'   by default S3 print methods.
#'
#' @examples
#' # S3 print method
#' lme_tab
#'
#' @export
print.mixed_effects_table <- function(x, n = 6L, ...) {
  left <- c("Fixed effects",
            "Random effects",
            "Number of models",
            "Subject field",
            "Number of subjects",
            "Number of observations") |> pad(25)
  right <- c(x$fixed, x$random, x$nModels, x$PIDfield, x$subjects, x$observations)
  writeLines(paste(" ", left, right))
  cat("\n")
  signal_rule("Stat Table", line_col = "blue")
  print(utils::head(x$stat.table, n))
  invisible(x)
}

