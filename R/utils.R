# this wrapper is necessary following the deprecation of
# purrr::cross_df(). Recommended to use tidyr::expand_grid()
# but that function is slow and re-orders the rows by variable
# relative to either expand.grid() or cross_df()
# this is for internal use only
.expand_grid <- function(...) {
  out <- expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  as_tibble(out)
}

get_analytes <- getFromNamespace("get_analytes", "helpr")
