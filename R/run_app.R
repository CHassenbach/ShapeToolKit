#' Deprecated launcher: use run_haug_app()
#' @noRd
run_app <- function(...) {
  stop("run_app() is deprecated. Please use: source('R/run_haug_app.R'); run_haug_app(...)", call. = FALSE)
}