#' Helper functions
#'
#' @export



## Source .Rmd

source_rmd <- function(file, ...) {
  tmp_file <- tempfile(fileext = ".R")
  on.exit(unlink(tmp_file), add = TRUE)
  knitr::purl(file, output = tmp_file, quiet = T)
  source(file = tmp_file, ...)
}
