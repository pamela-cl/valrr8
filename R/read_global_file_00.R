#' Call global file
#'
#' @return
#' @export
#'
read_global_file <- function(){
  source(system.file("read_global", "global.R", package = "valrr8"), encoding = "UTF-8")
}
