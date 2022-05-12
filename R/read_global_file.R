#' Call global file
#'
#' @return
#' @export
#'
#' @examples
read_global_file <- function(){
  source(system.file("read_global", "global.R", package = "srTemplates"), encoding = "UTF-8")
}
