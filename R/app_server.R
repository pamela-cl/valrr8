#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # leer los archivos
  mod_read_files_server("archivos")

  #  DGE crear tablas y validaciones de estructura
  mod_dge_read_files_server("dge_val_cnsf")

}

