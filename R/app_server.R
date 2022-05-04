#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  r <- reactiveValues(
    from_dge_upload_files = NULL
  )

  # leer los archivos

  r$from_dge_upload_files <- mod_upload_files_server("archivos")

  #  DGE Crear tablas y validaciones de estructura

  mod_dge_read_files_server("dge_val_cnsf", r$from_dge_upload_files)



}

