#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # leer los archivos

  tabla_dge_general <- mod_upload_files_server("archivos")

  #  DGE Crear tablas y validaciones de estructura

  mod_dge_read_files_server("dge_val_cnsf", tabla_dge_general$boton_archivos,
                            tabla_dge_general$dge_tabla)



}

