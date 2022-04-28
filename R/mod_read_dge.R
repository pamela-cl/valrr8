
# Apariencia de subida de archivos ----------------------------------------

mod_dge_read_files_ui <- function (id){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = "dge",
          h2("Validaciones Datos Generales"),
          fluidPage(
            fluidRow(
              column(
                12,
                "",
                box(
                  "Validaciones de estructura",
                  width = 12,
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  textOutput(ns("output_text"))
                )
              )
            )
          )
  )
}


# subir y leer archivos ---------------------------------------------------

#' Modulo para leer las tablas de Datos generales
#'
#' @param id
#' @param dge_table
#'
#' @return
#' @export
#'
#' @examples
mod_dge_read_files_server <- function (id, dge_table){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      observeEvent(
        dge_table,
        ignoreNULL = T,
        handlerExpr = {









        }
      )

      output$output_text <- renderText(
        print("lib")
      )
    }
  )
}

