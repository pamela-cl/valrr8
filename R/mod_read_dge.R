
# Apariencia de subida de archivos ----------------------------------------

mod_dge_read_files_ui <- function (id){
  ns <- NS(id)

  tabItem(tabName = "dge",
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

mod_dge_read_files_server <- function (id){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

    }
  )
}

