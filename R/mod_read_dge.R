
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
              ),
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

      tables <- reactiveValues(
        graphs = 0
      )

      # Empiezan las validaciones

      observeEvent(
        dge_table,
        ignoreNULL = T,
        handlerExpr = {

          dge_table_val <- dge_table


        browser()



          prueba <- dge_table_val %>%
            tibble::enframe() %>%
            mutate(type =
                     purrr::map2(
                       .x = value,
                       .y = name,
                       .f = ~ sapply(.x,class) %>%
                         tibble::enframe("clean_campo","tipo_col") %>%
                         dplyr::left_join(
                           names_cat_dg %>%
                             filter(riesgo == .y) %>%
                             select(clean_campo,tipo_r)
                         ) %>%
                         mutate(flag = tipo_col == tipo_r)

                     ))

          graphs <- purrr::map2(
            .x = prueba$type,
            .y = prueba$name,
            .f = ~ .x %>%
              count(flag) %>%
              graph_error(.y)
          )

          tables$graphs <- graphs



        }
      )

      output$output_text <- renderText(
        print("lib")
      )

      # Graficas formato de columnas

      output$plot_dge_inc <- renderPlot({
        tables$graphs[1]
      })

      output$plot_dge_rhi <- renderPlot({
        tables$graphs[2]
      })

      output$plot_dge_tev <- renderPlot({
        tables$graphs[3]
      })
    }
  )
}

