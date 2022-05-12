
# Apariencia de subida de archivos ----------------------------------------

mod_dge_read_files_ui <- function (id){
  ns <- NS(id)

  shinydashboard::tabItem(tabName = "dge",
                          h2("Validaciones Datos Generales"),
                          fluidPage(
                            fluidRow(
                              tabBox(
                                width = 12,
                                fluidRow(
                                  box(
                                    width = 12,
                                    status = "warning",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    title = "Validaciones de formato",
                                    column(
                                      12,
                                      plotOutput(ns("plot_dge")),
                                      br(),
                                      actionButton(ns("detalles"),"Detalles"),
                                      br(),
                                      conditionalPanel( condition = "input$detalles %% 2 == 1",
                                                        br(),
                                                        DT::DTOutput(ns("detalles"))
                                                        )

                                    )
                                  )
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
#' @import tidyr
#' @import dplyr
#' @export
#'
#' @examples
mod_dge_read_files_server <- function (id, boton, dge_table){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      tables <- reactiveValues(
        graphs = 0,
        datos = 0,
        detalles = NULL
      )

      # Empiezan las validaciones

      observeEvent(
        boton(),
        ignoreNULL = T,
        handlerExpr = {


          # llamamos la tabla de datos generales

          dge_table_val <- dge_table()

          # vamos a comprobar que las columnas tengan el formato correcto

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
                             dplyr::filter(riesgo == .y) %>%
                             dplyr::select(clean_campo,tipo_r)
                         ) %>%
                         dplyr::mutate(flag = tipo_col == tipo_r)

                     ))
          print("Se corrieron validaciones de formato")

          # Hacemos una tabla con los errores

          errores_dge <- prueba %>%
            select(-value) %>%
            tidyr::unnest(type) %>%
            group_by(name,flag) %>%
            count()

          # hacemos la gráfica
          print("se hace gráfica")
          errores_dge <- graph_error(errores_dge)


          # guardamos para el plot output
          tables$graphs <- errores_dge
          tables$datos <- prueba

        }
      )

      observeEvent(
        input$detalles,
        ignoreNULL = T,
        handlerExpr = {
          if(input$detalles %% 2 == 1){
          detalles <- tables$datos %>%
            select(-value) %>%
            tidyr::unnest(type) %>%
            mutate(
              name = recode(name,
                            "inc" = "Incendio",
                            "rhi" = "Riesgos Hidro",
                            "tev" = "Terremoto")
            )
          colnames(detalles) <- c("Riesgo","Columna","Formato","Formato_Cat","Igual")

          tables$detalles <- detalles
          }else{
            tables$detalles <- NULL
          }

        }
      )



      # Graficas formato de columnas

      output$plot_dge <- renderPlot({
        tables$graphs
      })

      output$detalles <- DT::renderDT(
        tables$detalles,
        # column filter on the top
        extensions = 'Buttons',
        escape = FALSE,
        selection = 'multiple',
        server = FALSE,
        options = list(
          scrollX=TRUE,
          scrollCollapse=TRUE
        ),
        filter = 'top',rownames= FALSE
      )
      outputOptions(output, "detalles", suspendWhenHidden = FALSE)


    }
  )
}

