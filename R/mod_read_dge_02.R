
# Apariencia de subida de archivos ----------------------------------------

mod_dge_read_files_ui <- function (id){
  ns <- NS(id)
tabItem(

  tabName = "dge",
  mainPanel(
    width = 12,
    tabsetPanel(
      tabPanel("General",
               fluidPage(
                 column(
                   12,
                   br(),
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
               ),
      tabPanel("Tabla",
               fluidPage(

                 )
      )
    )
  ) # close mainpanel
) # close tabitem
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
mod_dge_read_files_server <- function (id, boton, dge_table){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      tables <- reactiveValues(
        graphs = 0,
        datos = 0,
        detalles = NULL,
        validaciones = list(
          nchar = NULL
        )
      )

      # Empiezan las validaciones

      observeEvent(
        boton(),
        ignoreNULL = T,
        handlerExpr = {

          # llamamos la tabla de datos generales

          dge_table_val <- dge_table()


          # validación longitud de caracteres ---------------------------------------

          # vamos a comprobar que las columnas tengan el formato correcto

          print("Validación longitud de caracteres")
          val_nchar_tabla <- val_nchar(dge_table_val)

          # Hacemos una tabla con los errores

          errores_dge <- val_nchar_tabla %>%
            tidyr::unnest(val_nchar) %>%
            group_by(name,flag) %>%
            count()

          # hacemos la gráfica
          print("se hace gráfica")
          errores_dge <- graph_error(errores_dge)


          # guardamos para el plot output
          tables$graphs <- errores_dge
          tables$datos <- val_nchar_tabla

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

      output$table <- renderReactable({
        reactable(iris)
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

