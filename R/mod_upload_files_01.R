
# Apariencia de subida de archivos ----------------------------------------

#' Title
#'
#' @param id
#'
#' @return
#' @import shinydashboard
#' @import shiny
#'
mod_upload_files_ui <- function (id){
  ns <- NS(id)

  tabItem(tabName = "upload_files",
          h2("Como subir tus archivos"),
          tags$head(
            tags$style(HTML("
              .shiny-output-error-validation {
                color: red;
              }
            "))
          ),
          fluidPage(
            fluidRow(
              column(
                12,
                "",
                box(
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = T,
                  HTML("<p>Sube los archivos que quieras validar que pertenezcan a los ramos de Incendio, Terremoto o Riesgos hidrometeorológicos.</p>
                  <p> En orden de correr las funciones los nombres de archivos deben cumplir con las siguientes reglas : <br>

                  <ul class='roman'>
                   <li>Contener en el nombre del archivo (DGE, EMI, SIN) haciendo referencia a:
                   <ul class='square'>
                    <li>DGE - Datos Generales.</li>
                    <li>EMI - Emisión.</li>
                    <li>SIN - Siniestros.</li>
                   </ul>
                   </li>
                   <li>Contener el año del ejercicio con dos digitos (ej. 09, 10, 18, 19, 22).</li>
                   <li>Contener una referencia al ramo que corresponde (ej. INC, Inc, RHI, rhi, TEV, tev, Tev).</li>
                   <li>El tipo de archivos deberá estar al final del nombre y deberá tener 3 digitos (dge, DGE, emi, EMI, sin, SIN).</li>
                  </ul>

                  <p>Ejemplos: </p>
                  <ul>
                    <li>dge_22_Inc</li>
                    <li>EMI_18_sin</li>
                    <li>dge_22_inc</li>
                    <li>SIN_21_TEV *</li>
                  </ul>
                  <b> * opción recomendada</b>"))),
              column(
                width = 12,
                box(
                  width = 12,
                  title = "Selecciona tus archivos",
                  background = "teal",
                  collapsible = TRUE,
                  fileInput(ns("files"),
                            "",
                            multiple = T,
                            accept = ".csv"
                  ),
                  textOutput(ns("contents")))
              )
            )
          )
  )
}


# subir y leer archivos ---------------------------------------------------

#' Title
#'
#' @param id
#'
#' @return
#' @import dplyr
#'
mod_upload_files_server <- function (id){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      tables <- reactiveValues(
        dge_table = NULL,
        emi_table = NULL,
        sin_table = NULL,
        empieza = 0,
        graphs = 0
      )

      observeEvent(
        input$files,
        ignoreNULL = T,
        handlerExpr = {

          library(dplyr)
          library(stringr)

          tables$empieza <- 0

          file <- input$files
          ext <- tools::file_ext(file$datapath)

          if( sum(str_detect(ext,"txt")) == length(ext)){
            tables$empieza <- 1
          }else{
            tables$empieza <- 0
          }

          if(tables$empieza == 1){

            # Enseñar el progreso al usuario
            withProgress({

              # tabla con los nombres de archivos y rutas
              tablas <- input$files %>%
                tibble::as_tibble()

              print("Formamos tablas")

              # Hacemos una tabla para cada tipo de archivo


              tablas_dge <- filtrar_tabla(tablas,"dg|DG|DGE|Dge|dge")
              tablas_emi <- filtrar_tabla(tablas,"emi|EMI|Emi|emisión|emision")
              tablas_sin <- filtrar_tabla(tablas,"sin|SIN|Sin|siniestros|Siniestros")

              # # quitamos el sufijo creado por el map
              #
              # for(i in c(1:3)){
              #   colnames(tablas_dge_fun$data[[i]]) <- gsub("\\_[0-9]","",
              #                                              colnames(tablas_dge_fun$data[[i]]))
              # }

              tabla_gen_dge <- list_names(tablas_dge)
              tabla_gen_emi <- list_names(tablas_emi)
              tabla_gen_sin <- list_names(tablas_sin)

              tables$dge_table <- tabla_gen_dge
              tables$emi_table <- tabla_gen_emi
              tables$sin_table <- tabla_gen_sin

            },
            message = "Creando tablas de DGE, EMI y SIN")


          }else{

            shinyalert::shinyalert("Oops!", "Archivos con formato incorrecto", type = "error")
            # showNotification("Archivos con formato incorrecto.",
            #                  type = "error")
          }
        }
      )




      # Imprimo tabla de resultados, validar que sean txt

      output$contents <- renderText({

        file <- input$files
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need( sum(str_detect(ext,"txt")) == length(ext),
                       "Seleciona solo archivos en formato txt")
        )

      })

      return(
        list(
          dge_tabla = reactive({tables$dge_table}),
          emi_tabla = reactive({tables$emi_table}),
          sin_tabla = reactive({tables$sin_table}),
          boton_archivos = reactive({input$files}),
          inc_val = reactive({input$inc_val}),
          rhi_val = reactive({input$rhi_val}),
          tev_val = reactive({input$tev_val})
        )
      )

    }
  )
}

