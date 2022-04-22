
# Apariencia de subida de archivos ----------------------------------------

mod_read_files_ui <- function (id){
  ns <- NS(id)

  tabItem(tabName = "upload_files",
          h2("Sube tus archivos"),
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
                  title = "Instrucciones",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
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
                  <b> * opción recomendada</b>")
                ),
                box(
                  width = 12,
                  status = "warning", solidHeader = TRUE, collapsible = TRUE,
                  fileInput(ns("files"),
                            "Selecciona tus archivos",
                            multiple = T,
                            accept = ".csv"
                  ),
                  textOutput(ns("contents"))
                )
              )
            )
          )
  )
}


# subir y leer archivos ---------------------------------------------------

mod_read_files_server <- function (id){
  moduleServer(
    id,
    ## Below is the module function
    function(input, output, session) {

      observeEvent(
        input$files,
        handlerExpr = {

          library(dplyr)
          library(stringr)

          # llamo el catalogo con los tipos de columna y nombres
          names_cat_dg <-
            readr::read_rds("inst/catalogos/cat_cnsf_dg_base.rds")

          # tabla con los nombres de archivos y rutas solo para dge
          tablas_dg <- input$files %>%
            as_tibble() %>%
            filter(str_detect(.$name,"dg|DG|DGE|Dge|dge"))

          # extraigo el nombre y año del ejercicio
          tablas_dg <- tablas_dg %>%
            mutate(ramo = str_to_lower(str_sub(name,-7,-5))) %>%
            # creo una nueva columna con la informacion de las tablas
            mutate(data = purrr::map(
              .x = datapath,
              .f = ~ read_delim(
                .x,
                delim = "|"
              ) %>%
                select(c(1:(length(.)-1)))
            )
            )

          print("tope1__archivos leídos")

          # Ponemos los nombres adecuados a las tablas
          tablas_dg <- tablas_dg %>%
            mutate(
              data = purrr::map2(
                .id = NULL,
                .x = data,
                .y = ramo,
                .f = ~ setNames(.x,
                                names_cat_dg %>%
                                  filter(riesgo == .y) %>%
                                  pull(clean_campo)
                )
              )
            )

          # quitamos el sufijo creado por el map
          for(i in c(1:3)){
          colnames(tablas_dg$data[[i]]) <- gsub("\\_[0-9]","",
                                                colnames(tablas_dg$data[[i]]))
          }

          tabla_gen_dge <- tablas_dg %>% pull(data)
          setNames(tabla_gen_dge, tablas_dg$ramo)
          rm(tablas_dg)

          print("tope2__comienzan validaciones")

          browser()

        }
      )

      # Imprimo tabla de resultados, validar que sean txt

      output$contents <- renderText({

        file <- input$files
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need( sum(str_detect(ext,"txt")) == length(ext),
                       "Please upload only txt files")
        )
      })
    }
  )
}

