#' Filtrar las tablas por emi, sin y datos generales
#'
#' @param tabla
#' @param pattern
#'
#' @import dplyr
#' @import readr
#' @import stringr
#' @import purrr
#'
#' @examples
filtrar_tabla <- function(tabla, pattern){
  to_return <- tabla %>%
    dplyr::filter(stringr::str_detect(.$name,{{pattern}})) %>%
    # extraigo el nombre del ramo
    dplyr::mutate(ramo = str_to_lower(str_sub(name,-7,-5))) %>%
    # creo una nueva columna con la informacion de las tablas
    dplyr::mutate(data = purrr::map(
      .x = datapath,
      .f = ~ readr::read_delim(
        .x,
        delim = "|"
      ) %>%
        dplyr::select(c(1:(length(.)-1)))
    )
    ) %>%
    # Nombro las listas y las columnas
    dplyr::mutate(
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

  print(paste("--archivos leídos",pattern))

  return(to_return)
}
