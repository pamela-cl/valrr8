
# Validaciones ------------------------------------------------------------


#' Validacion longitud de caracteres para datos generales
#'
#' @param data se necesita una lista con data frames como elementos y el catalogo names_cat_dg
#'
#'
#' @import dplyr
#' @import purrr
#' @import tibble
#'
val_nchar <- function(data) {
  data %>%
    tibble::enframe() %>%
    dplyr::mutate(val_nchar = purrr::map2(
      .x = value,
      .y = name,
      .f = ~
        apply(.x,
              2,
              function(y) max(nchar(y))) %>%
        tibble::enframe("clean_campo",
                "tam_archivos") %>%
        # Ahora pegamos los valores del catalogo oficial
        dplyr::left_join(
          names_cat_dg %>%
            dplyr::filter(riesgo == .y) %>%
            dplyr::select(clean_campo,tamano),
          by = "clean_campo"
        ) %>%
        dplyr::mutate(flag = case_when(
          tam_archivos > tamano ~ FALSE,
          is.na(tam_archivos) ~ TRUE,
          TRUE ~ TRUE
          )
        )
    )) %>%
    select(name, val_nchar)
}
