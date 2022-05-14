
# Validaciones ------------------------------------------------------------


val_nchar <- function(data) {
  data %>%
    tibble::enframe() %>%
    mutate(val_nchar = map(
      .x = value,
      .f = ~
        apply(.x,
              2,
              function(y) max(nchar(y))) %>%
        enframe("clean_campo",
                "tam_archivos") %>%
        # Ahora pegamos los valores del catalogo oficial
        left_join(
          names_cat_dg %>%
            select(clean_campo,tamano),
          by = "clean_campo"
        ) %>%
        mutate(flag = tam_archivos <= tamano )
    )) %>%
    select(name, val_nchar)
}
