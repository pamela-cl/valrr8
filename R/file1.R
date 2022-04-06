



mod_read_files_dg_server <- function (){

# llamo el catalogo con los tipos de columna y nombres
 names_cat_dg <- read_rds("ins/cat_cnsf_dg_base.rds")

tablas <- list.files("data-raw/", full.names = T) %>%
    str_subset("dg|DG|DGE")


  checks <- tablas %>%
    purrr::map2(
      .y = str_subset(tablas, "dg|DG|DGE") %>%
        str_sub(-7,-5) %>%
        str_to_lower(),
      .f = ~ read_delim(
        .x,
        delim = "|"
      ) %>%
        setNames(names_cat_dg %>%
                   filter(riesgo == "inc") %>%
                   pull(clean_campo)) %>%
        as_tibble()
    )

  checks[1]
  names_cat_dg %>%
    filter(riesgo == "inc") %>%
    select(clean_campo,tipo) %>%


  map(
   .x = checks[],
   .f = ~ class(.x)
  )


}


git config --global user.name 'pamela-cl'
git config --global user.email 'pamelacardenas1902@gmail.com'


mod_dg_cnsf_server <- function (data){

}
