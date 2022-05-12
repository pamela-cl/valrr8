#List of libraries

# options(shiny.maxRequestSize=50*1024^2)# Can upload until 50mb
library(shiny)
rm(list=ls())
library(tippy)
library(rstudioapi)
library(shinydashboard)
library(shinyWidgets)
library(shinyFiles)
library(readxl)
library(janitor)
library(stringr)
library(tidyverse)
library(scales)
library(purrr)
library(dplyr)
library(DT)
library(shinyalert)
library(excelR)
library(lubridate)
library(shinyjs)
library(reactable)
library(shinyBS)

#List of catalogues in the cloud

# llamo el catalogo con los tipos de columna y nombres
names_cat_dg <-
  dplyr::mutate(
    .data = readr::read_rds("inst/catalogos/cat_cnsf_dg_base.rds"),
    tipo_r  = case_when(
      tipo == "Caracter" ~ "character",
      tipo %in% c("Numérico","Numerico") ~ "numeric",
      tipo == "Fecha" ~ "date",
      TRUE ~ "checar"
    )
  )
