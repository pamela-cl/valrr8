#' GGplot formato erroneo
#'
#' @param data tabla con el numero de columnas correctas e incorrectas
#' @param titles ramo al que pertenece
#'
#' @import ggplot2
#' @import dplyr
#'
graph_error <- function(data){
  data %>%
    mutate(
      name = recode(name,
                    "inc" = "Incendio",
                    "tev" = "Terremoto y Erupción Volcanica",
                    "rhi" = "Riesgos Hidrometereológicos"
      ),
      flag = as.character(flag),
      flag = recode(flag,
                    "TRUE" = "Correctos",
                    "FALSE" = "Incorrectos"
      )
    ) %>%
    ggplot(aes(flag, n, fill = name)) +
    geom_col() +
    geom_text(aes(label = n), vjust = 1) +
    facet_wrap("name") +
    xlab("") +
    ylab("Número de incorrectos") +
    theme(
      plot.background = element_rect(fill = "#F4F5E7"),
      legend.position = "none",
      panel.grid = element_blank(),
    ) +
    scale_fill_manual(values = c("#ff3300", "#0099ff", "#cc9900"))
}


#' Tema de la app.
#'
#' Descripción de colores, fuentes y tamaños.
#'
#' @import dashboardthemes
#'
customTheme_teal <- function()
{
  shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "#2D2D2D"
  ,primaryFontColor = "#0F0F0F"
  ,infoFontColor = "#0F0F0F"
  ,successFontColor = "#0F0F0F"
  ,warningFontColor = "#0F0F0F"
  ,dangerFontColor = "#0F0F0F"
  ,bodyBackColor = "#FEFBD8"

  ### header
  ,logoBackColor = dashboardthemes::cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#256b66"
    ,colorMiddle = "#256B66"
    ,colorEnd = "#41beb6"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )

  ,headerButtonBackColor = dashboardthemes::cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#256b66"
    ,colorMiddle = "#256B66"
    ,colorEnd = "#41beb6"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,headerButtonIconColor = "#DCDCDC"
  ,headerButtonBackColorHover = "#1D7874"
  ,headerButtonIconColorHover = "#256B66"

  ,headerBackColor = dashboardthemes::cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#256b66"
    ,colorMiddle = "#256B66"
    ,colorEnd = "#41beb6"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "#1D7874"
  ,sidebarPadding = "0"

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = "0"
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "#737373"

  ,sidebarSearchBackColor = "#fffefb"
  ,sidebarSearchIconColor = "#646464"
  ,sidebarSearchBorderColor = "#DCDCDC"

  ,sidebarTabTextColor = "#101212"
  ,sidebarTabTextSize = "14"
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = "0"

  ,sidebarTabBackColorSelected = "#6DAFA1"
  ,sidebarTabTextColorSelected = "#000000"
  ,sidebarTabRadiusSelected = "0px"

  ,sidebarTabBackColorHover = "#377E71"
  ,sidebarTabTextColorHover = "#000000"
  ,sidebarTabBorderStyleHover = "none solid none none"
  ,sidebarTabBorderColorHover = "#C8C8C8"
  ,sidebarTabBorderWidthHover = "4"
  ,sidebarTabRadiusHover = "0px"

  ### boxes
  ,boxBackColor = "#fffefb"
  ,boxBorderRadius = "5"
  ,boxShadowSize = "none"
  ,boxShadowColor = ""
  ,boxTitleSize = "18"
  ,boxDefaultColor = "#256B66"
  ,boxPrimaryColor = "#20639B"
  ,boxInfoColor = "#3CAEA3"
  ,boxSuccessColor = "#70AD47"
  ,boxWarningColor = "#F6D55C"
  ,boxDangerColor = "#ED553B"

  ,tabBoxTabColor = "#FEFBD8"
  ,tabBoxTabTextSize = "14"
  ,tabBoxTabTextColor = "#646464"
  ,tabBoxTabTextColorSelected = "#2D2D2D"
  ,tabBoxBackColor = "#FEFBD8"
  ,tabBoxHighlightColor = "#256B66"
  ,tabBoxBorderRadius = "5"

  ### inputs
  ,buttonBackColor = "#1D7874"
  ,buttonTextColor = "#2D2D2D"
  ,buttonBorderColor = "#FAF5F5"
  ,buttonBorderRadius = "5"

  ,buttonBackColorHover = "#fffefb"
  ,buttonTextColorHover = "#000000"
  ,buttonBorderColorHover = "#1D7874"

  ,textboxBackColor = "#FFFFFF"
  ,textboxBorderColor = "#1D7874"
  ,textboxBorderRadius = "5"
  ,textboxBackColorSelect = "#F5F5F5"
  ,textboxBorderColorSelect = "#1D7874"

  ### tables
  ,tableBackColor = "#FFF5F9"
  ,tableBorderColor = "#FFEEDB"
  ,tableBorderTopSize = "1"
  ,tableBorderRowSize = "1"
)
}

#' Poner el nombre del ramo a cada elemento de la lista
#'
#' @param data
#'
#' @import dplyr
#' @import stats
#'
list_names <- function(data) {
  data %>%
  dplyr::pull(data) %>%
    stats::setNames(data$ramo)
}
