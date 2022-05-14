#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import dashboardthemes
#' @noRd
app_ui <- function(request) {

read_global_file()


  dashboardPage(
    dashboardHeader(
      title = dashboardthemes::shinyDashboardLogo(
        theme = "blue_gradient",
        boldText = "Validaciones RR8",
        mainText = "",
        badgeText = "App"

      ),
      # Dropdown menu for messages
      dropdownMenu(type = "message", badgeStatus = "info",
                                   messageItem("git repository",
                               HTML("You can find the repository of this project here"),
                               icon = icon("fab fa-github")
                   ),
                   icon = icon("fas fa-info-circle")
      )
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Archivos", tabName = "upload_files",
                 icon = icon("fas fa-file-upload")),
        menuItem("Datos Generales", tabName = "dge",
                 icon = icon("fas fa-file-alt")),
        menuItem("Emisión", tabName = "emi",
                 icon = icon("fas fa-money-check-alt")),
        menuItem("Siniestros", tabName = "sin",
                 icon = icon("fas fa-house-damage")),
        actionButton("browser","Browser")
      )
    ),
    dashboardBody(
      # dashboardthemes::shinyDashboardThemes(
      #   theme = "blue_gradient"
      # ),
      costumeTheme(),
      tabItems(
        mod_upload_files_ui("archivos"),
        mod_dge_read_files_ui("dge_val_cnsf"),
        tabItem(tabName = "emi",
                h2("Validaciones Emisión")
        ),
        tabItem(tabName = "sin",
                h2("Validaciones Siniestros")
        )
      )
    )
  )
}
