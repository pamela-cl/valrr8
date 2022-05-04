#' GGplot formato erroneo
#'
#' @param data tabla con el numero de columnas correctas e incorrectas
#' @param titles ramo al que pertenece
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
graph_error <- function(data, titles){

  titles <- recode(
    titles,
    "inc" = "Incendio",
    "tev" = "Terremoto y Erupción Volcanica",
    "rhi" = "Riesgos Hidrometereológicos"
  )

  data <- data %>%
    mutate(flag  = case_when(
      flag == FALSE ~ "Incorrectas",
      TRUE ~ "Correctos"
    ))

  # Compute percentages
  data$fraction = data$n / sum(data$n)
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax = cumsum(data$fraction)
  # Compute the bottom of each rectangle
  data$ymin = c(0, head(data$ymax, n=-1))
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  # Compute a good label
  data$label <- paste0(data$flag, "\n value: ", data$n)
  # Make the plot
  a <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=flag)) +
    geom_rect() +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
    scale_fill_brewer(
      palette = "Accent"
    ) +
    labs(
      subtitle = paste("Número de columnas con el formato incorrecto"),
      title = titles
    )+
    theme_void()+
    theme(
      title = element_text(colour="black", size=12, face="bold",hjust = 0.5),
      legend.position = "none",
      panel.background = element_blank()
    )

  print(a)

}
