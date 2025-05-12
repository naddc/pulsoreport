#' Genera un gráfico de barras horizontales para preguntas de respuesta única, para PowerPoint
#'
#' @importFrom forcats fct_rev
#' @import ggplot2
#' @import ggh4x
#' @import ggtext
#' @import grid
#' @import gridExtra
#' @import patchwork
#' @import ggpubr
#' @import stringr
#' @param data Un data frame con los datos en formato sav.
#' @param var Variable para plotear.
#' @param levels Niveles para ordenar variables escalares. Si es NULL (opción por defecto), toma el orden de los valores de la variable.
#' @param order_freq Define si las barras se ordenarán según frecuencia (si TRUE). FALSE (opción por defecto) permite usar el orden de levels.
#' @param title Recibe un título para el gráfico. Si es NULL (opción por defecto), toma la etiqueta de la variable en var.
#' @param unit Unidad de observación para declarar el N de la base usada para el gráfico. Si es NULL (opción por defecto), toma el parámeto correspondiente al dataset en data ("data_unit") indicado en los params del yaml. Puede recibir un elemento directamente (no recomendado).
#' @param x_labels Define si se mostrarán las etiquetas del eje x, pensando en un gráfico invertido (si TRUE). FALSE (opción por defecto) muestra las etiquetas de las categorías de las barras.
#' @param show_notes Define si se mostrarán las notas de pie de "N" y el mensaje sobre el redondeo (si TRUE). FALSE por defecto.
#' @param show_n Define si se mostrará el N como subtítulo (si TRUE). FALSE por defecto.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export
plot_unica_h <- function(data, var, levels = NULL, order_freq = FALSE, ancho = NULL,
                       title = NULL, unit = NULL, x_labels = TRUE,
                       show_notes = FALSE, show_n = FALSE) {

  # 1. Crear tabla con los nombres de la variable elegida ----
  tab <- as.data.frame(prop.table(table(sjlabelled::as_label(data[[var]])))*100) %>%
    dplyr::filter(Freq != 0)


  if (!is.null(levels)) {
    tab$Var1 <- factor(tab$Var1, levels = levels) # Ordenar las categorías según el argumento levels
  }
  if (isTRUE(order_freq)) {
    tab <- tab %>%
      dplyr::mutate(Var1 = as.character(Var1)) %>%
      dplyr::arrange(Freq) %>%
      dplyr::mutate(Var1 = factor(Var1, levels = Var1))
  }
  if (is.null(levels) && isFALSE(order_freq)) {
    tab <- tab %>%
      dplyr::mutate(Var1 = forcats::fct_rev(Var1)) %>%
      dplyr::arrange(Var1)
  }

  # 2. Definir título ----
  etiqueta <- attributes(data[[var]])$label
  if (is.null(title)) {
    if (is.null(etiqueta)) {
      title <- 'Título'
    } else {
      title <- etiqueta
    }
  }

  # 3. Calcular N ----
  n_total <- sum(!is.na(data[[var]]))

  # 4. Plotear ----
  ymid <- (max(tab$Freq) - min(tab$Freq))/6

  p <- ggplot2::ggplot(data = tab, aes(x = Var1, y = Freq)) +
    ggplot2::labs(title = stringr::str_wrap(title, width = 30)) +
    ggplot2::geom_col(width = if (is.null(ancho)) 0.6 else ancho, fill = '#336699') +
    ggplot2::geom_text(aes(label = paste0(round(Freq), "%")),
              hjust = ifelse(tab$Freq < ymid, -1, 0.5),
              color = ifelse(tab$Freq < ymid, "#002060", "#FFFFFF"),
              position = position_stack(vjust = 0.5),
              size = 4.2,
              family = "Arial",
              fontface = "bold")  +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = element_text(size = 16,
                                color = "#002060",
                                face = "bold",
                                family = "Arial",
                                hjust = 0.5,
                                vjust = 1),
      plot.title.position = "plot",
      text = element_text(color = "#002060",
                          family = "Arial"),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA))

  if (isTRUE(x_labels)) {
    p <- p +
      ggplot2::theme(
        axis.text.y = element_text(size = 12),
        text = element_text(color = "#002060",
                            family = "Arial")) +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60))
  }

  if (isTRUE(show_n)) {
    p <- p + ggplot2::labs(subtitle = paste('N =', n_total)) +
      ggplot2::theme(plot.subtitle = element_text(size = 10,
                                         color = "#002060",
                                         face = "italic",
                                         family = "Arial",
                                         hjust = 1,
                                         vjust = 0))
  }

  if(isTRUE(show_notes)) {

    nombre_data <- if (deparse(substitute(data)) == ".") {
      deparse(sys.call(-1)[[2]])
    } else {
      deparse(substitute(data))  # Si no está en un pipe, captura normal
    }
    if (is.null(unit)) {
      unit <- if (paste0(nombre_data, "_unit") %in% names(params)) {
        params[[paste0(nombre_data, "_unit")]]
      } else {
        "participantes"
      }
    }

    p <- p +
      ggplot2::theme(
        plot.caption = element_text(hjust=c(-0.04, 1.08),
                                    color = "#002060",
                                    size = 10,
                                    face = "bold",
                                    family = "Arial",
                                    margin = margin(t = 40)),
        plot.caption.position = "plot") +
      ggplot2::labs(caption = c(paste('Base:', n_total, unit),
                       "Los porcentajes están redondeados y pueden no sumar 100%"))
  }

  g <- ggplot2::ggplotGrob(p)
  g$widths[6] <- unit(4, "cm")

  # Guardarlo como gráfico compatible con RMarkdown
  p_fixed <- ggpubr::as_ggplot(g)  # Convierte de nuevo en un objeto ggplot2

  return(p_fixed)
}
