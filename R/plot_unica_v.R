#' Gráfico de barras verticales para respuestas únicas
#'
#' @import ggplot2
#' @import ggh4x
#' @import ggtext
#' @import grid
#' @import gridExtra
#' @import patchwork
#' @import ggpubr
#' @import stringr
#' @importFrom rlang quo_is_null
#' @importFrom rlang as_string
#' @importFrom rlang ensym
#' @param data Un data frame con los datos en formato sav.
#' @param var Variable para plotear.
#' @param order_freq Define si las barras se ordenarán según frecuencia (si TRUE). FALSE (opción por defecto) permite usar el orden de levels.
#' @param title Recibe un título para el gráfico. Si es NULL (opción por defecto), toma la etiqueta de la variable en var.
#' @param unit Unidad de observación para declarar el N de la base usada para el gráfico. Si es NULL (opción por defecto), toma el parámeto correspondiente al dataset en data ("data_unit") indicado en los params del yaml. Puede recibir un elemento directamente (no recomendado).
#' @param x_labels Define si se mostrarán las etiquetas del eje x (si TRUE). FALSE (opción por defecto) muestra las etiquetas de las categorías de las barras.
#' @param show_notes Define si se mostrarán las notas de pie de "N" y el mensaje sobre el redondeo (si TRUE). FALSE por defecto.
#' @param show_n Define si se mostrará el N como subtítulo (si TRUE). FALSE por defecto.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export

plot_unica_v <- function(data,
                         var,
                         group = NULL,
                         order_freq = FALSE,
                         title = NULL,
                         unit = NULL,
                         x_labels = TRUE,
                         show_notes = FALSE,
                         show_n = FALSE) {

  # 1. Crear tabla con los nombres de la variable elegida ----
  tab <- data %>%
    sjlabelled::as_label() %>%
    dplyr::filter(!is.na({{var}})) %>%
    { if (!quo_is_null(enquo(group))) dplyr::group_by(., {{ group }}) else . } %>%
    dplyr::count({{ var }}) %>%
    {
      if (isTRUE(order_freq)) {
        dplyr::mutate(., !!enquo(var) := forcats::fct_reorder(!!enquo(var), n, .desc = TRUE))
      } else .
    } %>%  # Reordenar niveles
    dplyr::arrange(n) %>%
    dplyr::mutate(prop = 100 * n / sum(n))

  # 2. Definir título ----
  etiqueta <- attributes(data[[as_string(ensym(var))]])$label
  if (is.null(title)) {
    if (is.null(etiqueta)) {
      title <- 'Título'
    } else {
      title <- etiqueta
    }
  }

  # 3. Calcular N ----
  n_total <- sum(!is.na(data[[as_string(ensym(var))]]))

  # 4. Plotear ----
  p <- ggplot2::ggplot(data = tab,
              aes(x = if (!quo_is_null(enquo(group))) {{group}} else {{var}},
                  y = prop,
                  fill = if (!quo_is_null(enquo(group))) {{ var }} else NULL)) +
    ggplot2::labs(title = str_wrap(title, width = 60)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = element_text(size = 14,
                                color = "#002060",
                                family = "Arial",
                                hjust = 0.5,
                                vjust = 1),
      plot.title.position = "plot",
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA))
  if (!quo_is_null(enquo(group))) {
    p <- p +
      ggplot2::geom_col(width = 0.8, position = 'stack', color = "white") +
      ggplot2::geom_text(aes(label = paste0(janitor::round_half_up(prop), "%")),
                position = position_stack(vjust = 0.5),
                size = 12/2.845,
                color = "#FFFFFF",
                family = "Arial",
                fontface = "bold")  +
      ggplot2::scale_fill_manual(values = c('#8EAADC', '#336699', '#C19ED6', '#BDD7EE', '#FFE8A7', '#4D4D4D', '#8B7DDD', '#D0CECE')) +
      ggplot2::theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(
          size = 12,
          color = "#002060",
          family = "Arial"),
        legend.margin = margin(t = 20),
        legend.key.size=unit(0.4, 'cm'),
        legend.key.spacing.x = unit(0.5, 'cm'),
        plot.margin = margin(t = 5, b = 5)
      )
    if (isTRUE(x_labels)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = element_text(size = 11),
          text = element_text(color = "#002060",
                              family = "Arial")) +
        ggplot2::scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
    }
  }
  else {
    ymid <- (max(tab$prop) - min(tab$prop))/7
    p <- p +
      ggplot2::geom_col(width = 0.8, fill = '#336699') +
      ggplot2::geom_text(aes(label = paste0(janitor::round_half_up(prop), "%")),
                         # ifelse(tab$prop < ymid, -1.5, 1.5)
                         vjust = -1.5,
                         # ifelse(tab$prop < ymid, "#002060", "#FFFFFF")
                         color = "#002060",
                         size = 4.2,
                         family = "Arial",
                         fontface = "bold")
    if (isTRUE(x_labels)) {
      p <- p +
        ggplot2::theme(
          axis.text.x = element_text(size = 12),
          text = element_text(color = "#002060",
                              family = "Arial")) +
        ggplot2::scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
      }
    }
  if (isTRUE(show_n)) {
    p <- p + labs(subtitle = paste('N =', n_total)) +
      ggplot2::theme(plot.subtitle = element_text(size = 10,
                                         color = "#002060",
                                         face = "italic",
                                         family = "Arial",
                                         hjust = 0.5,
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
      ggplot2::theme(plot.caption = element_text(hjust=c(-0.04, 1.08),
                                        color = "#002060",
                                        size = 10,
                                        face = "bold",
                                        family = "Arial",
                                        margin = margin(t = 40)),
            plot.caption.position = "plot") +
      ggplot2::labs(caption = c(paste('Base:', n_total, unit),
                       "Los porcentajes están redondeados y pueden no sumar 100%"))
  }
  p
}
