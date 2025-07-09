#' Gráfico de torta para preguntas dicotómicas, para PowerPoint
#'
#' @import ggplot2
#' @import patchwork
#' @import stringr
#' @importFrom rlang as_name
#' @importFrom rlang enquo
#' @param data Un data frame con los datos en formato sav.
#' @param var Variable para plotear.
#' @param title Recibe un título para el gráfico. Si es NULL (opción por defecto), toma la etiqueta de la variable en var.
#' @param title.pos Define la posición del título con respecto al gráfico. Si es 'top' (opción por defecto), va encima; si es 'left', el título va a la izquierda y el gráfico a la derecha.
#' @param show_n FALSE por defecto. Si es TRUE, calcula e incluye en el gráfico el "N" de registros que respondieron.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export

plot_torta <- function(data,
                       var,
                       title = NULL,
                       title.pos = 'top',
                       legend.pos = 'bottom',
                       show_n = FALSE) {
  output_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # 1. Tabular ----
  p <- data %>%
    sjlabelled::as_label() %>%
    dplyr::filter(!is.na({{var}})) %>%
    dplyr::count({{var}}) %>%
    dplyr::mutate(prop = 100 * n / sum(n)) %>%
    dplyr::select({{var}}, prop) %>%

    # 2. Plotear ----
  ggplot2::ggplot(aes(x = '', y = prop, fill = {{var}})) +
    ggplot2::geom_bar(stat = 'identity', width = 1, color = '#FFFFFF') +
    ggplot2::coord_polar('y', start = 0) +
    ggplot2::geom_text(aes(label = paste0(round(prop), '%')),
                       position = position_stack(vjust = 0.7),
                       size = 4.93,
                       fontface = 'bold',
                       color = '#FFFFFF',
                       hjust = 1) +
    ggplot2::theme_void() +
    ggplot2::theme(text = element_text(color = '#002060', family = 'Arial'),
                   legend.title = element_blank(),
                   legend.position = legend.pos,
                   legend.text = element_text(
                     size = 12,
                     color = '#002060',
                     family = 'Arial'),
                   legend.margin = margin(t = 10),
                   legend.key.size=unit(0.4, 'cm'),
                   panel.background = element_rect(fill = 'transparent', colour = NA),
                   plot.background = element_rect(fill = 'transparent', colour = NA)) +
    ggplot2::scale_fill_manual(values = c('#5B92C9', '#336699'))

  # 3. Ubicar título y subtítulo ----
  if (title.pos == 'top') {
    p <- p +
      ggplot2::labs(title =
                      if (output_type == "docx" || is.null(title)) NULL
                    else stringr::str_wrap(
                      if (is.null(title)) attributes(data[[rlang::as_name(rlang::enquo(var))]])$label
                      else title, width = 30)) +
      ggplot2::theme(plot.title = element_text(size = 16,
                                               color = '#002060',
                                               family = 'Arial',
                                               face = 'bold',
                                               hjust = 0.5,
                                               vjust = 1),
                     plot.title.position = 'plot')

    if (isTRUE(show_n)) {
      p <- p +
        ggplot2::labs(subtitle = paste('N =', sum(!is.na(data[[rlang::as_name(rlang::enquo(var))]])))) +
        ggplot2::theme(plot.subtitle = element_text(size = 10,
                                                    color = '#002060',
                                                    face = 'italic',
                                                    family = 'Arial',
                                                    hjust = 0.5,
                                                    vjust = -1))
    }

  }
  else if (title.pos == 'left') {
    title_text <- ggplot2::ggplot() +
      ggplot2::annotate('text', x = 0, y = 0,
                        label = stringr::str_wrap(if (is.null(title)) attributes(data[[rlang::as_name(rlang::enquo(var))]])$label
                                                  else title, width = 50),
                        size = 4.93, hjust = 0, color = '#002060') +
      ggplot2::theme_void()

    p <- p +
      ggplot2::theme(plot.margin = margin(t = -50, l = 200),
                     plot.title = element_blank(),
                     legend.margin = margin(t = 5))

    if (isTRUE(show_n)) {
      p <- p +
        ggplot2::labs(subtitle = paste('N =', sum(!is.na(data[[rlang::as_name(rlang::enquo(var))]])))) +
        ggplot2::theme(plot.subtitle = element_text(size = 10,
                                                    color = '#002060',
                                                    face = 'italic',
                                                    family = 'Arial',
                                                    hjust = 0.8,
                                                    vjust = -12.5))
    }

    p <- list(title_text, p) %>%
      patchwork::wrap_plots() +
      patchwork::plot_layout(width = c(1.2, 5)) &
      ggplot2::theme(plot.background = element_rect(fill = 'transparent', color = NA))
  }

  if (output_type == "docx" && !is.null(title)) {
    return(list(plot = p, cap = title))
  } else {
    return(p)
  }
}
