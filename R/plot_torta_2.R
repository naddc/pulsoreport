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
#' @param unit description
#' @param unit_extra description
#' @param show_n FALSE por defecto. Si es TRUE, calcula e incluye en el gráfico el "N" de registros que respondieron dentro del gráfico.
#' @return Un objeto ggplot que puede ser exportado como dml para MS PPT o emf para MS Word.
#' @export

plot_torta_2 <- function(data,
                         var,
                         title = NULL,
                         title.pos = 'top',
                         legend.pos = 'bottom',
                         paleta = c('#9DC3E6','#336699'),
                         unit = TRUE,
                         unit_extra = TRUE,
                         show_n = FALSE) {
  output_type <- knitr::opts_knit$get('rmarkdown.pandoc.to')
  if (is.null(output_type)) output_type <- "docx"  # o "docx", según lo que uses por defecto

  # 1. Tabular ----
  p <- data %>%
    sjlabelled::as_label() %>%
    dplyr::filter(!is.na({{var}})) %>%
    dplyr::count({{var}}) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::mutate(pct = 100 * prop,
                  # @aito123 solución para centrar las etiquetas
                  # Compute the cumulative percentages (top of each rectangle)
                  ymax=cumsum(prop),
                  # Compute the bottom of each rectangle
                  ymin=c(0, head(ymax, n=-1)),
                  # Compute label position
                  labelPosition=(ymax + ymin) / 2)

  n_total <- sum(!is.na(data[[rlang::as_name(rlang::enquo(var))]]))

  # 2. Plotear ----
  colores <- colorRampPalette(paleta)

  p <- p %>%
    ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=0, fill={{var}})) +
    geom_rect(colour="#FFFFFF", size = 0.5) +
    geom_text(x = 2.5,
              aes(y = labelPosition,
                  label = paste0(janitor::round_half_up(pct), '%')),
              size = if (output_type == 'docx') 9*0.35 else 14*0.35,
              fontface = "bold",
              color = "#FFFFFF",
              hjust="1") +
    ggplot2::scale_fill_manual(values = colores(nrow(p))) +
    coord_polar(theta="y", start=0) +
    xlim(c(0, 4)) +
    theme_void() +
    ggplot2::theme(text = element_text(color = '#002060', family = 'Arial'),
                   legend.title = element_blank(),
                   legend.position = legend.pos,
                   legend.text = element_text(
                     size = if (output_type == 'docx') 7 else 12,
                     color = '#002060',
                     family = 'Arial'),
                   legend.margin = margin(t = 10),
                   legend.key.size = unit(if (output_type == "docx") 0.35 else 0.4, 'cm'),
                   panel.background = element_rect(fill = 'transparent', colour = NA),
                   plot.background = element_rect(fill = 'transparent', colour = NA))

    # ggplot2::ggplot(aes(x = '', y = pct, fill = {{var}})) +
    # ggplot2::geom_bar(stat = 'identity', width = 1, color = '#FFFFFF') +
    # ggplot2::coord_polar('y', start = 0) +
    # ggplot2::geom_text(aes(label = paste0(janitor::round_half_up(pct), '%')),
    #                    position = position_stack(vjust = 0.7),
    #                    size = if (output_type == 'docx') 9*0.35 else 14*0.35,
    #                    fontface = 'bold',
    #                    color = '#FFFFFF',
    #                    hjust = 1) +
    # ggplot2::theme_void() +
    # ggplot2::theme(text = element_text(color = '#002060', family = 'Arial'),
    #                legend.title = element_blank(),
    #                legend.position = legend.pos,
    #                legend.text = element_text(
    #                  size = if (output_type == 'docx') 9 else 12,
    #                  color = '#002060',
    #                  family = 'Arial'),
    #                legend.margin = margin(t = 10),
    #                legend.key.size=unit(0.4, 'cm'),
    #                panel.background = element_rect(fill = 'transparent', colour = NA),
    #                plot.background = element_rect(fill = 'transparent', colour = NA)) +
    # ggplot2::scale_fill_manual(values = c('#9DC3E6', '#336699'))

  # 3. Configurar título, subtítulo y nota de pie ----

  if (title.pos == 'top') {
    p <- p +
      ggplot2::labs(title =
                      if (output_type == 'docx') NULL
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
        ggplot2::labs(subtitle = paste('N =', n_total)) +
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
                        label =
                          if (output_type == 'docx') NULL
                        else stringr::str_wrap(
                          if (is.null(title)) attributes(data[[rlang::as_name(rlang::enquo(var))]])$label
                          else title, width = 50),
                        size = 4.93, hjust = 0, color = '#002060') +
      ggplot2::theme_void()

    p <- p +
      ggplot2::theme(plot.margin = margin(t = -50, l = 200),
                     plot.title = element_blank(),
                     legend.margin = margin(t = 5))

    if (isTRUE(show_n)) {
      p <- p +
        ggplot2::labs(subtitle = paste('N =', n_total)) +
        ggplot2::theme(plot.subtitle = element_text(size = if (output_type == 'docx') 9 else 10,
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

  nombre_data <- if (deparse(substitute(data)) == '.') {
    deparse(sys.call(-1)[[2]])
  } else {
    deparse(substitute(data))
  }

  if (isTRUE(unit)) {
    unit <- params[[paste0(nombre_data, '_unit')]]

    if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
      unit_long <- paste(unit, params[['unit_extra']])
    }
  }

  if (output_type == 'docx' && is.null(title)) {
    title <- attributes(data[[rlang::as_name(rlang::enquo(var))]])$label
  }

  if (output_type == 'docx') {
    return(list(
      plot = p,
      cap = if (!is.null(title) && is.character(title)) paste0(title, ', ', unit)else NULL,
      N = paste('N =',
                 if (exists('n_total')) n_total else '',
                 # if (exists('unit_long')) unit_long
                if (exists('unit')) unit
                else ''
                 )
      )
    )
  }
  else {
    return(p)
  }
}
