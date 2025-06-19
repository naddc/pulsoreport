#' Genera un gráfico de barras horizontales apiladas para preguntas escalares, para PowerPoint
#'
#' @import ggplot2
#' @import ggh4x
#' @import ggpubr
#' @import stringr
#' @param data Un data frame con los datos en formato sav.
#' @param vars Variable(s) para plotear.
#' @param levels Niveles de la escala para ordenar la presentación. Va de negativo a positivo en cuatro opciones y después de eso, valores NA.
#' @param T2B Define si se calcula e incluye en el gráfico el "Top 2 Box" (TRUE por defecto).
#' @param unit Unidad de observación para declarar el N de la base usada para el gráfico.
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export
plot_escala <- function(data,
                        vars,
                        levels = NULL,
                        T2B = TRUE,
                        unit = NULL,
                        unit_extra = TRUE) {

  # 1. CREACIÓN DE TABLA ----
  tablas <- list() # Crear lista para almacenar tablas y etiquetas
  etiquetas <- list() # Crear lista para almacenar etiquetas
  for (var in vars) {
    etiqueta <- if (!is.null(attributes(data[[var]])$label)) { # Obtener etiqueta de variable (si existe)
      attributes(data[[var]])$label
    } else {
      var # Devolver nombre de la variable si no existen los elementos anteriores
    }

    tab <- as.data.frame(prop.table(table(sjlabelled::as_label(data[[var]])))*100) # Crear tabla de proporciones

    if (is.null(levels)) {
      levels <- names(attr(data[[var]], 'labels'))
    }
    if (is.list(levels)) { # Verificar si levels es una lista
      nivel_actual = levels[[var]]
    } else {
      nivel_actual = levels
    }

    tab <- tab %>%
      dplyr::mutate(control = etiqueta,
             Var1 = factor(Var1,
                           levels = nivel_actual)) %>%
      dplyr::arrange(Var1)

    tablas[[var]] <- tab # Almacenar la tabla y la etiqueta en la lista
    etiquetas[[var]] <- etiqueta
  }

  tab <- do.call(rbind, tablas) # Combinar todas las tablas en una sola
  tab$control <- factor(tab$control, levels = rev(etiquetas[vars]))
  tab <- dplyr::filter(tab, Freq != 0)

  # 2. PLOTEO ----
  ## 2.1. Asignar colores de escala ----
  colores <- c() # Crear un vector de colores basado en los levels presentes en los datos
  for (var in vars) {
    if (is.list(levels)) {
      nivel_actual <- levels[[var]]
    } else {
      nivel_actual <- levels
    }
    color_map <- c('#F4B183',
                   '#FFD965',
                   '#ADD493',
                   '#70AD47',
                   '#A5A5A5',
                   rep('#D9D9D9', length(nivel_actual) - 5))
    names(color_map) <- nivel_actual
    colores <- c(colores, color_map)
  }
  colores <- colores[unique(tab$Var1)] # Asegurarse de que los nombres en el vector de colores coincidan con los niveles presentes en los datos

  ## 2.2. Calcular Top2Box ----
  if (isTRUE(T2B)) {
    T2B_df <- tibble(Top2B = character(), control = character())
    for (var in vars) {

      escala <- max(sjlabelled::get_values(data[[var]])[sjlabelled::get_values(data[[var]])<5])

      T2B_calc <- data %>%
        dplyr::filter(!is.na(.[[var]])) %>%
        dplyr::mutate(top2 = (.[[var]] %in% c(escala, escala-1))) %>%
        dplyr::summarise(top2_pct = janitor::round_half_up((sum(top2, na.rm = TRUE) / n()) * 100)) %>%
        dplyr::pull(top2_pct)

      T2B_df <- dplyr::bind_rows(T2B_df, tibble(
        Top2B = paste0(T2B_calc, '%'),
        control = attributes(data[[var]])$label
      ))
    }
  }

  ## 2.3. Calcular N ----
  n_total <- data %>%
    dplyr::select(all_of(vars)) %>%
    dplyr::mutate(across(everything(), as.character)) %>%
    dplyr::filter(rowSums(!is.na(.) & . != '') > 0) %>%
    dplyr::summarise(total = n()) %>%
    dplyr::pull(total)

  nombre_data <- if (deparse(substitute(data)) == '.') {
    deparse(sys.call(-1)[[2]])
  } else {
    deparse(substitute(data))  # Si no está en un pipe, captura normal
  }
  if (is.null(unit)) {
    if (paste0(nombre_data, '_unit') %in% names(params)) {
      unit <- params[[paste0(nombre_data, '_unit')]]

      if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
        unit <- paste(unit, params[['unit_extra']])
      }
    } else {
      unit <- 'participantes'
    }
  }

  ## 2.4. Generar gráfico ----
  p <- ggplot2::ggplot(data = tab, aes(x = control, y = Freq, fill = Var1)) +
    ggplot2::geom_col(position = 'stack', width = if (length(unique(tab$control)) < 2) 0.3
                      else if (length(unique(tab$control)) == 2) 0.4
                      else 0.5) +
    ggrepel::geom_text_repel(aes(label = paste0(janitor::round_half_up(Freq), '%')),
                             position = ggpp::position_stacknudge(vjust = 0.5),
                             size = 4.93,
                             color = '#002060',
                             family = 'Arial',
                             fontface = 'bold',
                             direction = 'x',
                             point.size = NA,
                             box.padding = 0) +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggh4x::force_panelsizes(total_width = unit(21, 'cm'), total_height = unit(4.55, 'in')) +
    ggplot2::theme(
      axis.text.y = element_text(size = 12,
                                 margin = margin(l = 20)),
      text = element_text(color = '#002060',
                          family = 'Arial'),
      legend.title = element_blank(),
      legend.position = 'bottom',
      legend.text = element_text(size = 10.5,
                                 color = '#002060',
                                 family = 'Arial'),
      legend.margin = margin(t = 15, r = 350, b = 0, l = 0),
      legend.key.size=unit(0.4, 'cm'),
      plot.caption = element_text(hjust=c(-0.039, 1.07),
                                  color = '#002060',
                                  size = 10,
                                  face = 'bold',
                                  family = 'Arial',
                                  margin = margin(t = 48)),
      plot.caption.position = 'plot',
      plot.margin = margin(l = 20, r = 30, t = 100, b = 0),
      panel.background = element_rect(fill = 'transparent', colour = NA),
      plot.background = element_rect(fill = 'transparent', colour = NA)) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_fill_manual(values = colores) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 55)) +
    ggplot2::labs(caption = c(str_wrap(paste('Base:', n_total, unit), width = 80),
                     'Los porcentajes están redondeados y pueden no sumar 100%'))
  if (isTRUE(T2B)) {
    # # Obtener los límites del eje Y
    # g <- ggplot2::ggplot_build(p)
    # y_min <- g$layout$panel_params[[1]]$y.range[1]
    # y_max <- g$layout$panel_params[[1]]$y.range[2]
    #
    # # Definir la posición relativa del texto
    # y_pos <- y_max + (y_max - y_min) * 0.05  # Ajusta el 0.05 para mover más arriba o abajo

    p <- p + ggplot2::geom_text(data = T2B_df,
                       aes(x = control, y = -10, label = Top2B),
                       inherit.aes = FALSE,
                       size = 4.55,
                       color = '#548135',
                       fontface = 'bold',
                       family = 'Arial',
                       hjust = 0) +
      ggplot2::labs(subtitle = 'TOP TWO BOX') +
      ggplot2::theme(plot.subtitle = element_text(size = 13,
                                         colour = '#548135',
                                         face = 'bold',
                                         family = 'Arial',
                                         hjust = 1.05,
                                         vjust = 2))
  }

  g <- ggplot2::ggplotGrob(p) # Convertir en gtable
  g$widths[6] <- unit(10, 'cm')  # Fijar el espacio del eje Y
  p_fixed <- ggpubr::as_ggplot(g)  # Convierte de nuevo en un objeto ggplot2
  return(p_fixed)
}
