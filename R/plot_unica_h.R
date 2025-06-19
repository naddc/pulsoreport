#' Gráfico de barras horizontales para respuestas únicas
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
#' @import ggrepel
#' @param data Un data frame con los datos en formato sav.
#' @param vars Variable o variables para plotear.
#' @param levels Permite introducir un conjunto de niveles particular para ordenar variables escalares. Si es NULL (opción por defecto), toma el orden de los valores de la variable.
#' @param order_freq Define si las barras se ordenarán según frecuencia (si TRUE). FALSE (opción por defecto) permite usar el orden de levels.
#' @param group X
#' @param display Define agrupamiento de barras. Si es "simple" (opción por defecto), se muestran barras individuales. Si es "stacked", se muestran barras apiladas.
#' @param title Recibe un título para el gráfico. Si es NULL (opción por defecto), toma la etiqueta de la variable en vars.
#' @param unit Unidad de observación para declarar el N de la base usada para el gráfico. Si es NULL (opción por defecto), toma el parámeto correspondiente al dataset en data ("data_unit") indicado en los params del yaml. Puede recibir un elemento directamente (no recomendado).
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @param x_labels Define si se mostrarán las etiquetas del eje x, pensando en un gráfico invertido (si TRUE). FALSE (opción por defecto) muestra las etiquetas de las categorías de las barras.
#' @param ancho Define el ancho de las barras (0.6 por defecto).
#' @param show_notes Define si se mostrarán las notas de pie de "N" y el mensaje sobre el redondeo (si TRUE). FALSE por defecto.
#' @param show_n Define si se mostrará el N como subtítulo (si TRUE). FALSE por defecto.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export

plot_unica_h <- function(data = NULL,
                         vars,
                         levels = NULL,
                         order_freq = FALSE,
                         group = NULL,
                         display = 'simple',
                         title = NULL,
                         unit = NULL,
                         unit_extra = TRUE,
                         x_labels = TRUE,
                         labels_width = 30,
                         ancho = 0.7,
                         show_notes = FALSE,
                         show_n = FALSE) {
  # 1. Tabular y calcular Ns ------------------------------------------------
  if (!is.null(data)) {
    var_sym <- rlang::enquo(vars)
    var_name <- rlang::as_name(var_sym)

    # Etiquetar solo la variable
    data <- data %>%
      dplyr::mutate(!!var_name := sjlabelled::as_label(!!var_sym))

    # Obtener levels
    if (is.null(levels)) {
      levels <- sjlabelled::get_labels(dplyr::pull(data, !!var_sym), values = "n") %>% unname()
    }

    # Tabular
    tab <- data %>%
      sjlabelled::as_label() %>%
      dplyr::filter(!is.na(!!var_sym)) %>%
      dplyr::count(!!var_sym) %>%
      dplyr::mutate(
        !!var_name := {
          x <- .data[[var_name]]
          if (isTRUE(order_freq)) {
            forcats::fct_reorder(x, n)
          } else {
            forcats::fct_rev(forcats::fct_relevel(x, levels))
          }
        }
      ) %>%
      dplyr::mutate(prop = 100 * n / sum(n))

    # Calcular N
    n_total <- sum(!is.na(data[[rlang::as_string(ensym(vars))]]))

    # Definir título
    if (is.null(title)) {
      etiqueta <- attributes(data[[rlang::as_string(ensym(vars))]])$label
      if (is.null(etiqueta)) {
        title <- 'Título'
      } else {
        title <- etiqueta
      }
    }
  }
  if (is.null(data) && is.list(vars)) {
    tablas <- list()
    etiquetas <- list()
    n_barras <- list()
    totales <- list()
    colores <- c()

    for (nombre_data in names(vars)) {
      dataset <- get(nombre_data)  # Obtener el dataset por su nombre
      var_name <- vars[[nombre_data]]  # Obtener el nombre de la variable dentro del dataset

      for (var_name in vars[[nombre_data]]) {  # Iterar sobre todas las variables en el dataset

        if (!var_name %in% names(dataset)) next  # Saltar si la variable no está en el dataset

        etiqueta <- attributes(dataset[[var_name]])$label  # Obtener la etiqueta de la variable

        # Crear tabla de proporciones
        tab <- as.data.frame(prop.table(table(sjlabelled::as_label(dataset[[var_name]]))) * 100)

        # Obtener niveles de la variable si tiene etiquetas
        levels <- names(attr(dataset[[var_name]], 'labels'))

        # Construir tabla con la etiqueta y los niveles ordenados
        tab <- tab %>%
          dplyr::mutate(control = etiqueta,
                        group = if (paste0(nombre_data, '_unit') %in% names(params)) {
                          stringr::str_replace(params[[paste0(nombre_data, '_unit')]],'^\\w{1}', toupper)
                        } else {
                          'Participantes'
                        },
                        Var1 = factor(Var1, levels = levels)) %>%
          dplyr::arrange(Var1)

        # Guardar en la lista con identificador único
        tablas[[paste(nombre_data, var_name, sep = '_')]] <- tab
        etiquetas[[paste(nombre_data, var_name, sep = '_')]] <- etiqueta

      }

      # Calcular Ns
      n <- dataset %>%
        select(all_of(var_name)) %>%
        dplyr::mutate(across(everything(), as.character)) %>%
        filter(rowSums(!is.na(.) & . != '') > 0) %>%
        dplyr::summarise(total = dplyr::n()) %>%
        dplyr::pull(total)

      totales[[params[[paste0(nombre_data, '_unit')]]]] <- n

      n_barras[[paste(nombre_data, var_name, sep = '_')]] <- n

    }

    n_df <- tibble::tibble(
      group = unlist(lapply(names(n_barras), function(x) {
        nombre_data <- strsplit(x, "_")[[1]][1]
        if (paste0(nombre_data, '_unit') %in% names(params)) {
          stringr::str_replace(params[[paste0(nombre_data, '_unit')]], '^\\w{1}', toupper)
        } else {
          'Participantes'
        }
      })),
      N_group = unname(unlist(n_barras))
    )

    tab_final <- do.call(rbind, tablas) %>%
      filter(Freq != 0)

    tab_final <- tab_final %>%
      dplyr::left_join(n_df, by = 'group')

    n_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')

    # Crear un vector de colores basado en los levels presentes en los datos
    nivel_actual <- levels(tab_final$Var1)

    color_base <- c('#9DC3E6', '#336699', '#8EAADC', '#C19ED6', '#BDD7EE', '#FFE8A7', '#4D4D4D', '#8B7DDD', '#D0CECE')
    color_map <- c(color_base, rep('#D9D9D9', max(0, length(nivel_actual) - length(color_base))))
    color_map <- color_map[seq_along(nivel_actual)]
    names(color_map) <- nivel_actual

    colores <- color_map[unique(tab_final$Var1)]

    # Ordenar según group (públicos) y control (enunciados)
    # Crear vector ordenado de públicos según orden de vars y params
    orden_grupos <- sapply(names(vars), function(nombre_data) {
      unidad_param <- paste0(nombre_data, '_unit')
      if (unidad_param %in% names(params)) {
        stringr::str_replace(params[[unidad_param]], '^\\w{1}', toupper)
      } else {
        'Participantes'
      }
    }) %>% rev(.)

    # Crear vector ordenado de enunciados según primeras variables en primeros elementos en list 'vars'
    orden_control <- c()
    for (nombre_data in names(vars)) {
      dataset <- get(nombre_data)
      for (var_name in vars[[nombre_data]]) {
        if (var_name %in% names(dataset)) {
          etiqueta <- attributes(dataset[[var_name]])$label
          if (!is.null(etiqueta) && !(etiqueta %in% orden_control)) {
            orden_control <- c(orden_control, etiqueta)
          }
        }
      }
    }
    orden_control_wrapped <- str_wrap(orden_control, width = 30)

    # Ordenar tab_final
    tab_final$group <- factor(tab_final$group, levels = orden_grupos)
    tab_final$control <- str_wrap(tab_final$control, width = 30)
    tab_final$control <- factor(tab_final$control, levels = orden_control_wrapped)

    display <- 'stacked'

    if (length(unique(tab_final$group)) <= 1) {
      x_labels <- FALSE
    }
  }

  # 2. Plotear --------------------------------------------------------------

  if (!is.null(data) && display == 'simple') {

    p <- ggplot2::ggplot(data = tab,
                         aes(x = if (!rlang::quo_is_null(enquo(group))) {{group}} else {{ vars }},
                             y = prop,
                             fill = if (!rlang::quo_is_null(enquo(group))) {{ vars }} else NULL)) +
      ggplot2::labs(title = stringr::str_wrap(title, width = 30)) +
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
    if (!rlang::quo_is_null(enquo(group))) {
      p <- p +
        ggplot2::geom_col(width = 0.8, position = 'stack', color = "white") +
        ggplot2::geom_text(aes(label = paste0(janitor::round_half_up(prop), "%")),
                           position = position_stack(vjust = 0.5),
                           size = 12/2.845,
                           color = "#FFFFFF",
                           family = "Arial",
                           fontface = "bold")  +
        ggplot2::coord_flip() +
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
            axis.text.y = element_text(size = 12),
            text = element_text(color = "#002060",
                                family = "Arial")) +
          ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = labels_width))
      }
    }
    else {
      ymid <- (max(tab$prop) - min(tab$prop))/6
      p <- p +
        ggplot2::geom_col(width = ancho, fill = '#336699') +
        ggplot2::geom_text(aes(label = paste0(janitor::round_half_up(prop), "%")),
                           hjust = ifelse(tab$prop < ymid, -1, 0.5),
                           color = ifelse(tab$prop < ymid, "#002060", "#FFFFFF"),
                           position = position_stack(vjust = 0.5),
                           size = 4.2,
                           family = "Arial",
                           fontface = "bold")  +
        ggplot2::coord_flip()

      if (isTRUE(x_labels)) {
        p <- p +
          ggplot2::theme(
            axis.text.y = element_text(size = 12),
            text = element_text(color = "#002060",
                                family = "Arial")) +
          ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = labels_width))
      }
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
    final_plot <- p
  }
  else if (is.null(data) && is.list(vars) && display == 'stacked') {
    if (length(unique(tab_final$control)) > 1) {

      control_levels <- unique(tab_final$control)

      plots_list <- purrr::map(control_levels, function(ctrl) {
        df_ctrl <- tab_final %>% filter(control == ctrl)

        p <- ggplot(df_ctrl, aes(x = group, y = Freq, fill = Var1)) +
          geom_col(position = 'stack', width = if (length(unique(df_ctrl$group)) == 1) 0.4 else ancho) +
          geom_text_repel(
            aes(label = paste0(janitor::round_half_up(Freq), '%')),
            position = ggpp::position_stacknudge(vjust = 0.5),
            size = 4.93,
            color = '#ffffff',
            family = 'Arial',
            fontface = 'bold',
            direction = 'x',
            point.size = NA,
            box.padding = 0
          ) +
          coord_flip() +
          scale_fill_manual(values = colores) +
          scale_y_reverse() +
          scale_x_discrete(
            labels = function(x) str_wrap(x, width = 60),
            expand =  if (length(unique(df_ctrl$group)) == 1) expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))
          ) +
          theme_void() +
          theme(
            axis.ticks.y = element_blank(),
            legend.position = 'none',
            plot.margin = margin(t = -10)
          )

        if (length(unique(tab_final$group)) <= 1) {
          x_labels <- FALSE
        }

        if (isTRUE(x_labels)) {
          p <- p +
            ggplot2::theme(
              axis.text.y = element_text(size = 12,
                                         margin = margin(r = 10),
                                         family = 'Arial',
                                         color = '#002060')
              )
        }
        p
      })

      # Alinear los gráficos entre sí en eje vertical, compartiendo eje Y (izquierda)
      aligned <- cowplot::align_plots(plotlist = plots_list, align = 'v', axis = 'l')

      plots_with_titles <- purrr::map2(control_levels, aligned, function(ctrl, aligned_plot) {
        text_label <- textGrob(
          ctrl,
          x = 0.4,
          gp = gpar(
            fontsize = 13,
            fontfamily = 'Arial',
            col = '#002060',
            fontface = if (isTRUE(x_labels)) 'bold' else 'plain',
            lineheight = 0.8
          )
        )

        # Combinar todo en arranged
        arranged <- arrangeGrob(
            text_label, aligned_plot,
            ncol = 2,
            widths = unit.c(unit(9, 'cm'), unit(20, 'cm'))
          )

        # Envolver para fondo transparente
        grobTree(arranged, vp = viewport(gp = gpar(fill = 'transparent')))
      })

      # Finalmente juntamos verticalmente todos los gráficos combinados
      n_barras <- sapply(control_levels, function(ctrl) {
        length(unique(tab_final$group[tab_final$control == ctrl]))
      })

      n_barras_ajustado <- sapply(n_barras, function(n) {
        if (n <= 1) 1 else 1 + (n - 2)^1.2
      })

      n_barras_ajustado <- n_barras_ajustado / sum(n_barras_ajustado) * 6
      n_barras_ajustado <- pmin(n_barras_ajustado, 5)

      # Combinar todos los gráficos
      plots_combined <- patchwork::wrap_plots(plots_with_titles,
                                              ncol = 1,
                                              heights = n_barras_ajustado)
      plots_combined <- patchwork::wrap_plots(plots_combined,
                                              nrow = 1,
                                              heights = c(6))

      p <- plots_combined

    }
    else{
    p <- ggplot2::ggplot(data = tab_final,
                         ggplot2::aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(
        position = 'stack',
        width = ancho) +
      ggrepel::geom_text_repel(ggplot2::aes(label = paste0(janitor::round_half_up(Freq), '%')),
                               position = ggpp::position_stacknudge(vjust = 0.5),
                               size = 4.93,
                               color = '#ffffff',
                               family = 'Arial',
                               fontface = 'bold',
                               direction = 'x',
                               point.size = NA,
                               box.padding = 0) +
      ggh4x::force_panelsizes(total_width = ggplot2::unit(9, 'in')
                              # ,
                              # total_height = ggplot2::unit(4.5, 'in')
      ) +
      ggplot2::theme_void() +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 12,
                                            margin = ggplot2::margin(r = 20)),
        text = ggplot2::element_text(color = '#002060',
                                     family = 'Arial'),
        legend.position = 'none') +
      ggplot2::scale_y_reverse()
    if (length(unique(tab_final$group)) <= 1) {
      x_labels <- FALSE
    }
    if (isTRUE(x_labels)) {
      p <- p +
        ggplot2::theme(
          axis.text.y = ggplot2::element_text(size = 12),
          text = ggplot2::element_text(color = "#002060",
                                       family = "Arial")) +
        ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60))
    }
    p <- p +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::coord_flip()
    if (isTRUE(show_n)) {
      n_labels <- tab_final %>%
        group_by(group) %>%
        mutate(
          y_pos = 5
        )
      p <- p +
        ggplot2::geom_text(
          data = n_labels,
          ggplot2::aes(label = paste0('N = ', N_group), x = group, y = y_pos),
          hjust = 0,
          vjust = -5,
          size = 3.5,
          family = 'Arial',
          fontface = 'italic',
          color = '#002060',
          inherit.aes = FALSE)
      }

    p_grob <- ggplot2::ggplotGrob(p)
    p_grob$widths[[which(p_grob$layout$name == "axis-l")]] <- ggplot2::unit(1, "cm")
    p <- ggpubr::as_ggplot(p_grob)
    }

    # Añadir leyenda
    # solución de @teunbrand
    get_legend <- function(plot, legend = NULL) {

      gt <- ggplot2::ggplotGrob(plot)

      pattern <- 'guide-box'
      if (!is.null(legend)) {
        pattern <- paste0(pattern, '-', legend)
      }

      indices <- grep(pattern, gt$layout$name)

      not_empty <- !vapply(
        gt$grobs[indices],
        inherits, what = 'zeroGrob',
        FUN.VALUE = logical(1)
      )
      indices <- indices[not_empty]

      if (length(indices) > 0) {
        return(gt$grobs[[indices[1]]])
      }
      return(NULL)
    }
    # Plot dummy para extraer leyenda
    legend_plot <- ggplot2::ggplot(tab_final, ggplot2::aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(position = 'stack') +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = 'bottom',
        legend.title = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 10.5, color = '#002060', family = 'Arial'),
        legend.key.size = ggplot2::unit(0.4, 'cm'),
        legend.margin = ggplot2::margin(l = 20, b = 10)
      ) +
      ggplot2::guides(shape = ggplot2::guide_legend(position = 'bottom'))

    legend_grob <- get_legend(legend_plot)

    legend_patch <- patchwork::wrap_elements(full = legend_grob) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

    final_plot <- p / legend_patch +
      patchwork::plot_layout(ncol = 1,
                             heights = c(5, 1))

    # Aplicar tema general
    final_plot <- final_plot +
      patchwork::plot_annotation(
        theme = ggplot2::theme(
          plot.margin = ggplot2::margin(t = 100, b = -15))) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    final_plot <- patchwork::wrap_elements(panel = final_plot) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
  }

  # 3. Añadir notal al pie --------------------------------------------------
  if(isTRUE(show_notes)) {
  nombre_data <- if (deparse(substitute(data)) == ".") {
    deparse(sys.call(-1)[[2]])
  } else {
    deparse(substitute(data))  # Si no está en un pipe, captura normal
  }

  if (is.null(unit) && !is.null(data)) {
    unit <- if (paste0(nombre_data, "_unit") %in% names(params)) {
      params[[paste0(nombre_data, "_unit")]]
    }
    else if (!is.null(unit) && !is.null(data)) {
      unit
    }
    else {
      'participantes'
    }
  }

  if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
      if (exists('n_totales') && is.null(data)) {
        n_totales <- paste(n_totales, params[['unit_extra']])
      }
      if (!is.null(unit)) {
        unit <- paste(unit, params[['unit_extra']])
      }
    }

    # Texto izquierdo (con lineheight)
    footer_izq <- ggplot2::ggplot() +
      ggplot2::geom_text(
        ggplot2::aes(x = -0.06, y = 0, label = stringr::str_wrap(paste("Base:",
                                                                       if (exists("n_totales")) n_totales else "",
                                                                       if (exists("n_total")) n_total else "",
                                                                       if (exists("unit")) unit else ""),
                                                                 width = 67)),
        hjust = 0, family = "Arial", size = 3.5, fontface = "bold",
        color = "#002060", lineheight = 0.85
      ) +
      ggplot2::coord_cartesian(xlim = c(0, 1), clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 0, r = 0, b = 12, l = -1)
      )

    # Texto derecho (con lineheight)
    footer_der <- ggplot2::ggplot() +
      ggplot2::geom_text(
        ggplot2::aes(x = 1.05, y = 0, label = "Los porcentajes están redondeados y pueden no sumar 100%"),
        hjust = 1, family = "Arial", size = 3.5, fontface = "bold",
        color = "#002060", lineheight = 0.85
      ) +
      ggplot2::coord_cartesian(xlim = c(0, 1), clip = "off") +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(t = 0, r = -1, b = 12, l = 0)
      )

    # Footer en dos columnas usando patchwork
    footer_plot <- (footer_izq + footer_der) +
      patchwork::plot_layout(widths = c(5, 10)) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    footer_plot_wrapped <- patchwork::wrap_elements(panel = footer_plot) &
      theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

    final_plot <- (final_plot) / (footer_plot_wrapped) +
      patchwork::plot_layout(heights = c(15, 1)) +
      patchwork::plot_annotation(
        theme = ggplot2::theme(plot.margin = ggplot2::margin(b = -18))) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

    # final_plot <- final_plot / footer_plot +
    #   patchwork::plot_layout(nrow = 3, heights = c(5, 1, 0.4))
  }

  # 4. Render final ------------------------------------------------------------
  final_plot <- final_plot &
    theme(
      panel.background = element_blank(),
      plot.background  = element_blank()
    )

  p_fixed <- final_plot &
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = 'transparent', color = NA),
      panel.background = ggplot2::element_rect(fill = 'transparent', color = NA)
    )
  return(p_fixed)
}
