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

plot_unica_h_2 <- function(data = NULL,
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
                           ancho = 0.8, # 0.7
                           show_notes = FALSE,
                           show_n = FALSE) {
  output_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(output_type)) output_type <- "docx"

  # 1. Tabular y calcular Ns ------------------------------------------------
  if (!is.null(data)) {
    var_sym <- rlang::enquo(vars)
    var_name <- rlang::as_name(var_sym)

    # Etiquetar solo la variable
    data_etiquetada <- data %>%
      dplyr::mutate(!!var_name := sjlabelled::as_label(!!var_sym))

    # Obtener levels
    if (is.null(levels)) {
      levels <- sjlabelled::get_labels(dplyr::pull(data_etiquetada, !!var_sym), values = "n") %>% unname()
    }

    # Tabular
    tab_final <- data_etiquetada %>%
      sjlabelled::as_label() %>%
      dplyr::filter(!is.na(!!var_sym)) %>%
      dplyr::count(!!var_sym) %>%
      dplyr::mutate(
        !!var_name := {
          x <- .data[[var_name]]
          if (isTRUE(order_freq)) {
            forcats::fct_reorder(x, n)
          } else {
            # forcats::fct_relevel(x, levels)

            forcats::fct_relevel(x, rev(levels))

            # forcats::fct_rev(forcats::fct_relevel(x, levels))
          }
        }
      ) %>%
      dplyr::mutate(prop = 100 * n / sum(n))

    # Calcular N
    n_total <- sum(!is.na(data_etiquetada[[rlang::as_string(ensym(vars))]]))

    # Definir título
    if (is.null(title)) {
      etiqueta <- attributes(data_etiquetada[[rlang::as_string(ensym(vars))]])$label
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
        dplyr::select(dplyr::all_of(var_name)) %>%
        dplyr::mutate(dplyr::across(everything(), as.character)) %>%
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
  }

  # 2. Plotear --------------------------------------------------------------
  ## 2.1. simple ------------------------------------------------------------

  if (!is.null(data) && display == 'simple') {

    p <- ggplot2::ggplot(data = tab_final,
                         aes(x = if (!rlang::quo_is_null(enquo(group))) {{group}} else {{ vars }},
                             y = prop,
                             fill = if (!rlang::quo_is_null(enquo(group))) {{ vars }} else NULL)) +
      ggplot2::scale_x_discrete(labels = NULL,
                                expand = expansion(add = c(0.7, 0.7))) +
      ggplot2::scale_y_continuous(limits = c(0, 100),
                                  breaks = seq(0, 100, by = 25),
                                  labels = c('0', '25', '50', '75', '100 (%)'),
                                  expand = c(0, 0)) +
      ggplot2::labs(title = if (output_type == "docx" || is.null(title)) NULL
                    else stringr::str_wrap(title, width = 30)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.line.x = element_line(colour = "#cccccc",
                                 size = 0.3),
        axis.text.x = element_text(color = "#cccccc",
                                   size = 7),
        axis.ticks.x.bottom = element_line(color = "#cccccc",
                                           linewidth = 0.3),
        axis.line.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.title = element_blank(),
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
                           size = if (output_type == "docx") 9*0.35 else 12*0.35,
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
            axis.text.y = element_text(size = if (output_type == "docx") 9 else 12,
                                       color = "#002060",
                                       family = "Arial"),
            text = element_text(color = "#002060",
                                family = "Arial")) +
          ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = labels_width))

      }
    }
    else {
      ymid <- (max(tab_final$prop) - min(tab_final$prop))/6
      p <- p +
        ggplot2::geom_col(width = ancho, fill = '#336699') +
        ggplot2::geom_text(aes(label = paste0(janitor::round_half_up(prop), "%")),
                           hjust = ifelse(tab_final$prop < ymid, -1.5, 0.5),
                           color = ifelse(tab_final$prop < ymid, "#002060", "#FFFFFF"),
                           position = position_stack(vjust = 0.5),
                           size = if (output_type == "docx") 9*0.35 else 12*0.35,
                           family = "Arial",
                           fontface = "bold")  +
        ggplot2::coord_flip()

      if (isTRUE(x_labels)) {
        label_df <- tab_final %>%
          dplyr::select(label = {{ var_name }}) %>%
          dplyr::distinct()
        # %>%
        #   dplyr::mutate(label = factor(label, levels = unique(label)))

        y_label_plot <- ggplot(label_df, aes(x = 1, y = label)) +
          geom_text(aes(label = stringr::str_wrap(label, width = labels_width)),
                    hjust = 1,
                    lineheight = 0.8,
                    color = "#002060",
                    size = 9 * 0.35,
                    family = "Arial") +
          scale_y_discrete(limits = levels(label_df$label),
                           expand = expansion(add = c(1.45, 0.9))) +
          scale_x_continuous(limits = c(0, 1),
                             expand = c(0, 0)) +
          coord_cartesian(clip = "off") +
          theme_void() +
          theme(plot.margin = margin(b = 0, t = 0))

        y_label_grob <- ggplotGrob(y_label_plot)
      } else {
        y_label_grob <- grid::nullGrob()
      }

      p <- gridExtra::arrangeGrob(
        y_label_grob,
        p,
        # add_border(y_label_grob, "blue"),
        # add_border(ggplotGrob(p), "red"),
        ncol = 2,
        widths = unit.c(
          unit(2, "cm"),
          unit(8, "cm"))
      )

      p <- gridExtra::arrangeGrob(p, ncol = 1)
      grid::grobTree(p, vp = viewport(gp = gpar(fill = 'transparent')))

      final_plot <- ggpubr::as_ggplot(p)
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

  }

  ## 2.2. apilada -----------------------------------------------------------

  else if (is.null(data) && is.list(vars) && display == 'stacked') {
    if (length(unique(tab_final$control)) > 1) {

      control_levels <- unique(tab_final$control)

      plots_list <- purrr::map(control_levels, function(ctrl) {
        df_ctrl <- tab_final %>% filter(control == ctrl)

        p <- ggplot2::ggplot(df_ctrl, aes(x = group, y = Freq, fill = Var1)) +
          geom_col(position = 'stack',
                   width = if (length(unique(df_ctrl$group)) == 1) 0.4 else 0.7 # ancho
                   ) +
          geom_text_repel(
            aes(label = paste0(janitor::round_half_up(Freq), '%')),
            position = ggpp::position_stacknudge(vjust = 0.5),
            size = if (output_type == "docx") 9*0.35 else 14*0.35,
            color = '#ffffff',
            family = 'Arial',
            fontface = 'bold',
            direction = 'x',
            point.size = NA,
            box.padding = 0) +
          coord_flip() +
          scale_fill_manual(values = colores) +
          scale_y_reverse() +
          scale_x_discrete(
            labels = NULL,
            expand =
              if (output_type == 'docx') c(0, 0)
            else if (length(unique(df_ctrl$group)) == 1) expansion(mult = c(0.5, 0.5))
            else expansion(mult = c(0.4, 0.4))) +
          theme_void() +
          theme(
            axis.ticks.y = element_blank(),
            legend.position = 'none',
            plot.margin = margin(t = if (output_type == 'docx') 0 else -10))

        if (length(unique(tab_final$group)) <= 1) {
          x_labels <- FALSE
        }

        p

      # Alinear los gráficos entre sí en eje vertical, compartiendo eje Y (izquierda)
      aligned <- cowplot::align_plots(plotlist = plots_list, align = 'v', axis = 'l')

      # plots_with_titles <- purrr::map2(control_levels, aligned, function(ctrl, aligned_plot) {
        text_label <- textGrob(
          ctrl,
          x = 0.4,
          gp = gpar(
            fontsize = if (output_type == "docx") 9 else 13,
            fontfamily = 'Arial',
            col = '#002060',
            fontface = 'bold',
            lineheight = 0.8
          )
        )

        # Etiquetas de eje Y como mini plot si x_labels = TRUE
        if (isTRUE(x_labels)) {
          label_df <- data.frame(group = rev(unique(df_ctrl$group)))

          y_label_plot <- ggplot(label_df, aes(x = 1, y = group)) +
            geom_text(aes(label = group),
                      hjust = 0.5,
                      color = "#002060",
                      size = if (output_type == "docx") 8 * 0.35 else 12 * 0.35,
                      family = "Arial",
                      fontface = if (output_type == "docx") 'bold' else 'plain') +
            scale_y_discrete(
              limits = rev(unique(df_ctrl$group)),
              expand = expansion(mult = c(0.4, 0.4))) +
            scale_x_discrete(
              expand = if (length(unique(df_ctrl$group)) <= 1)
                expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))) +
            theme_void() +
            theme(plot.margin = margin(
              t = if (length(unique(df_ctrl$group)) == 2) 0 else -10,
              b = if (length(unique(df_ctrl$group)) == 2) 0 else -10))

          y_label_grob <- ggplotGrob(y_label_plot)
        } else {
          y_label_grob <- grid::nullGrob()
        }

        # Combinar todo en arranged
        arranged <-
          gridExtra::arrangeGrob(
            text_label,
            y_label_grob,
            aligned_plot,
            grid::nullGrob(),
            ncol = 4,
            widths = unit.c(
              unit(if (output_type == "docx" &&
                       length(unique(tab_final$control)) == 1) 0.01    # Título
                   else if (output_type == "docx" &&
                            length(unique(tab_final$control)) > 1) 6.49
                   else if (length(vars) <= 1) 11.99
                   else 8.5, 'cm'),
              unit(if (output_type == "docx" &&
                       isTRUE(x_labels)) 3.49                          # Y labels
                   else if (length(vars) <= 1) 0.01
                   else 4, 'cm'),
              unit(if (output_type == "docx" &&
                       length(unique(tab_final$control)) == 1) 9.5       # Plot
                   else if (output_type == "docx" &&
                            length(unique(tab_final$control)) > 1) 6.5
                   else if (length(vars) <= 1) 17
                   else 17, 'cm'),
              unit(if (output_type == "docx") 2                         # T2B
                   else 2.5, 'cm')
            )
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
      plots_combined <- patchwork::wrap_plots(plots_list,
                                              ncol = 1,
                                              heights = n_barras_ajustado)
      plots_combined <- patchwork::wrap_plots(plots_combined,
                                              nrow = 1,
                                              heights = c(6))

      p <- plots_combined

    }
    else{
      tab_final$Var1 <- factor(tab_final$Var1, levels = rev(sort(unique(tab_final$Var1))))

      p <- ggplot2::ggplot(data = tab_final,
                           ggplot2::aes(x = group, y = Freq, fill = Var1)) +
        ggplot2::geom_col(
          position = 'stack',
          width = if (output_type == "docx" && length(unique(tab_final$group)) == 1) 1
          else if (length(unique(tab_final$group)) == 1) 0.5
          else 0.7
          ) +
        ggrepel::geom_text_repel(ggplot2::aes(label = paste0(janitor::round_half_up(Freq), '%')),
                                 position = ggpp::position_stacknudge(vjust = 0.5),
                                 size = if (output_type == "docx") 9*0.35 else 14*0.35,
                                 color = '#ffffff',
                                 family = 'Arial',
                                 fontface = 'bold',
                                 direction = 'x',
                                 point.size = NA,
                                 box.padding = 0) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = colores) +
        ggplot2::scale_x_discrete(labels = NULL,
                                  expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0, 100.1),
                                    expand = c(0, 0)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = 'none',
          # plot.margin = margin(
          #   t = if (output_type == "docx") 4
          #   else if (length(unique(tab_final$group)) == 2) 0 else -10,
          #   b = if (length(unique(tab_final$group)) == 2 |
          #           output_type == "docx") 0
          #   else -10)
          #
          plot.margin = margin(
            t = if (output_type == "docx") -3
            else if (length(unique(tab_final$group)) == 2) 0
            else -10,
            b = if (output_type == "docx") -3
            else if (length(unique(tab_final$group)) == 2) 0
            else -10)
        )

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

      p <- ggplotGrob(p)

      # Título del ítem
      text_label <- if (output_type == "docx" && length(unique(tab_final$control)) == 1) {
        grid::nullGrob()
      } else {
        grid::textGrob(
          ctrl,
          x = 1,
          just = 'right',
          gp = gpar(
            fontsize = if (output_type == "docx") 7
            else if (length(vars) <= 1) 12
            else 13,
            fontfamily = 'Arial',
            col = '#002060',
            fontface = if (output_type == "docx") 'bold'
            else if (isTRUE(x_labels)) 'bold'
            else 'plain',
            lineheight = 0.8
          )
        )
      }

      if (isTRUE(x_labels)) {
        label_df <- data.frame(group = rev(unique(tab_final$group)))

        y_label_plot <- ggplot(label_df, aes(x = 1, y = group)) +
          geom_text(aes(label = group),
                    hjust = 1,
                    color = "#002060",
                    size = if (output_type == "docx") 8*0.35 else 12*0.35,
                    fontface = if (output_type == "docx") 'bold' else 'plain',
                    family = "Arial") +
          scale_y_discrete(
            limits = rev(unique(tab_final$group)),
            # expand = if (length(tab_final$group) > 1) expansion(add = c(0.9, 0.7)) else expansion(add = c(0, 0))) +
            expand = if (length(tab_final$group) > 1) expansion(add = c(0.3, 0.3)) else expansion(add = c(0, 0))) +
            # expand = expansion(add = c(0.7, 0.8))) +
          scale_x_continuous(
            limits = c(0, 1),
            expand = expansion(mult = c(0.2, 0.2))) +
          coord_cartesian(clip = "off") +
          theme_void() +
          theme(plot.margin = margin(
            t = if (output_type == "docx") 0
            else if (length(unique(tab_final$group)) == 2) 0
            else -10,
            b = if (output_type == "docx") 0
            else if (length(unique(tab_final$group)) == 2) 0
            else -10)
          )
        y_label_grob <- ggplotGrob(y_label_plot)
      } else {
        y_label_grob <- grid::nullGrob()
      }

      p <-
        gridExtra::arrangeGrob(
          text_label,
          y_label_grob,
          p,
          grid::nullGrob(),
          # add_border(text_label, "blue"),
          # add_border(y_label_grob, "green"),
          # add_border(p, "orange"),
          # add_border(grid::nullGrob(), "purple"),
          ncol = 4,
          widths = unit.c(
            unit(if (output_type == "docx" &&
                     length(unique(tab_final$control)) == 1) 0.01    # Título
                 else if (output_type == "docx" &&
                          length(unique(tab_final$control)) > 1) 6.49
                 else if (length(vars) <= 1) 11.99
                 else 8.5, 'cm'),
            unit(if (output_type == "docx" &&
                     isTRUE(x_labels)) 3.49                          # Y labels
                 else if (length(vars) <= 1) 0.01
                 else 4, 'cm'),
            unit(if (output_type == "docx" &&
                     length(unique(tab_final$control)) == 1) 9.5       # Plot
                 else if (output_type == "docx" &&
                          length(unique(tab_final$control)) > 1) 6.5
                 else if (length(vars) <= 1) 17
                 else 17, 'cm'),
            unit(if (output_type == "docx") 2                         # T2B
                 else 2.5, 'cm')
          )
        )

      # Envolver para fondo transparente
      p <- gridExtra::arrangeGrob(p, ncol = 1)
      grobTree(p, vp = viewport(gp = gpar(fill = 'transparent')))
      p <- patchwork::wrap_plots(p,
                                 ncol = 1,
                                 heights = c(6))
    }

    # Añadir leyenda
    # Plot dummy para extraer leyenda
    tab_final$Var1 <- factor(tab_final$Var1, levels = rev(sort(unique(tab_final$Var1))))

    legend_plot <- ggplot2::ggplot(tab_final, ggplot2::aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(position = 'stack') +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = 'bottom',
        legend.title = ggplot2::element_blank(),
        legend.text = element_text(size = if (output_type == "docx") 7 else 10.5,
                                   color = '#002060',
                                   family = 'Arial'),
        legend.key.size = unit(if (output_type == "docx") 0.3 else 0.4, 'cm'),
        legend.margin = margin(l = 20,
                               t = 0,
                               b = if (output_type == "docx") 0 else 10)) +
        # legend.margin = ggplot2::margin(l = 20, b = 10)) +
      ggplot2::guides(shape = ggplot2::guide_legend(position = 'bottom'))

    legend_grob <- get_legend(legend_plot)

    legend_patch <- patchwork::wrap_elements(full = legend_grob) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))

    final_plot <- p / legend_patch +
      patchwork::plot_layout(ncol = 1,
                             heights = c(6, 1))

    # Aplicar tema general
    final_plot <- final_plot +
      patchwork::plot_annotation(
        theme = ggplot2::theme(
          plot.margin = ggplot2::margin(t = if (output_type == "docx") 0 else 100,
                                        b = if (output_type == "docx") 0 else -15))) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    final_plot <- patchwork::wrap_elements(panel = final_plot) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
  }

  # 3. Añadir notal al pie --------------------------------------------------

  if (output_type == "docx") {
    show_notes <- FALSE
  }

  if (!is.null(data)) {
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
  }

  if(isTRUE(show_notes)) {
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
        ggplot2::aes(x = -0.06, y = 0,
                     label = stringr::str_wrap(paste("Base:",
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

  ## Título automático

  # Generar título automático solo si no se ha especificado manualmente
  if (is.null(data) && is.null(title)) {
    labels_vars <- vars %>%
      purrr::imap(function(var_list, publico) {
        purrr::map_chr(var_list, ~ attr(get(publico)[[.x]], "label") %||% NA_character_)
      }) %>%
      unlist(use.names = FALSE)

    labels_validos <- labels_vars[!is.na(labels_vars)]

    if (length(labels_validos) > 0 && length(unique(labels_validos)) == 1) {
      title <- unique(labels_validos)
    } else {
      title <- "Título"
    }
  }

  if (output_type == "docx") {
    if (display == 'simple') {
      n_columnas <- length(unique(tab_final[[rlang::as_name(rlang::ensym(vars))]]))
    }
    if (display == 'stacked') {
      n_columnas <- max(length(unique(tab_final$control)), length(unique(tab_final$group)))
      }

    return(list(
      plot = p_fixed,
      cap =
        if (!is.null(title) && is.character(title))
          if (!is.null(data))
            paste0(title, ", ", unit)
      else title,
      N = paste0("N = ",
                 if (exists("n_totales")) n_totales else "",
                 if (exists("n_total")) n_total else "",
                 if (!is.null(unit)) paste0(' ', unit) else ""
      ),
      height =
        if (display == 'simple') 0.003333*n_columnas^2+0.27167*n_columnas+0.54
      else if (display == 'stacked') 0.01*n_columnas^2+0.55*n_columnas+0.06
    )
    )

  } else if (output_type == "pptx") {
    return(p_fixed)
  }
}
