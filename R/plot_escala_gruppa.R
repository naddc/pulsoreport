#' Barras horizontales apiladas para preguntas escalares y comparativas entre grupos
#'
#' Genera un gráfico de barras horizontales apiladas para preguntas escalares y comparativas entre grupos, para PowerPoint. Requiere distintas bases para cada grupo definido como lista en `vars`.
#' @import ggplot2
#' @import ggh4x
#' @import patchwork
#' @import ggpubr
#' @import stringr
#' @import grid
#' @param vars Variable(s) para plotear.
#' @param T2B Define si se calcula e incluye en el gráfico el "Top 2 Box" (TRUE por defecto).
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export
plot_escala_gruppa <- function(vars, T2B = TRUE, unit_extra = TRUE) {

  tablas <- list()
  etiquetas <- list()
  totales <- list()
  if (isTRUE(T2B)) {
    T2B_df <- tibble(Top2B = character(), group = character())
  }
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


      ## 2.2. Calcular Top2Box ----
      if (isTRUE(T2B)) {
        T2B_calc <- dataset %>%
          dplyr::filter(!is.na(.[[var_name]])) %>% # cambio para filtrar NA
          dplyr::mutate(top2 = (.[[var_name]] %in% c(4, 3))) %>%
          dplyr::summarise(top2_pct = round((sum(top2, na.rm = TRUE) / n()) * 100)) %>%
          dplyr::pull(top2_pct)

        T2B_df <- dplyr::bind_rows(T2B_df,
                            tibble(
                              control = etiqueta,
                              Top2B = paste0(T2B_calc, '%'),
                              group = str_replace(params[[paste0(nombre_data, '_unit')]],'^\\w{1}', toupper)
                            ))
      }

    }

    # 3. Calcular Ns
    n <- dataset %>%
      select(all_of(var_name)) %>%
      dplyr::mutate(across(everything(), as.character)) %>%
      filter(rowSums(!is.na(.) & . != '') > 0) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::pull(total)

    totales[[params[[paste0(nombre_data, '_unit')]]]] <- n

  }

  tab_final <- do.call(rbind, tablas) %>%
    filter(Freq != 0)

  n_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')

  # 4. Crear un vector de colores basado en los levels presentes en los datos
  nivel_actual <- levels(tab_final$Var1)

  if (length(nivel_actual) > 2) {
  color_map <- c('#F4B183',
                 '#FFD965',
                 '#ADD493',
                 '#70AD47',
                 '#A5A5A5',
                 rep('#D9D9D9', length(nivel_actual) - 5))
  names(color_map) <- nivel_actual
  colores <- c(colores, color_map)
  } else if (length(nivel_actual) == 2) {

  }

  colores <- colores[unique(tab_final$Var1)]


  if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
    n_totales <- paste(n_totales, params[['unit_extra']])
  }

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

  # Ordenar T2B_df
  if (exists('T2B_df')) {
    T2B_df$group <- factor(T2B_df$group, levels = orden_grupos)
    T2B_df$control <- str_wrap(T2B_df$control, width = 30)
    T2B_df$control <- factor(T2B_df$control, levels = orden_control_wrapped)
  }

  if (length(unique(tab_final$control)) > 1) {

    control_levels <- unique(tab_final$control)

    plots_list <- purrr::map(control_levels, function(ctrl) {
      df_ctrl <- tab_final %>% filter(control == ctrl)

      p <- ggplot(df_ctrl, aes(x = group, y = Freq, fill = Var1)) +
        geom_col(position = 'stack', width = if (length(unique(df_ctrl$group)) == 1) 0.4 else 0.6) +
        geom_text_repel(
          aes(label = paste0(round(Freq), '%')),
          position = ggpp::position_stacknudge(vjust = 0.5),
          size = 4.93,
          color = '#002060',
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
          axis.text.y = element_text(
            size = 12,
            margin = margin(r = 10),
            family = 'Arial',
            color = '#002060'),
          axis.ticks.y = element_blank(),
          legend.position = 'none',
          plot.margin = margin(t = -10)
        )

    })

    # Alinear los gráficos entre sí en eje vertical, compartiendo eje Y (izquierda)
    aligned <- cowplot::align_plots(plotlist = plots_list, align = 'v', axis = 'l')

    plots_with_titles <- purrr::map2(control_levels, aligned, function(ctrl, aligned_plot) {
      text_label <- textGrob(
        ctrl,
        gp = gpar(
          fontsize = 13,
          fontfamily = 'Arial',
          col = '#002060',
          fontface = 'bold'
        )
      )

      # Crear panel derecho T2B solo si se desea
      if (isTRUE(T2B)) {
        df_T2B_ctrl <- T2B_df %>% filter(control == ctrl)
        panel_t2b <- ggplotGrob(
          ggplot(df_T2B_ctrl, aes(x = 1, y = group)) +
            geom_text(aes(label = Top2B),
                      hjust = 0.5,
                      color = '#548135',
                      size = 4.55,
                      fontface = 'bold',
                      family = 'Arial') +
            scale_y_discrete(
              limits = rev(unique(df_T2B_ctrl$group)),
              expand = expansion(mult = c(0.4, 0.4))
            ) +
            scale_x_discrete(
              expand =  if (length(unique(df_T2B_ctrl$group)) == 1) expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))
            ) +
            theme_void() +
            theme(plot.margin = margin(t = -10))
        )
      }

      # # Función para envolver cualquier grob con un borde visible
      # add_border <- function(grob, color = 'red') {
      #   grobTree(
      #     rectGrob(gp = gpar(col = color, fill = NA, lwd = 1)),
      #     grob
      #   )
      # }
      #
      # # Aplicar a tus grobs
      # text_label_b <- add_border(text_label, 'red')
      # aligned_plot_b <- add_border(aligned_plot, 'blue')
      # panel_t2b_b <- add_border(panel_t2b, 'green')

      # Combinar todo en arranged
      arranged <- if (isTRUE(T2B)) {
        arrangeGrob(
          text_label, aligned_plot, panel_t2b,
          ncol = 3,
          widths = unit.c(unit(10, 'cm'), unit(19.5, 'cm'), unit(2.5, 'cm'))
        )
      } else {
        arrangeGrob(
          text_label, aligned_plot,
          ncol = 2,
          widths = unit.c(unit(9, 'cm'), unit(20, 'cm'))
        )
      }

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
    plots_combined <- wrap_plots(plots_with_titles, ncol = 1, heights = n_barras_ajustado)

    col_titles <- arrangeGrob(
      nullGrob(), nullGrob(),
      textGrob('TOP TWO BOX', gp = gpar(
        fontsize = 13, fontface = 'bold', col = '#548135', fontfamily = 'Arial'
      )),
      ncol = 3,
      widths = unit.c(unit(10, 'cm'), unit(19.5, 'cm'), unit(2.5, 'cm'))
    )
    plot_unified <- wrap_plots(
      col_titles,
      plots_combined,
      nrow = 2,
      heights = c(0.1, 6)
    )
  }

  else {
    p <- ggplot2::ggplot(data = tab_final, aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(position = 'stack', width = if (length(unique(tab_final$group)) < 2) 0.3 else 0.4) +
      ggrepel::geom_text_repel(aes(label = paste0(round(Freq), '%')),
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
      ggh4x::force_panelsizes(total_width = unit(9, 'in'), total_height = unit(4.5, 'in')) +
      ggplot2::theme(
        axis.text.y = element_text(size = 12,
                                   margin = margin(r = 20)),
        text = element_text(color = '#002060',
                            family = 'Arial'),
        legend.position = 'none') +
        # panel.background = element_rect(fill = 'transparent', colour = NA),
        # plot.background = element_rect(fill = 'transparent', colour = NA)) +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::scale_x_discrete(labels = function(x) str_wrap(x, width = 60))

    p_grob <- ggplotGrob(p)
    p_grob$widths[[which(p_grob$layout$name == "axis-l")]] <- unit(1, "cm")
    p <- as_ggplot(p_grob)

    if (isTRUE(T2B)) {
      # Panel con etiquetas Top Two Box
      panel_t2b <- ggplotGrob(
        ggplot(T2B_df, aes(x = 1, y = group)) +
          geom_text(aes(label = Top2B),
                    hjust = 0.5,
                    color = '#548135',
                    size = 4.55,
                    fontface = 'bold',
                    family = 'Arial') +
          scale_y_discrete(
            limits = rev(unique(T2B_df$group)),
            expand = expansion(mult = c(0.4, 0.4))
          ) +
          scale_x_discrete(
            expand =  if (length(unique(T2B_df$group)) == 1) expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))
          ) +
          theme_void() +
          theme(plot.margin = margin(t = -10))
      )

      # Título de la parte superior
      col_titles <- arrangeGrob(
        nullGrob(),
        textGrob('TOP TWO BOX', gp = gpar(
          fontsize = 13, fontface = 'bold', col = '#548135', fontfamily = 'Arial'
        )),
        ncol = 2,
        widths = unit.c(unit(29.5, 'cm'), unit(2.5, 'cm'))
      )

      arranged <- arrangeGrob(
        p, panel_t2b,
        ncol = 2,
        widths = unit.c(unit(29.5, 'cm'), unit(2.5, 'cm'))
      )

      grobTree(arranged, vp = viewport(gp = gpar(fill = 'transparent')))

      plot_unified <- wrap_plots(
        col_titles,
        arranged,
        nrow = 2,
        heights = c(0.1, 6)
      )
    }

  }

  # solución de @teunbrand
  get_legend <- function(plot, legend = NULL) {

    gt <- ggplotGrob(plot)

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
  legend_plot <- ggplot(tab_final, aes(x = group, y = Freq, fill = Var1)) +
    geom_col(position = 'stack') +
    coord_flip() +
    scale_fill_manual(values = colores) +
    scale_y_reverse() +
    scale_x_discrete(
      expand = expansion(mult = c(0.5, 0.5))
    ) +
    theme_void() +
    theme(
      legend.position = 'bottom',
      legend.title = element_blank(),
      legend.text = element_text(size = 10.5, color = '#002060', family = 'Arial'),
      legend.key.size = unit(0.4, 'cm'),
      legend.margin = margin(l = 20, b = 10)
    ) +
    guides(shape = guide_legend(position = 'bottom'))

  legend_grob <- get_legend(legend_plot)

  legend_patch <- patchwork::wrap_elements(full = legend_grob)

  final_plot <- plot_unified / legend_patch +
    plot_layout(heights = c(5, 1))

  # Aplicar tema general
  final_plot <- final_plot +
    plot_annotation(
      theme = theme(
        plot.margin = margin(t = 100, b = -15)
      ))

  # Texto izquierdo (con lineheight)
  footer_izq <- ggplot() +
    geom_text(
      aes(x = -0.04, y = 0, label = str_wrap(paste("Base:", n_totales), width = 67)),
      hjust = 0, family = "Arial", size = 3.5, fontface = "bold",
      color = "#002060", lineheight = 0.85
    ) +
    coord_cartesian(xlim = c(0, 1), clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(t = 0, r = 0, b = 12, l = -1)
    )

  # Texto derecho (con lineheight)
  footer_der <- ggplot() +
    geom_text(
      aes(x = 1.04, y = 0, label = "Los porcentajes están redondeados y pueden no sumar 100%"),
      hjust = 1, family = "Arial", size = 3.5, fontface = "bold",
      color = "#002060", lineheight = 0.85
    ) +
    coord_cartesian(xlim = c(0, 1), clip = "off") +
    theme_void() +
    theme(
      plot.margin = margin(t = 0, r = -1, b = 12, l = 0)
    )

  # Footer en dos columnas usando patchwork
  footer_plot <- footer_izq + footer_der + plot_layout(ncol = 2, widths = c(5, 10))

  final_plot <- final_plot / footer_plot +
    plot_layout(heights = c(5, 1, 0.4))

  p_fixed <- final_plot &
    theme(
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.background = element_rect(fill = 'transparent', color = NA)
    )

  return(p_fixed)

}
