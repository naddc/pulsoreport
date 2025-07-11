#' Barras horizontales apiladas para preguntas escalares y comparativas entre grupos
#'
#' Genera un gráfico de barras horizontales apiladas para preguntas escalares y comparativas entre grupos, para PowerPoint. Requiere distintas bases para cada grupo definido como lista en `vars`.
#' @import ggplot2
#' @import ggh4x
#' @import ggpubr
#' @import patchwork
#' @import stringr
#' @import grid
#' @param vars Variable(s) para plotear.
#' @param T2B Define si se calcula e incluye en el gráfico el "Top 2 Box" (TRUE por defecto).
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export

plot_gruppa <- function(vars,
                        ancho = 0.7,
                        T2B = TRUE,
                        unit_extra = TRUE,
                        show_notes = TRUE,
                        x_labels = TRUE) {

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
      FUN.VALUE = logical(1))
    indices <- indices[not_empty]
    if (length(indices) > 0) {
      return(gt$grobs[[indices[1]]])
    }
    return(NULL)
  }
  # 0. Clasificar las variables por tipo (dicotómica o escalar) ----
  clasificacion_vars <- vars %>%
    purrr::imap(function(var_list, publico) {
      var_list %>%
        purrr::map_chr(~ {
          n_labels <- length(attr(get(publico)[[.x]], "labels"))
          if (n_labels < 5) "dicotomica" else "escalar"
        }) %>%
        purrr::set_names(var_list)
    })
  ## Separar por tipo
  vars_dicotomicas <- clasificacion_vars %>%
    purrr::imap(~ names(.x)[.x == "dicotomica"]) %>%
    purrr::keep(~ length(.x) > 0)
  vars_escalares <- clasificacion_vars %>%
    purrr::imap(~ names(.x)[.x == "escalar"]) %>%
    purrr::keep(~ length(.x) > 0)
  # 1. Generar los gráficos de variables dicotómicas si hay alguna ----------
  p_dicotomicas <- NULL
  if (length(vars_dicotomicas) > 0) {
    ## Tabular y calcular Ns ----
    tablas <- list()
    etiquetas <- list()
    n_barras <- list()
    totales <- list()
    colores <- c()
    for (nombre_data in names(vars_dicotomicas)) {
      dataset <- get(nombre_data)  # Obtener el dataset por su nombre
      var_name <- vars_dicotomicas[[nombre_data]]  # Obtener el nombre de la variable dentro del dataset
      for (var_name in vars_dicotomicas[[nombre_data]]) {  # Iterar sobre todas las variables en el dataset
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
        dplyr::select(all_of(var_name)) %>%
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
    # Crear vector ordenado de públicos según orden de vars_dicotomicas y params
    orden_grupos <- sapply(names(vars_dicotomicas), function(nombre_data) {
      unidad_param <- paste0(nombre_data, '_unit')
      if (unidad_param %in% names(params)) {
        stringr::str_replace(params[[unidad_param]], '^\\w{1}', toupper)
      } else {
        'Participantes'
      }
    }) %>% rev(.)
    # Crear vector ordenado de enunciados según primeras variables en primeros elementos en list 'vars_dicotomicas'
    orden_control <- c()
    for (nombre_data in names(vars_dicotomicas)) {
      dataset <- get(nombre_data)
      for (var_name in vars_dicotomicas[[nombre_data]]) {
        if (var_name %in% names(dataset)) {
          etiqueta <- attributes(dataset[[var_name]])$label
          if (!is.null(etiqueta) && !(etiqueta %in% orden_control)) {
            orden_control <- c(orden_control, etiqueta)
          }
        }
      }
    }
    orden_control_wrapped <- str_wrap(orden_control, width = 35)
    ### Ordenar tab_final
    tab_final$group <- factor(tab_final$group, levels = orden_grupos)
    tab_final$control <- str_wrap(tab_final$control, width = 35)
    tab_final$control <- factor(tab_final$control, levels = orden_control_wrapped)
    tab_final
    ## Plotear ----
    if (length(unique(tab_final$group)) <= 1) {
      x_labels <- FALSE
    }
    if (length(unique(tab_final$control)) > 0) {
      control_levels <- unique(tab_final$control)
      plots_list <- purrr::map(control_levels, function(ctrl) {
        df_ctrl <- tab_final %>% filter(control == ctrl)
        if (length(unique(tab_final$group)) <= 1) {
          x_labels <- FALSE
        }
        p <- ggplot(df_ctrl, aes(x = group, y = Freq, fill = Var1)) +
          ggplot2::geom_col(position = 'stack', width = if (length(unique(df_ctrl$group)) == 1) 0.4 else ancho) +
          ggrepel::geom_text_repel(
            aes(label = paste0(janitor::round_half_up(Freq), '%')),
            position = ggpp::position_stacknudge(vjust = 0.5),
            size = 4.93,
            color = '#ffffff',
            family = 'Arial',
            fontface = 'bold',
            direction = 'x',
            point.size = NA,
            box.padding = 0) +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_manual(values = colores) +
          ggplot2::scale_y_reverse() +
          ggplot2::scale_x_discrete(
            labels = function(x) str_wrap(x, width = 60),
            expand =  if (length(unique(df_ctrl$group)) == 1) expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))) +
          ggplot2::theme_void() +
          ggplot2::theme(
            axis.ticks.y = element_blank(),
            legend.position = 'none',
            plot.margin = margin(t = -10))
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
      ### Alinear los gráficos entre sí en eje vertical, compartiendo eje Y (izquierda)
      aligned <- cowplot::align_plots(plotlist = plots_list, align = 'v', axis = 'l')
      plots_with_titles <- purrr::map2(control_levels, aligned, function(ctrl, aligned_plot) {
        text_label <- grid::textGrob(
          ctrl,
          x = 0.45,
          gp = gpar(
            fontsize = 13,
            fontfamily = 'Arial',
            col = '#002060',
            fontface = if (isTRUE(x_labels)) 'bold' else NULL,
            lineheight = 0.8))
        #### Combinar todo en arranged
        arranged <- if (isTRUE(T2B)) {
          gridExtra::arrangeGrob(
            text_label, aligned_plot, grid::nullGrob(),
            ncol = 3,
            widths = unit.c(unit(10, 'cm'), unit(19.5, 'cm'), unit(2.5, 'cm'))
          )
        }
        else {
          arranged <- gridExtra::arrangeGrob(
            text_label, aligned_plot,
            ncol = 2,
            widths = unit.c(unit(9, 'cm'), unit(20, 'cm'))
          )
        }
        arranged <- gridExtra::arrangeGrob(arranged, ncol = 1)
        #### Envolver para fondo transparente
        grid::grobTree(arranged, vp = viewport(gp = gpar(fill = 'transparent')))
      })
      ### Juntar verticalmente todos los gráficos combinados
      n_barras <- sapply(control_levels, function(ctrl) {
        length(unique(tab_final$group[tab_final$control == ctrl]))
      })
      n_barras_ajustado <- sapply(n_barras, function(n) {
        if (n <= 1) 1 else 1 + (n - 2)^1.2
      })
      n_barras_ajustado <- n_barras_ajustado / sum(n_barras_ajustado) * 6
      n_barras_ajustado <- pmin(n_barras_ajustado, 5)
      ### Combinar todos los gráficos
      plots_combined <- patchwork::wrap_plots(plots_with_titles,
                                              ncol = 1,
                                              heights = n_barras_ajustado)
      plots_combined <- patchwork::wrap_plots(plots_combined,
                                              nrow = 1,
                                              heights = c(6))
      p <- plots_combined &
        ggplot2::theme(
          plot.margin = margin(t = 0, b = -50),
          plot.background = element_rect(fill = 'transparent', color = NA),
          panel.background = element_rect(fill = 'transparent', color = NA))
    }
    ## Añadir leyenda: solución de @teunbrand ----
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
        legend.margin = ggplot2::margin(l = 20, b = 0)
      ) +
      ggplot2::guides(shape = ggplot2::guide_legend(position = 'bottom'))
    legend_grob <- get_legend(legend_plot)
    legend_patch <- patchwork::wrap_elements(full = legend_grob) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    final_plot <- p / legend_patch +
      plot_layout(ncol = 1,
                  heights = c(5, 0.1))
    ## Aplicar tema general ----
    final_plot <- final_plot +
      patchwork::plot_annotation(
        theme = ggplot2::theme(
          plot.margin = ggplot2::margin(b = -15))) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    p_dicotomicas <- patchwork::wrap_elements(panel = final_plot) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
  }
  # 2. Generar los gráficos de variables escalares si hay alguna ------------
  p_escalares <- NULL
  if (length(vars_escalares) > 0) {
    ## Tabular y calcular Ns ----
    tablas <- list()
    etiquetas <- list()
    totales <- list()
    colores <- c()
    if (isTRUE(T2B)) {
      T2B_df <- tibble(Top2B = character(), group = character())
    }
    for (nombre_data in names(vars_escalares)) {
      dataset <- get(nombre_data)  # Obtener el dataset por su nombre
      var_name <- vars_escalares[[nombre_data]]  # Obtener el nombre de la variable dentro del dataset
      for (var_name in vars_escalares[[nombre_data]]) {  # Iterar sobre todas las variables en el dataset
        if (!var_name %in% names(dataset)) next  # Saltar si la variable no está en el dataset
        etiqueta <- attributes(dataset[[var_name]])$label  # Obtener la etiqueta de la variable
        #### Crear tabla de proporciones
        tab <- as.data.frame(prop.table(table(sjlabelled::as_label(dataset[[var_name]]))) * 100)
        ### Obtener niveles de la variable si tiene etiquetas
        levels <- names(attr(dataset[[var_name]], 'labels'))
        ### Construir tabla con la etiqueta y los niveles ordenados
        tab <- tab %>%
          dplyr::mutate(control = etiqueta,
                        group = if (paste0(nombre_data, '_unit') %in% names(params)) {
                          stringr::str_replace(params[[paste0(nombre_data, '_unit')]],'^\\w{1}', toupper)
                        } else {
                          'Participantes'
                        },
                        Var1 = factor(Var1, levels = levels)) %>%
          dplyr::arrange(Var1)
        ### Guardar en la lista con identificador único
        tablas[[paste(nombre_data, var_name, sep = '_')]] <- tab
        etiquetas[[paste(nombre_data, var_name, sep = '_')]] <- etiqueta
        ### Calcular Top2Box
        if (isTRUE(T2B)) {
          T2B_calc <- dataset %>%
            dplyr::filter(!is.na(.[[var_name]])) %>% # cambio para filtrar NA
            dplyr::mutate(top2 = (.[[var_name]] %in% c(4, 3))) %>%
            dplyr::summarise(top2_pct = janitor::round_half_up((sum(top2, na.rm = TRUE) / dplyr::n()) * 100)) %>%
            dplyr::pull(top2_pct)
          T2B_df <- dplyr::bind_rows(T2B_df,
                                     tibble(
                                       control = etiqueta,
                                       Top2B = paste0(T2B_calc, '%'),
                                       group = str_replace(params[[paste0(nombre_data, '_unit')]],'^\\w{1}', toupper)))
        }
      }
      ### Calcular Ns
      n <- dataset %>%
        select(all_of(var_name)) %>%
        dplyr::mutate(across(everything(), as.character)) %>%
        filter(rowSums(!is.na(.) & . != '') > 0) %>%
        dplyr::summarise(total = dplyr::n()) %>%
        dplyr::pull(total)
      totales[[params[[paste0(nombre_data, '_unit')]]]] <- n
    }
    tab_final <- do.call(rbind, tablas) %>%
      filter(Freq != 0)
    n_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')
    ### Crear un vector de colores basado en los levels presentes en los datos
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
    }
    colores <- colores[unique(tab_final$Var1)]
    if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
      n_totales <- paste(n_totales, params[['unit_extra']])
    }
    ### Ordenar según group (públicos) y control (enunciados)
    #### Crear vector ordenado de públicos según orden de vars_escalares y params
    orden_grupos <- sapply(names(vars_escalares), function(nombre_data) {
      unidad_param <- paste0(nombre_data, '_unit')
      if (unidad_param %in% names(params)) {
        stringr::str_replace(params[[unidad_param]], '^\\w{1}', toupper)
      } else {
        'Participantes'
      }
    }) %>% rev(.)
    #### Crear vector ordenado de enunciados según primeras variables en primeros elementos en list 'vars_escalares'
    orden_control <- c()
    for (nombre_data in names(vars_escalares)) {
      dataset <- get(nombre_data)
      for (var_name in vars_escalares[[nombre_data]]) {
        if (var_name %in% names(dataset)) {
          etiqueta <- attributes(dataset[[var_name]])$label
          if (!is.null(etiqueta) && !(etiqueta %in% orden_control)) {
            orden_control <- c(orden_control, etiqueta)
          }
        }
      }
    }
    orden_control_wrapped <- str_wrap(orden_control, width = 35)
    #### Ordenar tab_final
    tab_final$group <- factor(tab_final$group, levels = orden_grupos)
    tab_final$control <- str_wrap(tab_final$control, width = 35)
    tab_final$control <- factor(tab_final$control, levels = orden_control_wrapped)
    #### Ordenar T2B_df
    if (exists('T2B_df')) {
      T2B_df$group <- factor(T2B_df$group, levels = orden_grupos)
      T2B_df$control <- str_wrap(T2B_df$control, width = 35)
      T2B_df$control <- factor(T2B_df$control, levels = orden_control_wrapped)
    }
    ## Plotear ----
    if (length(unique(tab_final$control)) > 0) {
      control_levels <- unique(tab_final$control)
      plots_list <- purrr::map(control_levels, function(ctrl) {
        df_ctrl <- tab_final %>% filter(control == ctrl)
        if (length(unique(tab_final$group)) <= 1) {
          x_labels <- FALSE
        }
        p <- ggplot2::ggplot(df_ctrl, aes(x = group, y = Freq, fill = Var1)) +
          ggplot2::geom_col(position = 'stack', width = if (length(unique(df_ctrl$group)) == 1) 0.5 else ancho) +
          ggrepel::geom_text_repel(
            aes(label = paste0(janitor::round_half_up(Freq), '%')),
            position = ggpp::position_stacknudge(vjust = 0.5),
            size = 4.93,
            color = '#002060',
            family = 'Arial',
            fontface = 'bold',
            direction = 'x',
            point.size = NA,
            box.padding = 0) +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_manual(values = colores) +
          ggplot2::scale_y_reverse() +
          ggplot2::scale_x_discrete(
            labels = function(x) str_wrap(x, width = 60),
            expand =  if (length(unique(df_ctrl$group)) <= 1) expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))) +
          ggplot2::theme_void() +
          ggplot2::theme(
            axis.ticks.y = element_blank(),
            legend.position = 'none',
            plot.margin = margin(
              t = if (length(unique(df_ctrl$group)) == 2) 0 else -10,
              b = if (length(unique(df_ctrl$group)) == 2) 0 else -10))
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
      ### Alinear los gráficos entre sí en eje vertical, compartiendo eje Y (izquierda)
      aligned <- cowplot::align_plots(plotlist = plots_list, align = 'v', axis = 'l')
      plots_with_titles <- purrr::map2(control_levels, aligned, function(ctrl, aligned_plot) {
        text_label <- grid::textGrob(
          ctrl,
          x = 0.45,
          gp = gpar(
            fontsize = 13,
            fontfamily = 'Arial',
            col = '#002060',
            fontface = if (isTRUE(x_labels)) 'bold' else 'plain',
            lineheight = 0.8))
        #### Crear panel derecho T2B solo si se desea
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
                expand = expansion(mult = c(0.4, 0.4))) +
              ggplot2::scale_x_discrete(
                expand =  if (length(unique(df_T2B_ctrl$group)) <= 1) expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))) +
              ggplot2::theme_void()  +
              ggplot2::theme(plot.margin = margin(
                t = if (length(unique(df_T2B_ctrl$group)) == 2) 0 else -10,
                b = if (length(unique(df_T2B_ctrl$group)) == 2) 0 else -10)))
        }
        arranged <- if (isTRUE(T2B)) {
          gridExtra::arrangeGrob(
            text_label, aligned_plot, panel_t2b,
            ncol = 3,
            widths = unit.c(unit(10, 'cm'), unit(19.5, 'cm'), unit(2.5, 'cm')))
        }
        else {
          gridExtra::arrangeGrob(
            text_label, aligned_plot,
            ncol = 2,
            widths = unit.c(unit(9, 'cm'), unit(20, 'cm')))
        }
        arranged <- gridExtra::arrangeGrob(arranged, ncol = 1)
        #### Envolver para fondo transparente
        grid::grobTree(arranged, vp = viewport(gp = gpar(fill = 'transparent')))
      })

      # Calcular cantidad de barras por facet ----
      n_barras <- sapply(control_levels, function(ctrl) {
        length(unique(tab_final$group[tab_final$control == ctrl]))
      })
      n_barras_ajustado <- sapply(n_barras, function(n) {
        if (n <= 1) 1 else 1 + (n - 2)^1.2
      })

      # Intercalar plots con spacer (sin argumento)
      plots_intercalados <- purrr::flatten(
        purrr::map2(plots_with_titles, n_barras_ajustado, function(p, h) {
          list(p, patchwork::plot_spacer())
        })
      )
      plots_intercalados <- plots_intercalados[-length(plots_intercalados)]  # quitar último spacer

      # Intercalar heights (usando altura fija para los spacers)
      heights_intercalado <- purrr::flatten_dbl(
        purrr::map(n_barras_ajustado, function(h) c(h, 0.2))
      )
      heights_intercalado <- heights_intercalado[-length(heights_intercalado)]  # quitar altura del último spacer

      # Escalar a total deseado (ej. 6)
      heights_intercalado <- heights_intercalado / sum(heights_intercalado) * 6
      total_alto_contenido <- sum(heights_intercalado)

      # Combinar los gráficos
      plots_combined <- patchwork::wrap_plots(plots_intercalados, ncol = 1, heights = heights_intercalado)
      p <- plots_combined

      #### Juntar verticalmente todos los gráficos combinados
      # n_barras <- sapply(control_levels, function(ctrl) {
      #   length(unique(tab_final$group[tab_final$control == ctrl]))
      # })
      # n_barras_ajustado <- sapply(n_barras, function(n) {
      #   if (n <= 1) 1 else 1 + (n - 2)^1.2
      # })
      # n_barras_ajustado <- n_barras_ajustado / sum(n_barras_ajustado) * 6
      # n_barras_ajustado <- pmin(n_barras_ajustado, 5)
      # total_alto_contenido <- sum(n_barras_ajustado)
      #### Combinar todos los gráficos
      # plots_combined <- patchwork::wrap_plots(plots_with_titles,
      #                                         ncol = 1,
      #                                         heights = n_barras_ajustado)
      # plots_combined <- patchwork::wrap_plots(plots_combined,
      #                                         nrow = 1,
      #                                         heights = c(6))
      p <- plots_combined
      if (isTRUE(T2B)) {
        col_titles <- arrangeGrob(
          nullGrob(),
          textGrob('TOP TWO BOX', gp = gpar(
            fontsize = 13, fontface = 'bold', col = '#548135', fontfamily = 'Arial')),
          ncol = 2,
          widths = unit.c(unit(29.5, 'cm'), unit(2.5, 'cm')))
        p <- patchwork::wrap_plots(
          col_titles,
          p,
          ncol = 1,
          heights = c(0.1, 6))
        p <- patchwork::wrap_plots(p,
                                   nrow = 1,
                                   heights = c(6.1)) &
          ggplot2::theme(
            plot.background = element_rect(fill = 'transparent', color = NA),
            panel.background = element_rect(fill = 'transparent', color = NA))
      }
    }
    ## Añadir leyenda: solución de @teunbrand ----
    ### Plot dummy para extraer leyenda
    legend_plot <- ggplot2::ggplot(tab_final, aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(position = 'stack') +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_x_discrete(
        expand = expansion(mult = c(0.5, 0.5))) +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 10.5, color = '#002060', family = 'Arial'),
        legend.key.size = unit(0.4, 'cm'),
        legend.margin = margin(l = 20, b = 10)) +
      guides(shape = guide_legend(position = 'bottom'))
    legend_grob <- get_legend(legend_plot)
    legend_patch <- patchwork::wrap_elements(full = legend_grob) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    final_plot <- p / legend_patch +
      plot_layout(ncol = 1,
                  heights = c(total_alto_contenido, 1))
    ## Aplicar tema general ----
    final_plot <- final_plot +
      patchwork::plot_annotation(
        theme = ggplot2::theme(
          plot.margin = ggplot2::margin(b = -15))) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    p_escalares <- patchwork::wrap_elements(panel = final_plot) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
  }
  # 3. Calcular Ns ---------
  totales <- list()
  for (nombre_data in names(vars)) {
    dataset <- get(nombre_data)  # Obtener el dataset por su nombre
    var_name <- vars[[nombre_data]]
    ### Calcular Ns
    n <- dataset %>%
      select(all_of(var_name)) %>%
      dplyr::mutate(across(everything(), as.character)) %>%
      filter(rowSums(!is.na(.) & . != '') > 0) %>%
      dplyr::summarise(total = dplyr::n()) %>%
      dplyr::pull(total)
    totales[[params[[paste0(nombre_data, '_unit')]]]] <- n
    n_totales_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')
  }
  if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
    n_totales_totales <- paste(n_totales_totales, params[['unit_extra']])
  }
  # 4. Unir gráficos de variables dicotómicas y escalares ------------
  if (!is.null(p_dicotomicas) && !is.null(p_escalares)) {
    final_plot <- (p_dicotomicas / p_escalares) +
      patchwork::plot_layout(heights = c(4, 6)) &
      # patchwork::plot_annotation(
      #   theme = ggplot2::theme(
      #     plot.margin = ggplot2::margin(b = 100))) &
      ggplot2::theme(
        plot.margin = ggplot2::margin(b = 19),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
  } else if (!is.null(p_dicotomicas)) {
    final_plot <- p_dicotomicas
  } else if (!is.null(p_escalares)) {
    final_plot <- p_escalares
  } else {
    stop("No se encontraron variables para graficar")
  }
  ## Aplicar tema general ----
  final_plot <- final_plot +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        plot.margin = ggplot2::margin(t = 100))) &
    ggplot2::theme(
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.background = element_rect(fill = 'transparent', color = NA))
  final_plot <- patchwork::wrap_elements(panel = final_plot) &
    ggplot2::theme(
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.background = element_rect(fill = 'transparent', color = NA))
  if (isTRUE(show_notes)) {
    ## Texto izquierdo (con lineheight) ----
    footer_izq <- ggplot() +
      geom_text(
        aes(x = -0.06, y = 0, label = str_wrap(paste("Base:", n_totales_totales), width = 67)),
        hjust = 0, family = "Arial", size = 3.5, fontface = "bold",
        color = "#002060", lineheight = 0.85) +
      coord_cartesian(xlim = c(0, 1), clip = "off") +
      ggplot2::theme_void() +
      theme(
        plot.margin = margin(t = 0, r = 0, b = 12, l = -1))
    ## Texto derecho (con lineheight) ----
    footer_der <- ggplot() +
      geom_text(
        aes(x = 1.05, y = 0, label = "Los porcentajes están redondeados y pueden no sumar 100%"),
        hjust = 1, family = "Arial", size = 3.5, fontface = "bold",
        color = "#002060", lineheight = 0.85) +
      coord_cartesian(xlim = c(0, 1), clip = "off") +
      ggplot2::theme_void() +
      theme(
        plot.margin = margin(t = 0, r = -1, b = 12, l = 0))
    ## Footer en dos columnas usando patchwork ----
    footer_plot <- (footer_izq + footer_der) +
      patchwork::plot_layout(widths = c(5, 10)) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    footer_plot_wrapped <- patchwork::wrap_elements(panel = footer_plot) &
      theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    p_fixed <- (final_plot) / (footer_plot_wrapped) +
      patchwork::plot_layout(heights = c(15, 1)) +
      patchwork::plot_annotation(
        theme = ggplot2::theme(plot.margin = ggplot2::margin(b = -18))) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
  } else {
    p_fixed <- final_plot
  }
  return(p_fixed)
}
