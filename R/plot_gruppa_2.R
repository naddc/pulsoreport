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

plot_gruppa_2 <- function(data = NULL,
                          vars,
                          ancho = 0.7,
                          T2B = TRUE,
                          title = NULL,
                          unit_extra = TRUE,
                          show_notes = TRUE,
                          x_labels = TRUE) {
  output <- rmarkdown::metadata$output
  output_type <- if (!is.null(output) && any(grepl("pptx", as.character(output)))) {
    "pptx"
  } else {
    "docx"
  }

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

    ## 1.1. Tabular y calcular Ns ----

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
        dplyr::select(dplyr::all_of(var_name)) %>%
        dplyr::mutate(dplyr::across(everything(), as.character)) %>%
        dplyr::filter(rowSums(!is.na(.) & . != '') > 0) %>%
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
      dplyr::filter(Freq != 0)
    tab_final <- tab_final %>%
      dplyr::left_join(n_df, by = 'group')
    n_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')

    # Crear un vector de colores basado en los levels presentes en los datos
    nivel_actual <- levels(tab_final$Var1)
    color_base <- c('#9DC3E6',
                    '#336699',
                    '#8EAADC',
                    '#C19ED6',
                    '#BDD7EE',
                    '#FFE8A7',
                    '#4D4D4D',
                    '#8B7DDD',
                    '#D0CECE')

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
    orden_control_wrapped <- str_wrap(orden_control, width = if (length(vars) <= 1) 60 else 35)
    ### Ordenar tab_final
    tab_final$group <- factor(tab_final$group, levels = orden_grupos)
    tab_final$control <- str_wrap(tab_final$control, width = if (length(vars) <= 1) 60 else 35)
    tab_final$control <- factor(tab_final$control, levels = orden_control_wrapped)

    ## 1.2. Plotear ----
    if (length(vars) <= 1) {
      x_labels <- FALSE
    }
    if (length(unique(tab_final$control)) > 0) {
      # Obtener niveles únicos de cada ítem/pregunta

      control_levels <- unique(tab_final$control)

      # Crear lista de gráficos, uno por cada ítem/pregunta

      plots_with_titles <- purrr::map(control_levels, function(ctrl) {

        # Filtrar datos por control

        df_ctrl <- tab_final %>% dplyr::filter(control == ctrl)

        p <- ggplot(df_ctrl, aes(x = group, y = Freq, fill = Var1)) +
          ggplot2::geom_col(position = 'stack', width =
                              if (length(unique(df_ctrl$group)) == 1) 0.4
                            else ancho) +
          ggrepel::geom_text_repel(
            aes(label = paste0(janitor::round_half_up(Freq), '%')),
            position = ggpp::position_stacknudge(vjust = 0.5),
            size =
              if (output_type == "docx") 9*0.35
            else 14*0.35,
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
            labels = NULL,
            expand =
              if (length(unique(df_ctrl$group)) == 1) expansion(mult = c(0.5, 0.5))
            else expansion(mult = c(0.4, 0.4))) +
          ggplot2::theme_void() +
          ggplot2::theme(
            axis.ticks.y = element_blank(),
            legend.position = 'none',
            plot.margin = margin(t = -10))

        # Alinear los gráficos verticalmente y al eje izquierdo
        aligned_plot <- cowplot::align_plots(plotlist = list(ggplotGrob(p)), align = "v", axis = "l")[[1]]

        # Título del ítem
        text_label <- grid::textGrob(
          ctrl,
          x = 1,
          just = 'right',
          gp = gpar(
            fontsize = if (output_type == "docx") 9 else if (length(vars) <= 1) 12 else 13,
            fontfamily = 'Arial',
            col = '#002060',
            fontface = if (isTRUE(x_labels)) 'bold' else 'plain',
            lineheight = 0.8
          )
        )

        # Etiquetas de eje Y como mini plot si x_labels = TRUE
        if (isTRUE(x_labels)) {
          label_df <- data.frame(group = rev(unique(df_ctrl$group)))

          y_label_plot <- ggplot(label_df, aes(x = 1, y = group)) +
            geom_text(aes(label = group),
                      hjust = 0.5,
                      just = 'right',
                      color = "#002060",
                      size = if (output_type == "docx") 9*0.35 else 12*0.35,
                      fontface = if (output_type == "docx") 'bold' else 'plain',
                      family = "Arial") +
            {if (output_type == "docx")
              scale_y_discrete(
                limits = rev(unique(df_ctrl$group)),
                expand = if (length(df_ctrl$group) > 1) expansion(add = c(0.3, 0.3)) else expansion(add = c(0, 0)))} +
            {if (output_type == "docx")
              scale_x_continuous(
                limits = c(0, 1),
                expand = expansion(mult = c(0.2, 0.2)))} +
            {if (output_type == "pptx")
              scale_y_discrete(
                limits = rev(unique(df_ctrl$group)),
                expand = expansion(mult = c(0.4, 0.4)))}+
            {if (output_type == "pptx")
              scale_x_discrete(
                expand =
                  if (length(unique(df_ctrl$group)) <= 1) expansion(mult = c(0.5, 0.5))
                else expansion(mult = c(0.4, 0.4)))} +
            # scale_y_discrete(
            #   limits = rev(unique(df_ctrl$group)),
            #   expand = expansion(mult = c(0.4, 0.4))) +
            # scale_x_discrete(
            #   expand =
            #     if (length(unique(df_ctrl$group)) <= 1) expansion(mult = c(0.5, 0.5))
            #   else expansion(mult = c(0.4, 0.4))) +
            coord_cartesian(clip = "off") +
            theme_void() +
            theme(
              plot.margin = margin(
                t = if (output_type == "docx" | length(unique(df_ctrl$group)) == 2) 0 else -10,
                b = if (output_type == "docx" | length(unique(df_ctrl$group)) == 2) 0 else -10)
            )

          y_label_grob <- ggplotGrob(y_label_plot)
        } else {
          y_label_grob <- grid::nullGrob()
        }

        #### Combinar todo en arranged
        # Ensamblar
        arranged <- if (isTRUE(T2B)) {
          gridExtra::arrangeGrob(
            # text_label,
            # y_label_grob,
            # aligned_plot,
            # grid::nullGrob(),
            add_border(text_label, "blue"),
            add_border(y_label_grob, "green"),
            add_border(aligned_plot, "orange"),
            add_border(grid::nullGrob(), "purple"),
            ncol = 4,
            widths = unit.c(
              unit(if (output_type == "docx") 2                 # Título
                   else if (length(vars) <= 1) 11.49
                   else 9, 'cm'),
              unit(if (length(vars) <= 1) 0.01                  # Y labels
                   else 2.5, 'cm'),
              unit(if (output_type == "docx") 8                 # Plot
                   else if (length(vars) <= 1) 18
                   else 18, 'cm'),
              unit(2.5, 'cm')                                   # T2B
            )
          )
        } else {
          gridExtra::arrangeGrob(
            text_label,
            y_label_grob,
            aligned_plot,
            ncol = 3,
            widths = unit.c(
              unit(4.2, "cm"),
              unit(5, "cm"),
              unit(20, "cm")
            )
          )
        }

        arranged <- gridExtra::arrangeGrob(arranged, ncol = 1)
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

    ## Añadir leyenda: solución de @teunbrand
    # Plot dummy para extraer leyenda
    legend_plot <- ggplot2::ggplot(tab_final, ggplot2::aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(position = 'stack') +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'bottom',
                     legend.title = ggplot2::element_blank(),
                     legend.text = ggplot2::element_text(size = 10.5, color = '#002060', family = 'Arial'),
                     legend.key.size = ggplot2::unit(0.4, 'cm'),
                     legend.margin = ggplot2::margin(l = 20, b = 0)) +
      ggplot2::guides(shape = ggplot2::guide_legend(position = 'bottom'))
    legend_grob <- get_legend(legend_plot)
    legend_patch <- patchwork::wrap_elements(full = legend_grob) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
    final_plot <- p / legend_patch +
      plot_layout(ncol = 1,
                  heights = c(5, 0.1))

    ## Aplicar tema general
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

    ## 2.1. Tabular y calcular Ns ----
    tablas <- list()
    etiquetas <- list()
    totales <- list()
    colores <- c()

    if (isTRUE(T2B)) {
      T2B_df <- tibble::tibble(Top2B = character(), group = character())
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
                        Var1 = factor(Var1, levels = rev(levels))) %>%
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
                                     tibble::tibble(
                                       control = etiqueta,
                                       Top2B = paste0(T2B_calc, '%'),
                                       group = str_replace(params[[paste0(nombre_data, '_unit')]],'^\\w{1}', toupper)))
        }
      }

      ### Calcular Ns
      n <- dataset %>%
        dplyr::select(dplyr::all_of(var_name)) %>%
        dplyr::mutate(dplyr::across(everything(), as.character)) %>%
        dplyr::filter(rowSums(!is.na(.) & . != '') > 0) %>%
        dplyr::summarise(total = dplyr::n()) %>%
        dplyr::pull(total)
      totales[[params[[paste0(nombre_data, '_unit')]]]] <- n
    }

    tab_final <- do.call(rbind, tablas) %>%
      dplyr::filter(Freq != 0)
    n_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')

    ### Crear un vector de colores basado en los levels presentes en los datos
    nivel_actual <- levels(tab_final$Var1)
    if (length(nivel_actual) > 2) {
      color_map <- c('#A5A5A5',
                     '#70AD47',
                     '#ADD493',
                     '#FFD965',
                     '#F4B183',
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

    orden_control_wrapped <- str_wrap(orden_control, width = if (length(vars) <= 1) 60 else 35)

    #### Ordenar tab_final
    tab_final$group <- factor(tab_final$group, levels = orden_grupos)
    tab_final$control <- str_wrap(tab_final$control, width = if (length(vars) <= 1) 60 else 35)
    tab_final$control <- factor(tab_final$control, levels = orden_control_wrapped)

    #### Ordenar T2B_df
    if (exists('T2B_df')) {
      T2B_df$group <- factor(T2B_df$group, levels = orden_grupos)
      T2B_df$control <- str_wrap(T2B_df$control, width = if (length(vars) <= 1) 60 else 35)
      T2B_df$control <- factor(T2B_df$control, levels = orden_control_wrapped)
    }

    ## 2.2. Plotear ----

    if (length(unique(tab_final$control)) > 1 && length(unique(tab_final$group)) <= 1) {
      x_labels <- FALSE
    }

      # Obtener niveles únicos de cada ítem/pregunta

      control_levels <- unique(tab_final$control)

        # Crear lista de gráficos, uno por cada ítem/pregunta

      plots_with_titles <- purrr::map(control_levels, function(ctrl) {

          # Filtrar datos por control

        df_ctrl <- tab_final %>% dplyr::filter(control == ctrl)

        # Crear plot principal

        p <- ggplot2::ggplot(df_ctrl, aes(x = group, y = Freq, fill = Var1)) +
          ggplot2::geom_col(
            position = 'stack',
            width = if (output_type == "docx" && length(unique(df_ctrl$group)) == 1) 1
            else if (length(unique(df_ctrl$group)) == 1) 0.5
            else ancho) +
          ggrepel::geom_text_repel(ggplot2::aes(label = paste0(janitor::round_half_up(Freq), '%')),
                                   position = ggpp::position_stacknudge(vjust = 0.5),
                                   size = if (output_type == "docx") 9*0.35 else 14*0.35,
                                   color = '#002060',
                                   family = 'Arial',
                                   fontface = 'bold',
                                   direction = 'x',
                                   point.size = NA,
                                   box.padding = 0) +
          ggplot2::coord_flip() +
          ggplot2::scale_fill_manual(values = colores) +

          {if (output_type == "docx")
            ggplot2::scale_x_discrete(labels = NULL,
                                      expand = c(0, 0))} +
          {if (output_type == "docx")
            ggplot2::scale_y_continuous(limits = c(0, 100.1),
                                        expand = c(0, 0))} +
          {if (output_type == "pptx")
            ggplot2::scale_y_reverse()} +
          {if (output_type == "pptx")
            ggplot2::scale_x_discrete(
              labels = NULL,
              expand =
                if (length(unique(tab_final$group)) == 1) expansion(mult = c(0.5, 0.5))
              else expansion(mult = c(0.4, 0.4)))} +

          ggplot2::theme_minimal() +
          ggplot2::theme(
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            legend.position = 'none',
            plot.margin = margin(
              t = if (output_type == "docx") -3
              else if (length(unique(df_ctrl$group)) == 2) 0
              else -10,
              b = if (output_type == "docx") -3
              else if (length(unique(df_ctrl$group)) == 2) 0
              else -10))

        # Alinear los gráficos verticalmente y al eje izquierdo
        aligned_plot <- cowplot::align_plots(plotlist = list(ggplotGrob(p)), align = "v", axis = "l")[[1]]

        # Título del ítem
        if (output_type == "docx" && length(unique(tab_final$control)) == 1) {
          text_label <- grid::nullGrob()
        }
        else {
          lineas <- length(strsplit(str_wrap(ctrl, width = 50), "\n")[[1]])
          y_pos <-
            if (ctrl == tail(control_levels, 1) & lineas > 3) 0.5
          else if (ctrl == tail(control_levels, 1)) 0.6
          else 0.6

          text_label <- grid::textGrob(
            str_wrap(ctrl, width = 50),
            x = 0.97,
            y = y_pos,
            just = c("right", "center"),
            gp = gpar(
              fontsize = if (output_type == "docx") 7
              else if (length(vars) <= 1) 12
              else 13,
              fontfamily = 'Arial',
              col = '#002060',
              fontface = if (output_type == "docx") 'bold'
                    else if (isTRUE(x_labels)) 'bold'
                       else 'plain',
              lineheight = if (output_type == "docx") 0.9 else 0.8
            )
          )
        }

        # Etiquetas de eje Y
        if (isTRUE(x_labels)) {
          label_df <- data.frame(group = rev(unique(df_ctrl$group)))

          y_label_plot <- ggplot(label_df, aes(x = 1, y = group)) +
            geom_text(aes(label = group),
                      hjust = 1,
                      color = "#002060",
                      size = if (output_type == "docx") 8*0.35 else 12*0.35,
                      family = "Arial",
                      fontface = if (output_type == "docx") 'bold' else 'plain') +
            scale_y_discrete(
              limits = rev(unique(df_ctrl$group)),
              expand = if (length(df_ctrl$group) > 1) expansion(add = c(0.3, 0.3)) else expansion(add = c(0, 0))) +
            scale_x_continuous(
              limits = c(0, 1),
              expand = expansion(mult = c(0.2, 0.2))) +
            coord_cartesian(clip = "off") +
            theme_void() +
            theme(
              plot.margin = margin(
                t =
                  if (output_type == "docx" | length(unique(df_ctrl$group)) == 2) 0
                else -10,
                b =
                  if (output_type == "docx" | length(unique(df_ctrl$group)) == 2) 0
                else -10)
            )
          y_label_grob <- ggplotGrob(y_label_plot)
        } else {
          y_label_grob <- grid::nullGrob()
        }

        # Panel derecho T2B
        if (isTRUE(T2B)) {
          df_T2B_ctrl <- T2B_df %>% dplyr::filter(control == ctrl)
          panel_t2b <- ggplotGrob(
            ggplot(df_T2B_ctrl, aes(x = 1, y = group)) +
              geom_text(aes(label = Top2B),
                        hjust = 0.5,
                        color = '#548135',
                        size = if (output_type == "docx") 8*0.35 else 4.55,
                        fontface = 'bold',
                        family = 'Arial') +
              scale_y_discrete(
                limits = rev(unique(df_T2B_ctrl$group)),
                expand = if (length(df_ctrl$group) > 1) expansion(add = c(0.3, 0.3)) else expansion(add = c(0, 0.2))) +

                # expand = expansion(mult = c(0.3, 0.3))) +
              scale_x_discrete(
                expand = if (length(unique(df_T2B_ctrl$group)) <= 1)
                  expansion(mult = c(0.5, 0.5)) else expansion(mult = c(0.4, 0.4))) +
              theme_void() +
              theme(
                plot.margin = margin(
                  t = if (output_type == "docx" | length(unique(df_T2B_ctrl$group)) == 2) 0
                  else -10,
                  b = if (output_type == "docx" | length(unique(df_T2B_ctrl$group)) == 2) 0
                  else -10)))
        }

        # Ensamblar
        arranged <- if (isTRUE(T2B)) {
          gridExtra::arrangeGrob(
            # add_border(text_label, "blue"),
            # add_border(y_label_grob, "green"),
            # add_border(aligned_plot, "orange"),
            # add_border(panel_t2b, "purple"),
            text_label,
            y_label_grob,
            aligned_plot,
            panel_t2b,
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
        } else {
          gridExtra::arrangeGrob(
            text_label,
            y_label_grob,
            aligned_plot,
            ncol = 3,
            widths = unit.c(
              unit(4.2, "cm"),
              unit(5, "cm"),
              unit(20, "cm")
            )
          )
        }

        arranged <- gridExtra::arrangeGrob(arranged, ncol = 1)
        grid::grobTree(arranged, vp = viewport(gp = gpar(fill = 'transparent')))
        }
      )

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

      # Escalar a total deseado
      heights_intercalado <- heights_intercalado / sum(heights_intercalado) * 6
      total_alto_contenido <- sum(heights_intercalado)

      # Combinar los gráficos
      plots_combined <- patchwork::wrap_plots(plots_intercalados, ncol = 1, heights = heights_intercalado)
      p <- plots_combined

      #### Juntar verticalmente todos los gráficos combinados
      p <- plots_combined


      # if (output_type == 'docx') {
      #
      #   eje <- ggplot2::ggplot(x = 1, y = 0) +
      #     ggplot2::theme_minimal() +
      #     ggplot2::scale_x_continuous(limits = c(0, 100.1),
      #                                 breaks = seq(0, 100, by = 25),
      #                                 expand = c(0, 0)) +
      #     ggplot2::theme(
      #       panel.grid.minor = element_blank(),
      #       panel.grid.major = element_blank(),
      #       axis.line = element_blank(),
      #       axis.ticks = element_blank(),
      #       axis.text.y = element_blank(),
      #       axis.title = element_blank(),
      #       axis.line.x = element_line(colour = "#cccccc",
      #                                  linewidth = 0.3),
      #       axis.text.x = element_text(color = "#cccccc",
      #                                  size = 7),
      #       axis.ticks.x.bottom = element_line(color = "#cccccc",
      #                                          linewidth = 0.3))
      #
      #   eje <- arrangeGrob(
      #     nullGrob(),
      #     eje,
      #     nullGrob(),
      #     ncol = 3,
      #     widths = unit.c(
      #       unit(if (length(unique(tab_final$control)) == 1) 3       # Título y Y labels
      #            else if (length(unique(tab_final$control)) > 1) 6
      #            # else if (length(vars) <= 1) 11.99
      #            else 8.5, 'cm'),
      #       unit(if (length(unique(tab_final$control)) == 1) 9.5       # Plot
      #            else if (length(unique(tab_final$control)) > 1) 6.5
      #            else 17, 'cm'),
      #       unit(1.5, 'cm')                                              # T2B
      #     )
      #   )
      #
      #   p <- patchwork::wrap_plots(
      #     p,
      #     eje,
      #     ncol = 1,
      #     heights = c(6.5,
      #                 0.5))
      #   p <- patchwork::wrap_plots(p,
      #                              nrow = 1,
      #                              heights = 6.5) &
      #     ggplot2::theme(
      #       plot.background = element_rect(fill = 'transparent', color = NA),
      #       panel.background = element_rect(fill = 'transparent', color = NA))
      #
      # }

      if (isTRUE(T2B)) {
        col_titles <- arrangeGrob(
          nullGrob(),
          textGrob('TOP TWO BOX',
                   y = if (output_type == "docx") 0.7 else 0,
                   gp = gpar(
                     fontsize = if (output_type == "docx") 7 else 13,
                     fontface = 'bold', col = '#548135',
                     fontfamily = 'Arial')),
          ncol = 2,
          widths = unit.c(unit(if (output_type == "docx") 13.01 else 29.5, 'cm'),
                          unit(if (output_type == "docx") 2 else 2.5, 'cm')))
        p <- patchwork::wrap_plots(
          col_titles,
          p,
          ncol = 1,
          heights = c(if (output_type == "docx") 0.5
                      else 0.1,
                      6))
        p <- patchwork::wrap_plots(p,
                                   nrow = 1,
                                   heights = c(if (output_type == "docx") 6.5 else 6.1)) &
          ggplot2::theme(
            plot.background = element_rect(fill = 'transparent', color = NA),
            panel.background = element_rect(fill = 'transparent', color = NA))
      }

    ## Añadir leyenda: solución de @teunbrand
    ### Plot dummy para extraer leyenda

      tab_final$Var1 <- factor(tab_final$Var1, levels = rev(sort(unique(tab_final$Var1))))

      legend_plot <- ggplot2::ggplot(tab_final, aes(x = group, y = Freq, fill = Var1)) +
        ggplot2::geom_col(position = 'stack') +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = colores) +
        ggplot2::scale_x_discrete(expand = expansion(mult = c(0.5, 0.5))) +
        ggplot2::theme_void() +
        ggplot2::theme(
          legend.position = 'bottom',
          legend.title = element_blank(),
          legend.text = element_text(size = if (output_type == "docx") 7 else 10.5,
                                     color = '#002060',
                                     family = 'Arial'),
          legend.key.size = unit(if (output_type == "docx") 0.3 else 0.4, 'cm'),
          legend.margin = margin(l = 20,
                                 t = if (output_type == "docx") 0 else 0,
                                 b = if (output_type == "docx") 0 else 10)) +
        guides(shape = guide_legend(position = 'bottom'))

      legend_grob <- get_legend(legend_plot)

      legend_patch <- patchwork::wrap_elements(full = legend_grob) &
        ggplot2::theme(
          plot.background = element_rect(fill = 'transparent', color = NA),
          panel.background = element_rect(fill = 'transparent', color = NA))

      final_plot <- p / legend_patch +
        plot_layout(ncol = 1,
                    heights = c(if (output_type == "docx") 6.5 else 6.1, 1))

      ## Aplicar tema general
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
      dplyr::select(dplyr::all_of(var_name)) %>%
      dplyr::mutate(dplyr::across(everything(), as.character)) %>%
      dplyr::filter(rowSums(!is.na(.) & . != '') > 0) %>%
      dplyr::summarise(total = dplyr::n()) %>%
      dplyr::pull(total)
    totales[[params[[paste0(nombre_data, '_unit')]]]] <- n
    n_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')
  }
  if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
    n_totales <- paste(n_totales, params[['unit_extra']])
  }

  # 4. Unir gráficos de variables dicotómicas y escalares ------------
  if (!is.null(p_dicotomicas) && !is.null(p_escalares)) {
    final_plot <- (p_dicotomicas / p_escalares) +
      patchwork::plot_layout(heights = c(4, 6)) &
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

  # 5. Aplicar tema general ----
  final_plot <- final_plot +
    patchwork::plot_annotation(
      theme = ggplot2::theme(
        plot.margin = ggplot2::margin(t = if (output_type == "docx") -10 else 100))) &
    ggplot2::theme(
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.background = element_rect(fill = 'transparent', color = NA))
  final_plot <- patchwork::wrap_elements(panel = final_plot) &
    ggplot2::theme(
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.background = element_rect(fill = 'transparent', color = NA))

  if (output_type == "docx") {
    show_notes <- FALSE
  }

  # 6. Aplicar título y notas de pie ----
  if (isTRUE(show_notes)) {

    ## Texto izquierdo (con lineheight) ----
    footer_izq <- ggplot() +
      geom_text(
        aes(x = -0.06, y = 0, label = str_wrap(paste("Base:", n_totales), width = 67)),
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

  # Generar título automático ----
  if (!is.null(title) && length(unique(tab_final$group)) == 1) {
    title <- paste0(title, ', ', params[[paste0(names(vars), '_unit')]])
  }

  if (is.null(title)) {
    labels_vars <- vars %>%
      purrr::imap(function(var_list, publico) {
        purrr::map_chr(var_list, ~ attr(get(publico)[[.x]], "label") %||% NA_character_)
      }) %>%
      unlist(use.names = FALSE)

    labels_validos <- labels_vars[!is.na(labels_vars)]

    if (length(labels_validos) > 0 && length(unique(labels_validos)) == 1) {
      title <- unique(labels_validos)
    } else if (length(unique(tab_final$group)) == 1) {
      title <- stringr::str_replace(params[[paste0(names(vars), '_unit')]],'^\\w{1}', toupper)
    } else {
      title <- "Título"
    }
  }

  # 7. Exportar ----

  if (output_type == "docx") {
    n_columnas <- max(length(unique(tab_final$control)), length(unique(tab_final$group)))

    return(list(
      plot = p_fixed,
      cap = if (!is.null(title) && is.character(title)) title else NULL,
      N = paste0("N = ",
                 if (exists("n_totales")) n_totales else NULL),
      height = 0.01*n_columnas^2+0.55*n_columnas+0.06
    ))
  } else if (output_type == "pptx") {
    return(p_fixed)
  }
}
