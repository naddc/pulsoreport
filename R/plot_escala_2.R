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
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export

plot_escala_2 <- function(data = NULL,
                          vars,
                          ancho = 0.7,
                          T2B = TRUE,
                          title = NULL,
                          unit_extra = TRUE,
                          show_n = TRUE,
                          show_notes = TRUE,
                          x_labels = TRUE) {

  output <- rmarkdown::metadata$output
  output_type <- if (!is.null(output) && any(grepl("pptx", as.character(output)))) {
    "pptx"
  } else {
    "docx"
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
                        Var1 = factor(Var1, levels = rev(levels))) %>%
          dplyr::arrange(Var1)

        # Guardar en la lista con identificador único
        tablas[[paste(nombre_data, var_name, sep = '_')]] <- tab
        etiquetas[[paste(nombre_data, var_name, sep = '_')]] <- etiqueta


        # Calcular Ns y guardar por combinación de control y group
        n <- dataset %>%
          dplyr::select(dplyr::all_of(var_name)) %>%
          dplyr::mutate(dplyr::across(everything(), as.character)) %>%
          dplyr::filter(rowSums(!is.na(.) & . != '') > 0) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)

        this_group <- if (paste0(nombre_data, '_unit') %in% names(params)) {
          stringr::str_replace(params[[paste0(nombre_data, '_unit')]], '^\\w{1}', toupper)
        } else {
          'Participantes'
        }

        totales[[params[[paste0(nombre_data, '_unit')]]]] <- n

        # Guardar N con su control y grupo
        n_barras[[paste(nombre_data, var_name, sep = '_')]] <- list(
          control = etiqueta,
          group = this_group,
          N_group = n
        )
      }
    }

    # Construir data.frame con control, group y N_group
    n_df <- purrr::map_dfr(n_barras, tibble::as_tibble)

    # Unir con tab_final
    tab_final <- do.call(rbind, tablas) %>%
      dplyr::filter(Freq != 0)

    tab_final <- tab_final %>%
      dplyr::left_join(n_df, by = c("control", "group"))

    # Crear un vector de colores basado en los levels presentes en los datos
    nivel_actual <- levels(tab_final$Var1)
    color_base <- c(
      '#336699',
      '#9DC3E6',
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
    }) %>%
      rev(.)

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
                                 color = '#FFFFFF',
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
          ggplot2::scale_x_discrete(
            labels = NULL,
            expand =
              # if (length(unique(tab_final$group)) == 1) expansion(mult = c(0.5, 0.5))
             expansion(add = c(0, 0)))} +
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
            else 0,
            b = if (output_type == "docx") -3
            else if (length(unique(df_ctrl$group)) == 2) 0
            else 0))

      if (isTRUE(show_n)) {
        n_labels <- tab_final %>%
          dplyr::filter(control == ctrl) %>%
          dplyr::distinct(group, control, N_group)

        n_barras <- sum(lengths(vars_dicotomicas)) + sum(lengths(vars_escalares))
        vjust_final <- -0.1 * n_barras^2 + 1.8 * n_barras - 9.9
        # vjust_final <- -3.4 - ((5 - n_barras) * 0.6)

        p <- p +
          ggplot2::geom_text(
            data = n_labels,
            ggplot2::aes(label = paste0('N = ', N_group), x = group, y = 100),
            hjust = 1,
            vjust = vjust_final,
            size = 3.5,
            family = 'Arial',
            fontface = 'italic',
            color = '#002060',
            inherit.aes = FALSE)
      }

      # Alinear los gráficos verticalmente y al eje izquierdo
      aligned_plot <- cowplot::align_plots(plotlist = list(ggplotGrob(p)), align = "v", axis = "l")[[1]]

      # Título del ítem
      if (output_type == "docx" && length(unique(tab_final$control)) == 1) {
        text_label <- grid::nullGrob()
      }
      else {
        lineas <- length(strsplit(str_wrap(ctrl, width =
                                             if (output_type == "docx") 50
                                           else 35),
                                  "\n")[[1]])
        y_pos <-
          if (ctrl == tail(control_levels, 1) & lineas > 3) 0.5
        else if (ctrl == tail(control_levels, 1)) 0.6
        else 0.6

        text_label <- grid::textGrob(
          str_wrap(ctrl, width =
                     if (output_type == "docx") 50
                   else 35),
          x = 0.45, # 0.97
          gp = gpar(
            fontsize =
              if (output_type == "docx") 7
            else if (length(vars) <= 1) 12
            else 13,
            fontfamily = 'Arial',
            col = '#002060',
            fontface =
              if (output_type == "docx") 'bold'
            else 'plain',
            lineheight =
              if (output_type == "docx") 0.9
            else 0.8
          )
        )
      }

      # Panel derecho T2B
      if (isTRUE(T2B)) {
        panel_t2b <- grid::nullGrob()
      }

      # Ensamblar
      arranged <- if (isTRUE(T2B)) {
        gridExtra::arrangeGrob(
          # add_border(text_label, "blue"),
          # add_border(aligned_plot, "orange"),
          # add_border(panel_t2b, "purple"),
          text_label,
          aligned_plot,
          panel_t2b,
          ncol = 3,
          widths = unit.c(
            unit(if (output_type == "docx" &&
                     length(unique(tab_final$control)) == 1) 0.01    # Título
                 else if (output_type == "docx" &&
                          length(unique(tab_final$control)) > 1) 6.49
                 else if (length(vars) <= 1) 11.49
                 else 13, 'cm'),
            unit(if (output_type == "docx" &&
                     length(unique(tab_final$control)) == 1) 9.5       # Plot
                 else if (output_type == "docx" &&
                          length(unique(tab_final$control)) > 1) 6.5
                 else if (length(vars) <= 1) 16
                 else 16, 'cm'),
            unit(if (output_type == "docx") 2                         # T2B
                 else 2, 'cm')
          )
        )
      } else {
        gridExtra::arrangeGrob(
          text_label,
          aligned_plot,
          ncol = 2,
          widths = unit.c(
            unit(9.2, "cm"),
            unit(20, "cm")
          )
        )
      }

      arranged <- gridExtra::arrangeGrob(arranged, ncol = 1)
      grid::grobTree(arranged, vp = viewport(gp = gpar(fill = 'transparent')))
    }
    )

    # Calcular cantidad de grupos (barras) por control
    n_grupos <- sapply(control_levels, function(ctrl) {
      length(unique(tab_final$group[tab_final$control == ctrl]))
    })

    # Normalizar alturas para que sumen 6
    heights_normalizados <- n_grupos / sum(n_grupos) * 6

    # Intercalar plot + spacer
    plots_intercalados <- purrr::flatten(
      purrr::map(plots_with_titles, function(p) list(p, patchwork::plot_spacer()))
    )

    # Quitar último spacer (no se necesita al final)
    plots_intercalados <- plots_intercalados[-length(plots_intercalados)]

    # Intercalar altura + spacer (espacio fijo, ej: 0.2)
    heights_intercalado <- purrr::flatten_dbl(
      purrr::map(heights_normalizados, function(h) c(h, 0.2))
    )
    heights_intercalado <- heights_intercalado[-length(heights_intercalado)]

    # Escalar todo para que sume 6 unidades totales
    heights_intercalado <- heights_intercalado / sum(heights_intercalado) * 6

    # Combinar todos los gráficos con separación visual
    plots_combined <- patchwork::wrap_plots(
      plots_intercalados,
      ncol = 1,
      heights = heights_intercalado
    )

    p <- plots_combined

    if (isTRUE(T2B)) {
      col_titles <- nullGrob()
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

    # Añadir leyenda
    # Plot dummy para extraer leyenda
    tab_final$Var1 <- factor(tab_final$Var1, levels = rev(sort(unique(tab_final$Var1))))

    legend_plot <- ggplot2::ggplot(tab_final, ggplot2::aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(position = 'stack') +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(values = colores) +
      # ggplot2::scale_y_reverse() +
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
    n_barras <- list()
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

        # Calcular Ns y guardar por combinación de control y group
        n <- dataset %>%
          dplyr::select(dplyr::all_of(var_name)) %>%
          dplyr::mutate(dplyr::across(everything(), as.character)) %>%
          dplyr::filter(rowSums(!is.na(.) & . != '') > 0) %>%
          dplyr::summarise(total = dplyr::n()) %>%
          dplyr::pull(total)

        this_group <- if (paste0(nombre_data, '_unit') %in% names(params)) {
          stringr::str_replace(params[[paste0(nombre_data, '_unit')]], '^\\w{1}', toupper)
        } else {
          'Participantes'
        }

        totales[[params[[paste0(nombre_data, '_unit')]]]] <- n

        # Guardar N con su control y grupo
        n_barras[[paste(nombre_data, var_name, sep = '_')]] <- list(
          control = etiqueta,
          group = this_group,
          N_group = n
        )
      }
    }

    # Construir data.frame con control, group y N_group
    n_df <- purrr::map_dfr(n_barras, tibble::as_tibble)

    # Unir con tab_final
    tab_final <- do.call(rbind, tablas) %>%
      dplyr::filter(Freq != 0)

    tab_final <- tab_final %>%
      dplyr::left_join(n_df, by = c("control", "group"))

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

    ### Ordenar según group (públicos) y control (enunciados)
    #### Crear vector ordenado de públicos según orden de vars_escalares y params
    orden_grupos <- sapply(names(vars_escalares), function(nombre_data) {
      unidad_param <- paste0(nombre_data, '_unit')
      if (unidad_param %in% names(params)) {
        stringr::str_replace(params[[unidad_param]], '^\\w{1}', toupper)
      } else {
        'Participantes'
      }
    }) %>%
      rev(.)

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
          width =
            if (output_type == "docx" && length(unique(df_ctrl$group)) == 1) 1
          # else if (length(vars_dicotomicas) == 0) 0.5
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
        ggplot2::scale_fill_manual(values = colores)

      if (output_type == "docx") {
        p <- p +
          ggplot2::scale_x_discrete(labels = NULL,
                                    expand = c(0, 0)) +
          ggplot2::scale_y_continuous(limits = c(0, 100.1),
                                      expand = c(0, 0))
      }

      if (output_type == "pptx") {
        p <- p +
          ggplot2::scale_x_discrete(
            labels = NULL,
            expand = expansion(add = c(
              if (length(vars_dicotomicas) == 0) 0.3 else 0,
              if (length(vars_dicotomicas) == 0) 0.3 else 0)))
      }

      p <- p +
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
            else if (length(vars_dicotomicas) == 0 && length(unique(df_ctrl$control)) == 2
            ) 40
            else 0,
            b = if (output_type == "docx") -3
            else if (length(vars_dicotomicas) == 0 && length(unique(df_ctrl$control)) == 2
            ) 40
            else 0))

      if (isTRUE(show_n)) {
        n_labels <- df_ctrl %>%
          dplyr::filter(control == ctrl) %>%
          dplyr::distinct(group, control, N_group)

        n_barras <- sum(lengths(vars_dicotomicas)) + sum(lengths(vars_escalares))
        vjust_final <- -0.1 * n_barras^2 + 1.8 * n_barras - 9.9

        p <- p +
          ggplot2::geom_text(
            data = n_labels,
            ggplot2::aes(label = paste0('N = ', N_group), x = group, y = 100),
            hjust = 1,
            vjust = vjust_final,
            size = 3.5,
            family = 'Arial',
            fontface = 'italic',
            color = '#002060',
            inherit.aes = FALSE)
      }

      # Alinear los gráficos verticalmente y al eje izquierdo
      aligned_plot <- cowplot::align_plots(plotlist = list(ggplotGrob(p)), align = "v", axis = "l")[[1]]

      # Título del ítem
      if (output_type == "docx" && length(unique(tab_final$control)) == 1) {
        text_label <- grid::nullGrob()
      }
      else {
        lineas <- length(strsplit(str_wrap(ctrl, width = if (output_type == "docx") 50 else 35),
                                  "\n")[[1]])
        y_pos <-
          if (ctrl == tail(control_levels, 1) & lineas > 3) 0.5
        else if (ctrl == tail(control_levels, 1)) 0.6
        else 0.6

        if (output_type == "docx") {
          text_label <- grid::textGrob(
            str_wrap(ctrl, width = 50),
            x = 0.97,
            y = y_pos,
            just = c("right", "center"),
            gp = gpar(
              fontsize = 7,
              fontfamily = 'Arial',
              col = '#002060',
              fontface = 'bold',
              lineheight = 0.9
            )
          )
        } else {
          text_label <- grid::textGrob(
            str_wrap(ctrl, width = 50),
            x = 0.5,
            gp = gpar(
              fontsize = if (length(vars) <= 1) 12 else 13,
              fontfamily = 'Arial',
              col = '#002060',
              # fontface = 'plain',
              lineheight = 0.8
            )
          )
        }

      }

      # Panel derecho T2B
      if (isTRUE(T2B)) {
        df_T2B_ctrl <- T2B_df %>% dplyr::filter(control == ctrl)
        panel_t2b <- ggplotGrob(
          ggplot(df_T2B_ctrl, aes(x = 1, y = group)) +
            geom_text(aes(label = Top2B),
                      hjust = if (output_type == "docx") 0.5 else 0.2,
                      color = '#548135',
                      size = if (output_type == "docx") 8*0.35 else 4.55,
                      fontface = 'bold',
                      family = 'Arial') +
            scale_y_discrete(
              limits = rev(unique(df_T2B_ctrl$group)),
              expand =
                if (length(vars_dicotomicas) == 0 && output_type == "pptx") expansion(add = c(0.3, 0.3))
              else expansion(add = c(0.3, 0.3))) +
            scale_x_discrete(
              expand =
                if (length(unique(df_T2B_ctrl$group)) <= 1) expansion(mult = c(0.5, 0.5))
              else expansion(mult = c(0.4, 0.4))) +
            theme_void() +
            theme(
              plot.margin = margin(
                t = if (output_type == "docx") 0
                else if (length(vars_dicotomicas) == 0 && length(unique(df_ctrl$control)) == 2
                ) 40
                else if(length(unique(df_T2B_ctrl$group)) == 2) 0
                else 0,
                b = if (output_type == "docx") 0
                else if (length(vars_dicotomicas) == 0 && length(unique(df_ctrl$control)) == 2
                ) 40
                else if(length(unique(df_T2B_ctrl$group)) == 2) 0
                else 0)))
      }

      # Ensamblar
      arranged <- if (isTRUE(T2B)) {
        gridExtra::arrangeGrob(
          # add_border(text_label, "blue"),
          # add_border(aligned_plot, "orange"),
          # add_border(panel_t2b, "purple"),
          text_label,
          aligned_plot,
          panel_t2b,
          ncol = 3,
          widths = unit.c(
            unit(if (output_type == "docx" &&
                     length(unique(tab_final$control)) == 1) 0.01    # Título
                 else if (output_type == "docx" &&
                          length(unique(tab_final$control)) > 1) 6.49
                 else if (output_type == "docx" &&
                          length(vars) <= 1) 11.49
                 else 10, 'cm'),
            unit(if (output_type == "docx" &&
                     length(unique(tab_final$control)) == 1) 9.5       # Plot
                 else if (output_type == "docx" &&
                          length(unique(tab_final$control)) > 1) 6.5
                 else if (length(vars) <= 1) 19
                 else 19, 'cm'),
            unit(if (output_type == "docx") 2                         # T2B
                 else 2, 'cm')
          )
        )
      } else {
        gridExtra::arrangeGrob(
          text_label,
          aligned_plot,
          ncol = 3,
          widths = unit.c(
            unit(9.2, "cm"),
            unit(20, "cm")
          )
        )
      }

      arranged <- gridExtra::arrangeGrob(arranged, ncol = 1)
      grid::grobTree(arranged, vp = viewport(gp = gpar(fill = 'transparent')))
    }
    )

    # Combinar los gráficos
    # Calcular cantidad de grupos (barras) por control
    n_grupos <- sapply(control_levels, function(ctrl) {
      length(unique(tab_final$group[tab_final$control == ctrl]))
    })

    # Normalizar alturas para que sumen 6
    heights_normalizados <- n_grupos / sum(n_grupos) * 6

    # Intercalar plot + spacer
    plots_intercalados <- purrr::flatten(
      purrr::map(plots_with_titles, function(p) list(p, patchwork::plot_spacer()))
    )

    # Quitar último spacer (no se necesita al final)
    plots_intercalados <- plots_intercalados[-length(plots_intercalados)]

    # Intercalar altura + spacer (espacio fijo, ej: 0.2)
    heights_intercalado <- purrr::flatten_dbl(
      purrr::map(heights_normalizados, function(h) c(h,
                                                     if(length(p_dicotomicas) > 0) 0.2
                                                     else 0.2))
    )
    heights_intercalado <- heights_intercalado[-length(heights_intercalado)]

    # Escalar todo para que sume 6 unidades totales
    heights_intercalado <- heights_intercalado / sum(heights_intercalado) * 6

    # Combinar todos los gráficos con separación visual
    plots_combined <- patchwork::wrap_plots(
      plots_intercalados,
      ncol = 1,
      heights = heights_intercalado
    )

    p <- plots_combined

    if (isTRUE(T2B)) {
      col_titles <- arrangeGrob(
        nullGrob(),
        textGrob('TOP TWO BOX',
                 y = if (output_type == "docx") 0.7 else 1.3, # 0
                 gp = gpar(
                   fontsize = if (output_type == "docx") 7 else 13,
                   fontface = 'bold', col = '#548135',
                   fontfamily = 'Arial')),
        ncol = 2,
        widths = unit.c(unit(if (output_type == "docx") 13.01 else 29.5, 'cm'),
                        unit(if (output_type == "docx") 2 else 2, 'cm')))

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
                               b = if (output_type == "docx") 0 else 5)) +
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

  # 4. Unir gráficos de variables dicotómicas y escalares ------------
  if (!is.null(p_dicotomicas) && !is.null(p_escalares)) {
    d <- sum(lengths(vars_dicotomicas))
    e <- sum(lengths(vars_escalares))

    # Normalizamos los pesos para que sumen 10
    total <- d + e
    d_normalized <- (10 * d / total)
    e_normalized <- (10 * e / total)

    final_plot <- (p_dicotomicas / p_escalares) +
      patchwork::plot_layout(heights = c(d_normalized, e_normalized)) &
      ggplot2::theme(
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
        plot.margin = ggplot2::margin(t = if (output_type == "docx") -10 else 100,
                                      b = if (output_type == "docx") 0 else 19),
      )) &
    ggplot2::theme(
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.background = element_rect(fill = 'transparent', color = NA))
  final_plot <- patchwork::wrap_elements(panel = final_plot) &
    ggplot2::theme(
      plot.background = element_rect(fill = 'transparent', color = NA),
      panel.background = element_rect(fill = 'transparent', color = NA))

  # 6. Aplicar título y notas de pie ----
  if (output_type == "docx") {
    show_notes <- FALSE
  }

  if (show_notes) {
    if (!is.null(data)) {
      footer_plot_wrapped <- show_notes(
        data = data,
        vars = vars
      )
    } else if (is.null(data) && is.list(vars)) {
      footer_plot_wrapped <- show_notes(
        data = vars
      )
    }

    p_fixed <- (final_plot) / (footer_plot_wrapped) +
      patchwork::plot_layout(heights = c(15, 1)) +
      patchwork::plot_annotation(
        theme = ggplot2::theme(plot.margin = ggplot2::margin(b = -18))) &
      ggplot2::theme(
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_rect(fill = 'transparent', color = NA))
  }
  else {
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
