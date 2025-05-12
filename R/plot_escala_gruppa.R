#' Barras horizontales apiladas para preguntas escalares y comparativas entre grupos
#'
#' Genera un gráfico de barras horizontales apiladas para preguntas escalares y comparativas entre grupos, para PowerPoint. Requiere distintas bases para cada grupo definido como lista en `vars`.
#' @import ggplot2
#' @import ggh4x
#' @import ggpubr
#' @import stringr
#' @param vars Variable(s) para plotear.
#' @param T2B Define si se calcula e incluye en el gráfico el "Top 2 Box" (TRUE por defecto).
#' @return Un objeto ggplot que puede ser exportado como dml.
#' @export
plot_escala_gruppa <- function(vars, T2B = TRUE) {

  tablas <- list()
  etiquetas <- list()
  totales <- list()
  if (isTRUE(T2B)) {
    T2B_df <- tibble(Top2B = character(), group = character())
  }

  for (nombre_data in names(vars)) {
    dataset <- get(nombre_data)  # Obtener el dataset por su nombre
    var_name <- vars[[nombre_data]]  # Obtener el nombre de la variable dentro del dataset

    for (var_name in vars[[nombre_data]]) {  # Iterar sobre todas las variables en el dataset

      if (!var_name %in% names(dataset)) next  # Saltar si la variable no está en el dataset

      etiqueta <- attributes(dataset[[var_name]])$label  # Obtener la etiqueta de la variable

      # Crear tabla de proporciones
      tab <- as.data.frame(prop.table(table(sjlabelled::as_label(dataset[[var_name]]))) * 100)

      # Obtener niveles de la variable si tiene etiquetas
      levels <- names(attr(dataset[[var_name]], "labels"))

      # Construir tabla con la etiqueta y los niveles ordenados
      tab <- tab %>%
        dplyr::mutate(control = etiqueta,
               group = if (paste0(nombre_data, "_unit") %in% names(params)) {
                 stringr::str_replace(params[[paste0(nombre_data, "_unit")]],"^\\w{1}", toupper)
               } else {
                 "participantes"
               },
               Var1 = factor(Var1, levels = levels)) %>%
        dplyr::arrange(Var1)

      # Guardar en la lista con identificador único
      tablas[[paste(nombre_data, var_name, sep = "_")]] <- tab
      etiquetas[[paste(nombre_data, var_name, sep = "_")]] <- etiqueta


      ## 2.2. Calcular Top2Box ----
      if (isTRUE(T2B)) {
        T2B_calc <- dataset %>%
          dplyr::mutate(top2 = (dataset[[var_name]] %in% c(4, 3))) %>%
          dplyr::summarise(top2_pct = round((sum(top2, na.rm = TRUE) / n()) * 100)) %>%
          dplyr::pull(top2_pct)

        T2B_df <- dplyr::bind_rows(T2B_df,
                            tibble(
                              control = etiqueta,
                              Top2B = paste0(T2B_calc, '%'),
                              group = str_replace(params[[paste0(nombre_data, "_unit")]],"^\\w{1}", toupper)
                            ))
      }

    }

    n <- dataset %>%
      select(all_of(var_name)) %>%
      dplyr::mutate(across(everything(), as.character)) %>%
      filter(rowSums(!is.na(.) & . != '') > 0) %>%
      dplyr::summarise(total = n()) %>%
      dplyr::pull(total)

    totales[[params[[paste0(nombre_data, "_unit")]]]] <- n

  }

  tab_final <- do.call(rbind, tablas) %>%
    filter(Freq != 0)

  colores <- c() # Crear un vector de colores basado en los levels presentes en los datos

  nivel_actual <- levels(tab_final$Var1)

  color_map <- c('#F4B183',
                 '#FFD965',
                 '#ADD493',
                 '#70AD47',
                 '#A5A5A5',
                 rep('#D9D9D9', length(nivel_actual) - 5))
  names(color_map) <- nivel_actual
  colores <- c(colores, color_map)

  colores <- colores[unique(tab_final$Var1)] # Asegurarse de que los nombres en el vector de colores coincidan con los niveles presentes en los datos


  n_totales <- paste(totales, names(totales), sep = " ", collapse = ", ")

  tab_final$control <- str_wrap(tab_final$control, width = 30)
  T2B_df$control <- str_wrap(T2B_df$control, width = 30)

  # tab_final$group <- droplevels(factor(tab_final$group))

  if (length(unique(tab_final$control)) > 1) {

  p <- ggplot2::ggplot(data = tab_final, aes(x = group, y = Freq, fill = Var1)) +
    ggplot2::geom_col(position = "stack", width = if (length(unique(tab_final$group)) < 2) 0.3 else 0.6) +
    ggrepel::geom_text_repel(aes(label = paste0(round(Freq), "%")),
                             position = ggpp::position_stacknudge(vjust = 0.5),
                             size = 4.93,
                             color = "#002060",
                             family = "Arial",
                             fontface = "bold",
                             direction = 'x',
                             point.size = NA,
                             box.padding = 0) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~control, strip.position = "left", ncol=1,
                        scales = "free_y") +
    ggh4x::force_panelsizes(total_width = unit(6.5, "in"), total_height = unit(4.5, "in")) +
    ggplot2::theme_void() +
    ggplot2::theme(
      text = element_text(color = "#002060",
                          family = "Arial"),
      axis.text.y = element_text(size = 12,
                                 margin = margin(r = 10)),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 10.5,
                                 color = "#002060",
                                 family = "Arial"),
      legend.margin = margin(t = 15, r = 350, b = 0, l = 0),
      legend.key.size=unit(0.4, 'cm'),
      plot.caption = element_text(hjust=c(-0.04, 1.08),
                                  color = "#002060",
                                  size = 10,
                                  face = "bold",
                                  family = "Arial",
                                  margin = margin(t = 48)),
      plot.caption.position = "plot",
      plot.margin = margin(l = 25, r = 52.5, t = 90, b = 3),
      panel.spacing = unit(if (length(unique(tab_final$group)) < 3) -2 else -1.5,
                           "lines"),
      strip.placement = "outside",
      strip.text.y = element_text(size = 13,
                                  color = "#002060",
                                  face = "bold",
                                  family = "Arial"),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)) +
    ggplot2::scale_fill_manual(values = colores) +
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_discrete(
      labels = function(x) str_wrap(x, width = 60),
      expand = expansion(mult = c(0.5, 0.5))) +
    ggplot2::labs(caption = c(paste('Base:', n_totales),
                     "Los porcentajes están redondeados y pueden no sumar 100%"))
  if (isTRUE(T2B)) {
    p <- p +
      ggplot2::geom_text(data = T2B_df,
                aes(x = group, y = -10, label = Top2B),
                inherit.aes = FALSE,
                size = 4.55,
                color = "#548135",
                fontface = "bold",
                family = "Arial",
                hjust = 0) +
      ggplot2::facet_wrap(~control, strip.position = "left", ncol=1,
                          scales = "free_y") +
      ggplot2::labs(subtitle = "TOP TWO BOX") +
      ggplot2::theme(plot.subtitle = element_text(size = 13,
                                         colour = "#548135",
                                         face = "bold",
                                         family = "Arial",
                                         hjust = 1.0819,
                                         vjust = -2.7))
  }
  g <- ggplot2::ggplotGrob(p) # Convertir en gtable
  g$widths[6] <- unit(10, "cm")  # Fijar el espacio del eje Y
  p_fixed <- ggpubr::as_ggplot(g)  # Convierte de nuevo en un objeto ggplot2
  }
  else {

    p <- ggplot2::ggplot(data = tab_final, aes(x = group, y = Freq, fill = Var1)) +
      ggplot2::geom_col(position = "stack", width = if (length(unique(tab_final$group)) < 2) 0.3 else 0.4) +
      # ggplot2::geom_text(aes(label = paste0(round(Freq), "%")),
      #           position = position_stack(vjust = 0.5),
      #           size = 4.93,
      #           color = "#002060",
      #           family = "Arial",
      #           fontface = "bold") +
      ggrepel::geom_text_repel(aes(label = paste0(round(Freq), "%")),
                               position = ggpp::position_stacknudge(vjust = 0.5),
                               size = 4.93,
                               color = "#002060",
                               family = "Arial",
                               fontface = "bold",
                               direction = 'x',
                               point.size = NA,
                               box.padding = 0) +
      ggplot2::coord_flip() +
      ggplot2::theme_void() +
      ggh4x::force_panelsizes(total_width = unit(9.9, "in"), total_height = unit(4.5, "in")) +
      ggplot2::theme(
        axis.text.y = element_text(size = 12,
                                   margin = margin(r = 20)),
        text = element_text(color = "#002060",
                            family = "Arial"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 10.5,
                                   color = "#002060",
                                   family = "Arial"),
        legend.margin = margin(t = 25, r = 50, b = 0, l = -50),
        legend.key.size=unit(0.4, 'cm'),
        plot.caption = element_text(hjust=c(-0.04, 1.08),
                                    color = "#002060",
                                    size = 10,
                                    face = "bold",
                                    family = "Arial",
                                    margin = margin(t = 44)),
        plot.caption.position = "plot",
        plot.margin = margin(l = 0, r = 27, t = 100, b = 2),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_fill_manual(values = colores) +
      ggplot2::scale_x_discrete(labels = function(x) str_wrap(x, width = 60)) +
      ggplot2::labs(caption = c(paste('Base:', n_totales),
                       "Los porcentajes están redondeados y pueden no sumar 100%"))

    if (isTRUE(T2B)) {
      p <- p +
        ggplot2::geom_text(data = T2B_df,
                         aes(x = group, y = -10, label = Top2B),
                         inherit.aes = FALSE,
                         size = 4.55,
                         color = "#548135",
                         fontface = "bold",
                         family = "Arial",
                         hjust = 0) +
        ggplot2::labs(subtitle = "TOP TWO BOX") +
        ggplot2::theme(plot.subtitle = element_text(size = 13,
                                           colour = "#548135",
                                           face = "bold",
                                           family = "Arial",
                                           hjust = 1.0509,
                                           vjust = 1.2 # 1.3
                                           ))
    }
    g <- ggplot2::ggplotGrob(p) # Convertir en gtable
    g$widths[6] <- unit(6, "cm")  # Fijar el espacio del eje Y
    p_fixed <- ggpubr::as_ggplot(g)  # Convierte de nuevo en un objeto ggplot2
  }

  return(p_fixed)

}
