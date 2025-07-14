ggspider <- function(data,
                     labels_width = 25, # 30
                     show_axis_labels = TRUE,
                     area_fill = FALSE,
                     central_distance = 0,
                     axis_name_offset = 0.2,
                     ticks = 5,
                     colores = c("#002060", "#ED7D31",'#FFC000'),
                     min_value = 0,
                     max_value = NULL,
                     output_type = 'pptx'
) {

  # 1. Parámteros básicos ==================

  n_axis <- ncol(data) - 1                       # Cantidad de variables (columnas excepto "group")
  var_names <- colnames(data)[-1]                # Nombres de las variables
  angles <- seq(0, 2 * pi,                       # Ángulos equidistantes para cada eje (en sentido antihorario, empezando desde arriba)
                length.out = n_axis + 1)[- (n_axis + 1)] + pi / 2

  # 2. Valor máximo y mínimo ===============

  if (is.null(max_value)) {
    max_value <- max(data %>%
                       select(-group),
                     na.rm = TRUE)
  }

  if (is.null(min_value)) {
    min_value <- min(data %>%
                       select(-group),
                     na.rm = TRUE)
  }

  # 3. Coordenadas de los puntos ===========

  # Tabla auxiliar que relaciona cada "parameter" con su ángulo
  angle_df <- tibble(parameter = var_names,
                     angle = angles)

  # Conversión de valores a coordenadas cartesianas con el ángulo y el radio proporcional
  spider_coords <- data %>%
    tidyr::pivot_longer(-group,
                        names_to = "parameter",
                        values_to = "value") %>%
    dplyr::left_join(angle_df,
                     by = "parameter") %>%
    dplyr::mutate(
      r = (value - min_value) / (max_value - min_value), # Proporción del valor sobre el máximo
      x = (r + central_distance) * cos(angle),
      y = (r + central_distance) * sin(angle),
      # x = r * cos(angle),                                                     # Coordenada X polar -> cartesiana
      # y = r * sin(angle)                                                      # Coordenada Y polar -> cartesiana
    )

  spider_coords_closed <- spider_coords %>%
    group_by(group) %>%
    dplyr::reframe(
      across(
        c(parameter, value, angle, r, x, y),
        ~ c(., .[1])),
      .groups = "drop") %>%
    dplyr::mutate(group = rep(unique(spider_coords$group),
                              each = n_axis + 1))

  # 4. Círculos guía de referencia =========

  circle_coords <- function(r) {
    tibble(
      x = (r + central_distance) * cos(angles),
      y = (r + central_distance) * sin(angles),
      r = r
    )
  }

  # Círculos de fondo: valores relativos a min_value y max_value
  tick_values <- seq(min_value, max_value, length.out = ticks + 1)
  tick_rs <- (tick_values - min_value) / (max_value - min_value)

  background_grid <- purrr::map_df(seq_along(tick_rs), function(i) {
    r <- tick_rs[i]
    x_vals <- r * cos(angles)
    y_vals <- r * sin(angles)

    tibble(
      x = c(x_vals, x_vals[1]),
      y = c(y_vals, y_vals[1]),
      r = r,
      label = tick_values[i],
      group = factor(i)
    )
  })

  # 5. Etiquetas de variables ==============

  axis_labels <- tibble(
    angle = angles,
    label = var_names,
    x = (1 + central_distance + axis_name_offset) * cos(angle),
    y = (1 + central_distance + axis_name_offset) * sin(angle)
  ) %>%
    mutate(
      label_wrapped = stringr::str_wrap(label, width = labels_width),

      # Ajuste manual del eje x para centrar visualmente los textos
      x = dplyr::case_when(
        cos(angle) >  0.15 ~ x + 0.4,  # 0.6 hacia la derecha: desplazar más a la derecha
        cos(angle) < -0.15 ~ x - 0.4,  # 0.6 hacia la izquierda: desplazar más a la izquierda
        TRUE               ~ x          # arriba/abajo: sin ajuste
      )
    )

  # 6. Ploteo ==============================

  ggplot() +

    # Fondos circulares (líneas guía)
    geom_path(data = background_grid, aes(x, y, group = group),
              color = "#D9D9D9", linewidth = if (output_type == 'docx') 0.2 else 0.5) +

    # Líneas que conectan los puntos por grupo
    geom_path(data = spider_coords_closed,
              aes(x, y, group = group, color = group),
              linewidth = if (output_type == 'docx') 0.5 else 1) +

    # Puntos en cada vértice
    geom_point(data = spider_coords_closed,
               aes(x, y, group = group, color = group),
               size = if (output_type == 'docx') 1 else 3) +

    # Etiquetas numéricas de los ticks (eje de referencia)
    {if (show_axis_labels) geom_text(data = background_grid %>%
                                       dplyr::group_by(label) %>%
                                       dplyr::summarise(
                                         x = 0,
                                         y = unique(r)[1],
                                         label = unique(label)[1]),
                                     aes(x, y, label = label),
                                     size = if (output_type == 'docx') 6 * 0.35 else 9 * 0.35,
                                     color = "#808080")} +

    # Área rellena (opcional)
    {if (area_fill) geom_polygon(data = spider_coords_closed,
                                 aes(x, y, fill = group, group = group, color = group),
                                 alpha = 0.05, linewidth = 1, show.legend = FALSE)} +

    # Colores
    scale_color_manual(values = colores) +
    scale_fill_manual(values = colores) +

    # Nombres de los ejes (fuera del círculo)
    geom_text(data = axis_labels,
              aes(x, y, label = label_wrapped),
              size = if (output_type == 'docx') 9 * 0.35 else 14 * 0.35,
              fontface = 'bold',
              family = 'Arial',
              color = '#002060') +

    # Estilo general del gráfico y leyenda
    coord_fixed(clip = "off") +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = if (output_type == 'docx') 7 else 12,
                                 family = 'Arial',
                                 color = '#002060'),
      legend.margin = margin(t = if (output_type == 'docx') 10 else 10),
      # b = if (output_type == 'docx') 0 else -10),
      legend.key.size = unit(if (output_type == 'docx') 0.5 else 1, "cm"),
      legend.key.width = unit(if (output_type == 'docx') 0.5 else 1.5, "cm"),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}
