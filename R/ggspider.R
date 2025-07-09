#' Spider/radar chart con ggplot
#'
#' Genera un gráfico de tipo radar o araña con valores absolutos. A partir de https://towardsdatascience.com/how-to-make-a-spider-chart-in-r-using-ggplot2-85a4f1898cab/
#' @import ggplot2
#' @import stringr
#' @param data Tabla para plotear. Primera columna `group` contiene nombres de las categorías de los grupos. Resto de columnas contienen los valores por cada variable.
#' @param area_fill Define si rellenar el área del grupo (FALSE por defecto).
#' @param central_distance Define distancia entre el centro y el origen del eje.
#' @param axis_name_offset Define cuánto se alejan los nombres de las variables del borde.
#' @param ticks Define cantidad de valores del eje a graficar.
#' @param colores Define paleta de colores.
#' @param min_value Define valor mínimo esperado para los ejes (si no se define, se calcula).
#' @param max_value Define valor máximo esperado para los ejes (si no se define, se calcula).
#' @return Un objeto ggplot que puede ser exportado como dml o png.

ggspider <- function(data,
                     labels_width = 30,
                     area_fill = FALSE,
                     central_distance = 0,
                     axis_name_offset = 0.2,
                     ticks = 4,
                     colores = c("#002060", "#ED7D31",'#FFC000'),
                     min_value = NULL,
                     max_value = NULL
                     ) {

  # ========================================
  # 1. PARÁMETROS BÁSICOS
  # ========================================

  n_axis <- ncol(data) - 1                       # Cantidad de variables (columnas excepto "group")
  var_names <- colnames(data)[-1]                # Nombres de las variables
  angles <- seq(0, 2 * pi,                      # Ángulos equidistantes para cada eje (en sentido antihorario, empezando desde arriba)
                length.out = n_axis + 1)[- (n_axis + 1)] + pi / 2


  # ========================================
  # 2. VALOR MÁXIMO y MÍNIMO (escala común para todos los ejes)
  # ========================================

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

  # ========================================
  # 3. COORDENADAS DE LOS PUNTOS
  # ========================================

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
      r = ((value - min_value) / (max_value - min_value)) + central_distance, # Proporción del valor sobre el máximo
      x = r * cos(angle),                                                     # Coordenada X polar -> cartesiana
      y = r * sin(angle)                                                      # Coordenada Y polar -> cartesiana
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

  # ========================================
  # 4. CÍRCULOS GUÍA DE REFERENCIA
  # ========================================

  circle_coords <- function(r) {
    tibble(
      x = r * cos(angles),
      y = r * sin(angles),
      r = r
    )
  }

  # Círculos de fondo: valores relativos a min_value y max_value
  ticks <- seq(min_value, max_value, length.out = ticks + 1)
  background_grid <- purrr::map_df((ticks - min_value) / (max_value - min_value) + central_distance, circle_coords)

  # ========================================
  # 5. ETIQUETAS DE LAS VARIABLES (en los extremos de cada eje)
  # ========================================

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
        cos(angle) >  0.15 ~ x + 0.6,  # hacia la derecha: desplazar más a la derecha
        cos(angle) < -0.15 ~ x - 0.6,  # hacia la izquierda: desplazar más a la izquierda
        TRUE               ~ x          # arriba/abajo: sin ajuste
      )
    )

  # ========================================
  # 6. CONSTRUCCIÓN DEL GRÁFICO
  # ========================================

  ggplot() +

    # Fondos circulares (líneas guía)
    geom_polygon(data = background_grid, aes(x, y, group = r),
                 fill = NA, color = "#D9D9D9", linewidth = 0.2) +

    # Líneas que conectan los puntos por grupo
    geom_path(data = spider_coords_closed,
              aes(x, y, group = group, color = group),
              linewidth = 0.5) +


    # Puntos en cada vértice
    geom_point(data = spider_coords_closed,
               aes(x, y, group = group, color = group),
               size = 1) +

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
              size = 9 * 0.35,
              fontface = 'bold',
              family = 'Arial',
              color = '#002060') +

    # Estilo general del gráfico y leyenda
    coord_fixed(clip = "off") +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 7,
                                 family = 'Arial',
                                 color = '#002060'),
      legend.margin = margin(t = 10),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}
