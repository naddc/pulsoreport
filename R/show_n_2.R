#' Muestra N de respuestas y nota al pie de página
#'
#' @param data Un data frame con los datos en formato sav.
#' @param var Variable o variables a considerar para el cálculo de respuestas.
#' @param unit Unidad de observación para declarar el N.
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @export
show_n <- function(data,
                   var,
                   unit = NULL,
                   unit_extra = TRUE) {

  # 1. Obtener unit
  nombre_data <- if (deparse(substitute(data)) == ".") {
    deparse(sys.call(-1)[[2]])
  } else {
    deparse(substitute(data))
  }

  if (is.null(unit)) {
    unit <- params[[paste0(nombre_data, "_unit")]]

    if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
      unit <- paste(unit, params[['unit_extra']])
    }

  }

  # 2. Tabular
  n_total <- data %>%
    select(all_of(var)) %>%
    mutate(across(everything(), as.character)) %>%
    filter(rowSums(!is.na(.) & . != '') > 0) %>%
    summarise(total = n()) %>%
    pull(total)

  if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
    if (exists('n_totales') && is.null(data)) {
      n_totales <- paste(n_totales, params[['unit_extra']])
    }
    if (!is.null(unit)) {
      unit <- paste(unit, params[['unit_extra']])
    }
  }

  # 3. Plotear
  ggplot() +
    theme_void() +
    theme(
      text = element_text(color = "#002060",
                          family = "Arial"),
      plot.caption = element_text(hjust=c(0, 1),
                                  color = "#002060",
                                  size = 10,
                                  face = "bold",
                                  family = "Arial"),
      plot.caption.position = "plot") +
    labs(caption = c(str_wrap(paste('Base:', n_total, unit), width = 80),
                     "Los porcentajes están redondeados y pueden no sumar 100%"))

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
  }

