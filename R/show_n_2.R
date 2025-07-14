#' Muestra N de respuestas y nota al pie de página
#'
#' @param data Un data frame con los datos en formato sav.
#' @param vars Variable o variables a considerar para el cálculo de respuestas.
#' @export

show_n_2 <- function(data,
                     ...,
                     vars = NULL) {

  # 1. Obtener tabla de máximos
  # Pasar el resultado directamente sin variable intermedia
  tab_n <- get_n(data, ..., vars = vars)

  # Procesar max_pr_public directamente
  out <- tab_n %>%
    dplyr::rowwise() %>%
    dplyr::mutate(max_n = max(c_across(-público), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(público, max_n)

  # 2. Armar texto
  text_n <- glue::glue_collapse(
    paste(out$max_n,
          stringr::str_to_lower(out$público)),
    sep = ", ",
    last = " y "
  )

  # 3. Plotear
  # Texto izquierdo (con lineheight)
  footer_izq <- ggplot2::ggplot() +
    ggplot2::geom_text(
      ggplot2::aes(x = -0.06, y = 0,
                   label = stringr::str_wrap(paste("Base:",
                                                   text_n),
                                             width = 67)),
      hjust = 0, vjust = 0, family = "Arial", size = 3.5, fontface = "bold",
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
      ggplot2::aes(x = 1.05, y = 0,
                   label = "Los porcentajes están redondeados y pueden no sumar 100%"),
      hjust = 1, vjust = 0, family = "Arial", size = 3.5, fontface = "bold",
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
