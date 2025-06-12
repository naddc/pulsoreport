#' Muestra N de respuestas y nota al pie de página
#'
#' @param data Un data frame con los datos en formato sav.
#' @param var Variable o variables a considerar para el cálculo de respuestas.
#' @param unit Unidad de observación para declarar el N.
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @export
show_n <- function(data, var, unit = NULL, unit_extra = TRUE) {

  # 1. Obtener unit
  nombre_data <- if (deparse(substitute(data)) == ".") {
    deparse(sys.call(-1)[[2]])
  } else {
    deparse(substitute(data))  # Si no está en un pipe, captura normal
  }

  if (is.null(unit)) {
    unit <- params[[paste0(nombre_data, "_unit")]]

    if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
      unit <- paste(unit, params[['unit_extra']])
    }

    # unit <- if (paste0(nombre_data, "_unit") %in% names(params)) {
    #   params[[paste0(nombre_data, "_unit")]]
    # } else {
    #   "participantes"
    # }
  }

  # 2. Tabular
  n_total <- data %>%
    select(all_of(var)) %>%
    mutate(across(everything(), as.character)) %>%
    filter(rowSums(!is.na(.) & . != '') > 0) %>%
    summarise(total = n()) %>%
    pull(total)


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



  }
