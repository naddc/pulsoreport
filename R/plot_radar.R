#' Gráfico de radar para respuestas de Top Two Box
#'
#' @import purrr
#' @import ggplot2
#' @import stringr
#' @param data Un data frame con los datos en formato sav.
#' @param vars Variable o variables para plotear.
#' @param title Recibe un título para el gráfico. Si es NULL (opción por defecto), toma la etiqueta de la variable en vars.
#' @param unit Unidad de observación para declarar el N de la base usada para el gráfico. Si es NULL (opción por defecto), toma el parámeto correspondiente al dataset en data ("data_unit") indicado en los params del yaml. Puede recibir un elemento directamente (no recomendado).
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @param show_notes Define si se mostrarán las notas de pie de "N" y el mensaje sobre el redondeo (si TRUE). FALSE por defecto.
#' @param show_n Define si se mostrará el N como subtítulo (si TRUE). FALSE por defecto.
#' @return Un objeto ggplot que puede ser exportado como dml o png.
#' @export

plot_radar <- function(data = NULL,
                       vars,
                       labels_width = 30,
                       show_axis_labels = FALSE,
                       area_fill = FALSE,
                       central_distance = 0,
                       axis_name_offset = 0.2,
                       ticks = 4,
                       colores = c("#002060", "#ED7D31",'#FFC000'),
                       min_value = 60,
                       max_value = 100,
                       title = NULL,
                       # unit = NULL,
                       unit_extra = TRUE
) {
  output_type <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(output_type)) output_type <- "docx"

  # 1. Tabular ------------------------------------------------

  calc_T2B_row <- function(data, vars, top_values = c(4, 3)) {
    data %>%
      tidyr::pivot_longer(cols = all_of(vars),
                          names_to = "variable",
                          values_to = "valor") %>%
      filter(!is.na(valor)) %>%
      dplyr::mutate(top2 = valor %in% top_values) %>%
      group_by(variable) %>%
      summarise(t2b = janitor::round_half_up(mean(top2) * 100), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = variable, values_from = t2b)
  }

  # Aplicar a cada dataset nombrado en vars
  T2B_result <- imap_dfr(vars, function(var_names, dataset_name) {
    df <- get(dataset_name)

    # Extraer etiquetas
    etiquetas <- map_chr(df[var_names], ~ attr(., "label"))

    # Calcular T2B
    res <- calc_T2B_row(df, vars = var_names)

    # Renombrar columnas con etiquetas
    colnames(res)[match(names(etiquetas), colnames(res))] <- etiquetas

    res %>% mutate(group = dataset_name)
  }, .id = NULL)

  # reorganizar grupo primero
  tab_final <- T2B_result %>%
    mutate(group = factor(group, levels = names(vars))) %>%
    dplyr::relocate(group)

  # 2. Graficar ----
  p <- ggspider(tab_final,
                labels_width = labels_width,
                show_axis_labels = show_axis_labels,
                area_fill = area_fill,
                central_distance = central_distance,
                axis_name_offset = axis_name_offset,
                ticks = ticks,
                colores = colores,
                min_value = min_value,
                max_value = max_value) +
    theme(
      plot.margin =
        margin(t = 10))

  # Generar título automático solo si no se ha especificado manualmente
  if (is.null(title)) {
    labels_vars <- vars %>%
      purrr::imap(function(var_list, publico) {
        purrr::map_chr(var_list, ~ attr(get(publico)[[.x]], "label") %||% NA_character_)
      }) %>%
      unlist(use.names = FALSE)

    labels_validos <- labels_vars[!is.na(labels_vars)]

    if (length(labels_validos) > 0 && length(unique(labels_validos)) == 1) {
      title <- unique(labels_validos)
    } else {
      title <- "Título"
    }
  }

  # 7. Exportar ----

  if (output_type == "docx") {

    # Calcular Ns ---------
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
      n_totales_totales <- paste(n_totales, params[['unit_extra']])
    }

    return(list(
      plot = p,
      cap = if (!is.null(title) && is.character(title)) title else NULL,
      N = paste0("N = ",
                 if (exists("n_totales")) n_totales else NULL),
      height = 4.5
    )
    )
  } else if (output_type == "pptx") {
    return(p)
  }

}
