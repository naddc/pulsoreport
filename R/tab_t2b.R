#' Tabla para respuestas de Top Two Box
#'
#' @import purrr
#' @import ggplot2
#' @import stringr
#' @param data Un data frame con los datos en formato sav.
#' @param vars Variable o variables para plotear.
#' @param labels_width Define ancho de etiquetas del eje en gráficos de barras no apiladas (30 por defecto).
#' @param unit_extra Define si añade una descripción adicional a la unidad de observación en la nota sobre el N de la base (TRUE por defecto). Requiere añadir el texto adicional desde params.
#' @param show_notes Falta descripción.
#' @return Una tabla que puede ser exportada.
#' @export

tab_t2b <- function(data = NULL,
                    vars,
                    labels_width = 30,
                    unit_extra = FALSE,
                    show_notes = TRUE
) {
  output <- rmarkdown::metadata$output
  output_type <- if (!is.null(output) && any(grepl("pptx", as.character(output)))) {
    "pptx"
  } else {
    "docx"
  }

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

    res %>% mutate(`Top Two Box` = dataset_name)
  }, .id = NULL)

  # reorganizar grupo primero
  tab_final <- T2B_result %>%
    dplyr::relocate(`Top Two Box`) %>%
    t() %>%
    tibble::as_tibble(rownames = "Top Two Box") %>%
    janitor::row_to_names(row_number = 1) %>%
    mutate(across(-1, ~ paste0(.x, "%")),
           across(1, ~ str_wrap(., 25))
    )

  # Renombrar encabezado de la primera columna a mayúsculas
  colnames(tab_final)[1] <- toupper(colnames(tab_final)[1])

  # Calcular Ns ---------
  if (output_type == "docx") {

    if (show_notes) {
      if (!is.null(data)) {
        footer <- show_notes(
          data = data,
          vars = var_name,
          output_type = output_type
        )
      } else if (is.null(data) && is.list(vars)) {
        footer <- show_notes(
          data = vars,
          output_type = output_type
        )
      }
    }
  }
  # totales <- list()
  # for (nombre_data in names(vars)) {
  #   dataset <- get(nombre_data)  # Obtener el dataset por su nombre
  #   var_name <- vars[[nombre_data]]
  #   ### Calcular Ns
  #   n <- dataset %>%
  #     dplyr::select(dplyr::all_of(var_name)) %>%
  #     dplyr::mutate(dplyr::across(everything(), as.character)) %>%
  #     dplyr::filter(rowSums(!is.na(.) & . != '') > 0) %>%
  #     dplyr::summarise(total = dplyr::n()) %>%
  #     dplyr::pull(total)
  #   totales[[params[[paste0(nombre_data, '_unit')]]]] <- n
  #   n_totales <- paste(totales, names(totales), sep = ' ', collapse = ', ')
  # }
  # if (isTRUE(unit_extra) && !is.null(params[['unit_extra']])) {
  #   n_totales <- paste(n_totales, params[['unit_extra']])
  # }

  # Crear tabla tipo flextable ----
  if (output_type == 'docx') {
    num_mat <- tab_final[, -1] %>%
      mutate(across(everything(), ~ suppressWarnings(as.numeric(str_remove_all(., "%")))))

    colormatrix <- ifelse(num_mat <= 75, "red", "black")

    tab_final <- tab_final %>%
      flextable::flextable() %>%
      flextable::set_header_labels(.default = names(tab_final)) %>%
      flextable::bold(part = "header") %>%
      flextable::color(color = "white", part = "header") %>%
      flextable::color(j = 2:ncol(tab_final), color = colormatrix) %>%
      flextable::bg(bg = "#336699", part = "header") %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::align(j = 1, align = "left", part = "all") %>%
      flextable::border_remove() %>%
      flextable::border_outer(border = fp_border(color = "black", width = 1)) %>%
      flextable::border_inner(border = fp_border(color = "black", width = 0.75)) %>%
      flextable::fontsize(size = 7, part = "all") %>%
      flextable::autofit()
  }
  else {
    colnames(tab_final)[1] <- toupper(colnames(tab_final)[1])

    tab_final <- tab_final %>%
      flextable::flextable() %>%
      flextable::set_header_labels(.default = names(tab_final)) %>%
      flextable::bold(part = "header") %>%
      flextable::bold(j = 1) %>%
      flextable::color(color = "white", part = "header") %>%
      flextable::bg(bg = "#002060", part = "header") %>%
      flextable::color(color = "#002060", part = "body") %>%
      flextable::align(align = "center", part = "all") %>%
      flextable::align(j = 1, align = "left", part = "header") %>%
      flextable::align(j = 1, align = "justify", part = "body") %>%
      flextable::border_remove() %>%
      flextable::border_outer(border = fp_border(color = "white", width = 1)) %>%
      flextable::border_inner(border = fp_border(color = "white", width = 0.75)) %>%
      flextable::fontsize(size = 11, part = "all") %>%
      flextable::autofit() %>%
      flextable::height_all(height = .6)
  }

  if (output_type == "docx") {

    cat(knit_print(tab_final))

    cat(paste0(
      '\n\n```{=openxml}\n',
      '<w:p>\n',
      '  <w:pPr><w:pStyle w:val="Footnote"/></w:pPr>\n',
      '  <w:r><w:t>', htmltools::htmlEscape(paste0('N = ', footer, ". Fuente: PULSO PUCP 2025.")), '</w:t></w:r>\n',
      '</w:p>\n',
      '```\n\n'
    ))
  } else {
    tab_final
  }
}
