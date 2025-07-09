#' Función para imprimir plot y elementos en word.

png <- function(ggobj,
                fig.width = 6,
                fig.height = NULL) {
  if (!is.list(ggobj) || is.null(ggobj$plot)) {
    stop("ggobj debe ser un objeto con $plot")
  }

  fig_width <- fig.width
  fig_height <- fig.height %||% ggobj$height %||% 1.8
  fig_cap <- ggobj$cap %||% NULL

  temp_file <- tempfile(fileext = ".Rmd")

  writeLines(c(
    "---",
    "---",
    "",
    paste0("```{r, echo=FALSE, fig.height=", fig_height,
           ", fig.width=", fig_width,
           if (!is.null(fig_cap)) paste0(', fig.cap="', fig_cap, '"') else "",
           "}"),
    "print(ggobj$plot)",
    "```",
    "",
    "```{=openxml}",
    paste0('<w:p><w:pPr><w:pStyle w:val="Footnote"/></w:pPr><w:r><w:t>',
           htmltools::htmlEscape(paste0(ggobj$N, ". Fuente: PULSO PUCP 2025.")),
           '</w:t></w:r></w:p>'),
    "```",
    "",
    "```{=openxml}",
    "<w:p/>",
    "```"
  ), con = temp_file)

  env <- new.env()
  env$ggobj <- ggobj

  cat(knitr::knit_child(temp_file, quiet = TRUE, envir = env))
}

# png <- function(ggobj) {
#   if (is.list(ggobj)) {
#     print(ggobj$plot)
#
#     # Usar htmltools para escapar caracteres especiales para XML
#     note_text <- htmltools::htmlEscape(paste0(ggobj$N, ". Fuente: PULSO PUCP 2025."))
#
#     # Insertar como bloque openxml con el estilo deseado
#     cat(paste0(
#       '\n\n```{=openxml}\n',
#       '<w:p>\n',
#       '  <w:pPr><w:pStyle w:val="Footnote"/></w:pPr>\n',
#       '  <w:r><w:t>', note_text, '</w:t></w:r>\n',
#       '</w:p>\n',
#       '```\n\n'
#     ))
#
#     cat('\n\n```{=openxml}\n<w:p/>\n```\n\n')  # espacio visual
#   } else {
#     print(ggobj)
#     cat('\n\n```{=openxml}\n<w:p/>\n```\n\n')  # espacio visual
#   }
# }
