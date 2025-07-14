#' Muestra N de respuestas y nota al pie de página
#'
#'

max_pr_public <- function(tabla_n) {
  out <- tabla_n %>%
    dplyr::rowwise() %>%
    dplyr::mutate(max_n = max(c_across(-público), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(público, max_n)

  if (all(out$público == ".")) {
    llamada <- sys.call(-1)[[2]]
    nombre_data <- if (is.call(llamada)) as.character(llamada[[2]]) else deparse(llamada)
    out$público <- nombre_data
  }

  return(out)
}
