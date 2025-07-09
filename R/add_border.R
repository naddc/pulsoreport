#' Función para añadir bordes a ggobjects para pruebas.
#'
#' Helper para agregar borde de depuración
#' @keywords internal

add_border <- function(grob, color = "red") {
  grid::grobTree(
    grid::rectGrob(gp = grid::gpar(col = color, fill = NA, lwd = 1)),
    grob
  )
}
