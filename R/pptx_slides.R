#' Formato para presentaciones en PowerPoint
#'
#' @import officedown
#' @export
pptx_slides <- function(...) {

  officedown::rpptx_document(reference_doc = system.file("rmarkdown/templates/pptx_slides/skeleton/template.pptx", package = "pulsoreport"),
                             slide_level = 2)
}

