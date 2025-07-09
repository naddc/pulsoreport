#' solución de @teunbrand

get_legend <- function(plot, legend = NULL) {

  gt <- ggplot2::ggplotGrob(plot)

  pattern <- 'guide-box'
  if (!is.null(legend)) {
    pattern <- paste0(pattern, '-', legend)
  }

  indices <- grep(pattern, gt$layout$name)

  not_empty <- !vapply(
    gt$grobs[indices],
    inherits, what = 'zeroGrob',
    FUN.VALUE = logical(1)
  )
  indices <- indices[not_empty]

  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}
