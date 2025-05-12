#' Genera una tabla de ficha técnica, para PowerPoint
#'
#' @param info Texto que irá en la tabla, en forma de objeto tribble
#' @param height Altura mínima de las filas (0.7 por defecto)
#' @return Una tabla con flextable
#' @export
fichatec <- function(info, height = NULL) {
  info %>%
    flextable::flextable() %>%
    flextable::delete_part(part = "header") %>%
    flextable::border_inner_h(border = fp_border(color = "#757070", width = 0.5)) %>%
    flextable::border_inner_v(border = fp_border(color = "#757070", width = 0.5)) %>%
    flextable::border_outer(border = fp_border(color = "#757070", width = 0.5)) %>%
    flextable::width(j = 1, width = 2) %>%
    flextable::width(j = 2, width = 10.5) %>%
    flextable::height(height = if (is.null(height)) 0.7 else height, part = "body") %>%
    flextable::align(j = 1, align = "center", part = "body") %>%
    flextable::bold(j = 1, part = "body") %>%
    flextable::bg(j = 1, bg = "#D8D8D8", part = "body") %>%
    flextable::align(j = 2, align = "left", part = "body") %>%
    flextable::color(color = "#0E255C", part = "body") %>%
    flextable::fontsize(size = 15, part = "body") %>%
    flextable::font(fontname = "Arial", part = "body") %>%
    flextable::valign(valign = "center", part = "body") %>%
    flextable::hrule(rule = "atleast", part = "body") %>%
    ftExtra::colformat_md(.sep = "\n")
}
