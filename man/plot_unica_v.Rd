% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_unica_v.R
\name{plot_unica_v}
\alias{plot_unica_v}
\title{Gráfico de barras verticales para respuestas únicas}
\usage{
plot_unica_v(
  data,
  var,
  group = NULL,
  order_freq = FALSE,
  title = NULL,
  unit = NULL,
  x_labels = TRUE,
  show_notes = FALSE,
  show_n = FALSE
)
}
\arguments{
\item{data}{Un data frame con los datos en formato sav.}

\item{var}{Variable para plotear.}

\item{order_freq}{Define si las barras se ordenarán según frecuencia (si TRUE). FALSE (opción por defecto) permite usar el orden de levels.}

\item{title}{Recibe un título para el gráfico. Si es NULL (opción por defecto), toma la etiqueta de la variable en var.}

\item{unit}{Unidad de observación para declarar el N de la base usada para el gráfico. Si es NULL (opción por defecto), toma el parámeto correspondiente al dataset en data ("data_unit") indicado en los params del yaml. Puede recibir un elemento directamente (no recomendado).}

\item{x_labels}{Define si se mostrarán las etiquetas del eje x (si TRUE). FALSE (opción por defecto) muestra las etiquetas de las categorías de las barras.}

\item{show_notes}{Define si se mostrarán las notas de pie de "N" y el mensaje sobre el redondeo (si TRUE). FALSE por defecto.}

\item{show_n}{Define si se mostrará el N como subtítulo (si TRUE). FALSE por defecto.}
}
\value{
Un objeto ggplot que puede ser exportado como dml.
}
\description{
Gráfico de barras verticales para respuestas únicas
}
