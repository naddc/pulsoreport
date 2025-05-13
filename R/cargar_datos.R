#' @importFrom haven read_sav
#' @export
cargar_datos <- function(params) {
  nombres_validos <- names(params)[!grepl("_unit$", names(params))]

  for (nombre in nombres_validos) {
    filepath <- here::here(params[[nombre]])

    if (file.exists(filepath)) {
      message(paste("Cargando", nombre, "desde", filepath))
      assign(nombre, haven::read_sav(filepath), envir = .GlobalEnv)
    } else {
      message(paste("Archivo no encontrado en", filepath, "- Buscando en paquete 'pulsoreport'..."))
      filepath_pkg <- system.file(file.path('extdata', params[[nombre]]), package = "pulsoreport")

      if (file.exists(filepath_pkg) && nzchar(filepath_pkg)) {
        message(paste("Cargando", nombre, "desde el paquete 'pulsoreport' en", filepath_pkg))
        assign(nombre, haven::read_sav(filepath_pkg), envir = .GlobalEnv)
      } else {
        message(paste("Error: Archivo no encontrado ni en local ni en el paquete para", nombre))
      }
    }
  }
}
