#' Muestra N de respuestas y nota al pie de página
#'

get_n <- function(data,
                  ...,
                  vars = NULL) {
  # Caso 1: si `data` es una lista nombrada (múltiples dataframes)
  if (is.list(data) && !inherits(data, "data.frame")) {
    purrr::map_dfr(
      .x = names(data),
      .f = function(name) {
        df <- get(name, envir = parent.frame())  # recupera el objeto por nombre
        vars <- data[[name]]
        df %>%
          dplyr::select(all_of(vars)) %>%
          purrr::map_dfr(~ sum(!is.na(.)), .id = "variable") %>%
          dplyr::mutate(público = name, .before = 1)
      }
    )
  } else {
    # Caso 2: data es un solo data.frame (pipe)
    # Capturar el nombre del dataframe de manera más simple
    nombre_data <- tryCatch({
      # Obtener todas las llamadas del stack
      calls <- sys.calls()
      found_name <- NULL

      # Buscar la llamada más reciente que contenga %>%
      for (i in length(calls):1) {
        call_str <- paste(deparse(calls[[i]]), collapse = "")
        if (grepl("%>%", call_str)) {
          # Extraer todo antes del primer %>%
          before_pipe <- sub("\\s*%>%.*", "", call_str)
          found_name <- gsub("^\\s+|\\s+$", "", before_pipe)  # trim
          break
        }
      }

      if (!is.null(found_name)) {
        found_name
      } else {
        # Si no hay pipe, usar el método original
        sub_name <- deparse(substitute(data))
        if (sub_name == ".") {
          "data"
        } else {
          sub_name
        }
      }
    }, error = function(e) {
      "data"
    })

    quos_vars <- rlang::enquos(...)

    # Permitir usar `vars = c(...)` o tidyselect (`...`)
    if (!is.null(vars)) {
      selected_data <- dplyr::select(data, dplyr::all_of(vars))
    } else if (length(quos_vars) > 0) {
      selected_data <- dplyr::select(data, !!!quos_vars)
    } else {
      rlang::abort("Debes especificar columnas con `vars = c(...)` o usar `...`.")
    }

    selected_data %>%
      purrr::map_dfr(~ sum(!is.na(.)), .id = "variable") %>%
      dplyr::mutate(público = nombre_data, .before = 1)
  }
}
