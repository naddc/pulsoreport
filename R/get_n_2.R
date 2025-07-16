get_n_2 <- function(data,
                  ...,
                  vars = NULL) {

  # Caso 1: data es una lista nombrada (varios data.frames)
  if (is.list(data) && !inherits(data, "data.frame")) {
    purrr::map_dfr(
      .x = names(data),
      .f = function(name) {
        df <- get(name, envir = parent.frame())
        vars_local <- data[[name]]
        df %>%
          dplyr::select(all_of(vars_local)) %>%
          purrr::map_dfr(~ sum(!is.na(.)), .id = "variable") %>%
          tidyr::pivot_wider(names_from = variable, values_from = value) %>%
          dplyr::mutate(público = name, .before = 1)
      }
    )
  } else {

    # ⚠ Capturar nombre del data.frame incluso si viene desde show_notes()
    nombre_data <- tryCatch({
      call <- match.call(definition = sys.function(-1), call = sys.call(-1))
      if ("data" %in% names(call)) {
        as.character(call$data)
      } else {
        sub_name <- deparse(substitute(data))
        if (sub_name == ".") {
          # Si es desde un pipe, buscar más atrás
          calls <- sys.calls()
          for (i in length(calls):1) {
            call_str <- paste(deparse(calls[[i]]), collapse = "")
            if (grepl("%>%", call_str)) {
              before_pipe <- sub("\\s*%>%.*", "", call_str)
              return(gsub("^\\s+|\\s+$", "", before_pipe))
            }
          }
          "."
        } else {
          sub_name
        }
      }
    }, error = function(e) {
      "data"
    })

    # Elegir variables
    quos_vars <- rlang::enquos(...)
    selected_data <- if (!is.null(vars)) {
      dplyr::select(data, dplyr::all_of(vars))
    } else if (length(quos_vars) > 0) {
      dplyr::select(data, !!!quos_vars)
    } else {
      rlang::abort("Debes especificar columnas con `vars = c(...)` o usar `...`.")
    }

    selected_data %>%
      purrr::map_dfc(~ sum(!is.na(.))) %>%
      dplyr::mutate(público = nombre_data, .before = 1)
  }
}
