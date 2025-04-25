#' @importFrom rlang caller_env
#' @importFrom rlang enquo
#' @importFrom rlang try_fetch
#' @importFrom rlang quo_get_expr
#' @importFrom rlang quo_get_env
#' @importFrom rlang is_missing
#' @export
try_or_error = function(expr, error_msg, .frame) {
  if (is_missing(.frame)) {
    .frame = caller_env()
  }

  expr = enquo(expr)

  try_fetch(
    eval(quo_get_expr(expr), envir = quo_get_env(expr)),
    error = function(err) {
      cli_abort(
        error_msg,
        parent = err,
        call = .frame,
        .envir = .frame
      )
    }
  )
}


.get = function(x, i) {
  if (!has_name(x, i)) {
    abort(glue("No key named {{.field {i}}}"))
  }
  x[[i]]
}


.r_path = function(fig, global) {
  file.path(global[["script_dir"]], fig[["script"]])
}

.image_path = function(fig, ext, global) {
  file.path(global[["output_dir"]], paste0(fig[["name"]], ".", ext))
}


.save_figures = function(plot, fig, global, .file) {
  try_or_error(
    { 
      cli_inform(c(i = "Saving figure {fig$name}...\n"), class = "immediateCondition")

      # cnd_signal(msg)
      iwalk(global[["ext"]], \(dev, ext) {
        filename = .image_path(fig, ext, global)
        
        ggsave(
          filename,
          plot = plot,
          device = dev,
          units = "mm",
          width = fig$width, 
          height = fig$height,
          create.dir = TRUE
        )
        cli_inform(c(i = "Figure {fig$name} saved as {.file {filename}}!"), class = "immediateCondition")
      })
    },
    "Error while saving figure {fig$name}"
  )
}


.parse_ext = function(dev_name, ext) {
  if (is.null(dev_name) || is.na(dev_name) || dev_name == "") {
    dev_name = ext
  }

  .get_function_from_string(dev_name)
}


.merge = function(x, y) {
  list_assign(x, !!!y)
}


.get_function_from_string <- function(s) {
  # Handle triple-colon and double-colon
  if (grepl(":::", s)) {
    parts <- strsplit(s, ":::")[[1]]
    if (length(parts) != 2) stop("Malformed 'pkg:::fun' format")
    pkg <- parts[1]
    fun <- parts[2]

    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' not available"))
    }
    return(get(fun, envir = asNamespace(pkg), inherits = FALSE))

  } else if (grepl("::", s)) {
    parts <- strsplit(s, "::")[[1]]
    if (length(parts) != 2) stop("Malformed 'pkg::fun' format")
    pkg <- parts[1]
    fun <- parts[2]

    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' not available"))
    }
    return(getExportedValue(pkg, fun))

  } else {
    # Just a plain function name
    if (!exists(s, mode = "function", inherits = TRUE)) {
      stop(paste0("Function '", s, "' not found in global environment or search path"))
    }
    return(get(s, mode = "function", inherits = TRUE))
  }
}
