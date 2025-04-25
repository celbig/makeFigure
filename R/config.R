parse_figure_config = function(config_raw) {
  try_or_error(
    global <- .get(config_raw, ".global"),
    "Unable to find the global configuration!"
  )

  try_or_error(
    script_dir <- .get(global, "script_dir"),
    "The directory for the R scripts is not defined!"
  )

  try_or_error(
    output_dir <- .get(global, "output_dir"),
    "The output directory for the figures is not defined!"
  )

  try_or_error(
    ext <- imap(.get(global, "exts"), .parse_ext),
    "File extensions are not defined!"
  )

  try_or_error(
    dpi <- .get(global, "dpi"),
    "DPI value is not defined!"
  )

  data_dir = pluck(global, "data_dir", .default = file.path(output_dir, "."))
  raw_dir = pluck(
    global,
    "raw_output_dir",
    .default = file.path(output_dir, "raw")
  )
  individual_plot_dir = pluck(
    global,
    "individual_plot_dir",
    .default = file.path(script_dir, "plots")
  )
  figures = compact(imap(
    config_raw,
    parse_figure_config_field,
    global = global
  ))
  if (is_empty(figures)) {
    cli_abort("No valid figure configuration found!")
  }

  width = pluck(global, "width", .default = character())
  height = pluck(global, "height", .default = character())

  config = list(
    global = list(
      script_dir = as.character(script_dir),
      individual_plot_dir = as.character(individual_plot_dir),
      output_dir = as.character(output_dir),
      raw_dir = raw_dir,
      ext = ext,
      dpi = as.numeric(dpi),
      width = width,
      height = height
    ),
    figures = figures
  )

  return(config)
}


parse_figure_config_field = function(field_config, field_name, global) {
  try_or_error(
    {
      if (str_starts(field_name, fixed("."))) {
        config = NULL
        return(invisible())
      }

      # Dimensions of the figure
      height = field_config[["height"]]
      width = field_config[["width"]]
      ratio = field_config[["ratio"]]

      if (is.null(height) && is.null(width)) {
        abort("No height and no width specified")
      }
      if (!is.null(height) && !is.null(width) && !is.null(ratio)) {
        abort("height, width and ratio cannot be specified at the same time")
      }

      accepted_height_values = names(global[["height"]])
      if (is.character(height) && (height %in% accepted_height_values)) {
        if (is(global[["height"]][[height]], "numeric")) {
          height = global[["height"]][[height]]
        } else {
          abort(glue(
            "Configuration error: {.field .global} > {.field height} > {.field {height}} must be a numeric value"
          ))
        }
      }

      accepted_width_values = names(global[["width"]])
      if (is.character(width) && (width %in% accepted_width_values)) {
        if (is(global[["width"]][[width]], "numeric")) {
          width = global[["width"]][[width]]
        } else {
          abort(glue(
            "Configuration error: {.field .global} > {.field width} > {.field {width}} must be a numeric value"
          ))
        }
      }

      if (is.null(height)) {
        height = width / ratio
      }

      if (is.null(width)) {
        width = height * ratio
      }

      script = pluck(
        field_config,
        "script",
        .default = paste0(field_name, ".R")
      )
      script = file.path(global[["script_dir"]], script)

      data = pluck(field_config, "data", .default = list())
      if (!is.list(data)) {
        data = as.list(data)
      }
      params = pluck(field_config, "params", .default = list())
      if (!is.list(params)) {
        params = as.list(params)
      }
      library = pluck(field_config, "library", .default = character())
      if (!is.character(library)) {
        library = as.character(library)
      }
      default_libs = c("ggplot2", "patchwork", "glue")
      library = unique(c(default_libs, library))

      used_keys = c(
        "height",
        "width",
        "ratio",
        "script",
        "data",
        "params",
        "library",
        "individual_plots"
      )
      if (any(!names(field_config) %in% used_keys)) {
        wrong_names = names(field_config)[which(
          !names(field_config) %in% used_keys
        )]
        msg = glue(
          "{{.field {wrong_names}}} entry was ignored in configuration for figure {field_name}"
        )
        names(msg) = rep("i", length = length(msg))
        cli::cli_warn(c(
          "!" = "Only {.field {used_keys}} are valid configuration fields for a figure!",
          msg
        ))
      }

      individual_plots = pluck(
        field_config,
        "individual_plots",
        .default = list()
      )
      individual_plots = compact(imap(
        individual_plots,
        parse_config_plot_field,
        global = global,
        fig_data = data,
        fig_params = params,
        fig_library = library
      ))
      if (is_empty(individual_plots)) {
        abort("No individual plot defined!")
      }

      config = list(
        name = field_name,
        width = width,
        height = height,
        script = script,
        data = data,
        params = params,
        library = library,
        plots = individual_plots
      )
    },
    "Unable to correctly parse configuration for figure {.name {field_name}}!"
  )

  return(config)
}


parse_config_plot_field <- function(
  field_config,
  field_name,
  global,
  fig_data,
  fig_params,
  fig_library
) {
  try_or_error(
    {
      if (is.character(field_config)) {
        if (length(field_config > 1)) {
          abort("A plot cannot be defined by a list of strings!")
        }
        field_config = list(script = field_config)
      }

      if (is.list(field_config) && is.null(names(field_config))) {
        abort(
          "The configuration must be provided as a dictionary object, not unamed list!"
        )
      }

      script = pluck(
        field_config,
        "script",
        .default = paste0(field_name, ".R")
      )
      script = file.path(global[["individual_plot_dir"]], script)

      data = pluck(field_config, "data", .default = list())
      if (!is.list(data)) {
        data = as.list(data)
      }
      data = .merge(fig_data, data)

      params = pluck(field_config, "params", .default = list())
      if (!is.list(params)) {
        params = as.list(params)
      }
      params = .merge(fig_params, params)

      library = pluck(field_config, "library", .default = character())
      if (!is.character(library)) {
        library = as.character(library)
      }

      used_keys = c("script", "data", "params", "library")
      if (any(!names(field_config) %in% used_keys)) {
        wrong_names = names(field_config)[which(
          !names(field_config) %in% used_keys
        )]
        msg = glue(
          "{{.field {wrong_names}}} entry was ignored in configuration for figure {field_name}"
        )
        names(msg) = rep("i", length = length(msg))
        cli::cli_warn(c(
          "!" = "Only {.field {used_keys}} are valid configuration fields for a figure!",
          msg
        ))
      }

      config = list(
        name = field_name,
        script = script,
        params = params,
        data = data,
        library = library
      )
    },
    "Error while parsing plot config for plot {.field {field_name}}"
  )

  return(config)
}
