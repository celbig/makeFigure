.generate_figure = function(figure, global, theme) {

  try_or_error(
    {
      exec_env = env()
      try_or_error(
        {
          plot_object = .with_exec_environment(
            {
              plot_list = map(
                figure[["plots"]],
                .generate_plot
              )
              names(plot_list) = map(figure[["plots"]], "name")
              attach(plot_list, name = ".plot_list")

              # Call the figure script
              env = rlang::current_env()
              try_or_error(
                {
                  sys.source(
                    .script,
                    envir = env,
                    keep.source = FALSE,
                    keep.parse.data = FALSE
                  )
                },
                "Error in execution of plot figure script {.file {(.script)}}!"
              )
              detach(name = ".plot_list", character.only = TRUE)

              # Ensure the result is sound (the plot variable should be in `p`)
              try_or_error(
                {
                  if (!rlang::env_has(env = env, nms = "p")) {
                    rlang::abort("No object named {.var {p}}")
                  }

                  plot_object = rlang::env_get(env, nm = "p")
                  if (!patchwork:::is_valid_plot(plot_object)) {
                    rlang::abort(c(
                      "The variable {.var {p}} is not a correct plot object",
                      "{.var {p}} must be either a ggplot or grob object..."
                    ))
                  }
                },
                "Invalid plot object after execution of plot plot script {.file {(.script)}}"
              )

              plot_object
            },
            packages = figure$library,
            data = figure$data,
            objects = list(.params = figure$params, .script = figure$script),
            env = exec_env
          )
        },
        "Error while running figure script {.field {figure$script}}"
      )

      try_or_error(
        {
          plot_object = patchwork::wrap_elements(plot_object) & theme
        },
        "Error while applying theme!"
      )
    },
    "Error while generating figure {figure$name}"
  )

  return(plot_object)
}

#' @importFrom rlang current_env
#' @importFrom rlang env_has
#' @importFrom rlang env_get
#' @importFrom rlang abort
.generate_plot = function(plot) {
  exec_env = env()
  try_or_error(
    {
      plot_object = .with_exec_environment(
        {
          # Call the plot script
          env = rlang::current_env()
          cli::cli_inform("Processing plot {.field {plot$name}}", class = "immediateCondition")
          try_or_error(
            {
              sys.source(
                .script,
                envir = env,
                keep.source = FALSE,
                keep.parse.data = FALSE
              )
            },
            "Error in execution of plot script {.file {(.script)}}!"
          )

          # Ensure the result is sound (the plot variable should be in `p`)
          try_or_error(
            {
              if (!env_has(env = env, nms = "p")) {
                abort("No object named {.var {p}}")
              }

              plot_object = env_get(env, nm = "p")
              if (!patchwork:::is_valid_plot(plot_object)) {
                abort(c(
                  "The variable {.var {p}} is not a correct plot object",
                  "{.var {p}} must be either a ggplot or grob object..."
                ))
              }
            },
            "Invalid plot object after execution of plot plot script {.file {script}}"
          )

          plot_object
        },
        packages = plot$library,
        data = plot$data,
        objects = list(.script = plot$script, .params = plot$params),
        env = exec_env
      )
    },
    "Error while running plot script {.field {plot$script}}"
  )

  if (
    inherits(plot_object$theme, "theme") && isTRUE(plot_object$theme$complete)
  ) {
    cli_warn(c(
      "!" = "The plot {.field {plot$name}} uses a complete theme",
      i = "A global theme will be applied instead"
    ))
  }

  return(plot_object)
}
