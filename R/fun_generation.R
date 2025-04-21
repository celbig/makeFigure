.generate_figure = function(figure, global, theme) {
  try_or_error(
    {
      .f = \(plot_list, figure, global) {
        # Setting up the environnement for plot generation
        makeFIgures.BM.project::try_or_error(
          {
            purrr::walk(figure[["library"]], base::library, character.only = TRUE)

            exec_env = rlang::new_environment(parent = rlang::global_env())
            makeFIgures.BM.project::set_shims(exec_env)
            purrr::walk(figure[["data"]], load, envir = exec_env)
            purrr::iwalk(plot_list, \(p, nm)rlang::env_poke(exec_env, nm, p))
          },
          "Error while setting up execution environnement for Figure {.field {figure$name}}!"
        )

        # Call the plot script
        makeFIgures.BM.project::try_or_error(
          {
            source(figure[["script"]], local = exec_env)
          },
          "Error in execution of plot figure script {.file {figure$script}}!"
        )

        # Ensure the result is sound (the plot variable should be in `p`)
        makeFIgures.BM.project::try_or_error(
          {
            if (!rlang::env_has(env = exec_env, nms = "p")) {
              rlang::abort("No object named {.var {p}}")
            }

            plot_object = rlang::env_get(exec_env, nm = "p")
            if (!patchwork:::is_valid_plot(plot_object)) {
              rlang::abort(c(
                "The variable {.var {p}} is not a correct plot object",
                "{.var {p}} must be either a ggplot or grob object..."
              ))
            }
          },
          "Invalid plot object after execution of plot plot script {.file {script}}"
        )

        return(plot_object)
      }

      plot_list = map(
        figure[["plots"]],
        .generate_plot,
        fig = figure,
        global = global
      )
      names(plot_list) = figure[["plots"]]

      try_or_error(
        {
          plot_object = r(.f, args = list(plot_list, figure, global), stdout = "log")
        },
        "Error while running figure script {.field {figure$script}}"
      )

      try_or_error(
        {
          plot_object = patchwork::wrap_elements(plot_object) & theme
        },
        "Error while applying theme!"
      )

      return(plot_object)
    },
    "Error while generating plot for figure {figure$name}"
  )
}


.generate_plot = function(plot, fig, global) {
  .f = \(script, libraries, data, params) {
    # Setting up the environnement for plot generation
    makeFIgures.BM.project::try_or_error(
      {
        purrr::walk(libraries, base::library, character.only = TRUE)

        exec_env = rlang::new_environment(
          list(params = params),
          parent = rlang::global_env()
        )
        makeFIgures.BM.project::set_shims(exec_env)
        purrr::walk(data, load, envir = exec_env)
      },
      "Error while setting up execution environnement for plot {.field {plot}}!"
    )

    # Call the plot script
    makeFIgures.BM.project::try_or_error(
      {
        source(script, local = exec_env)
      },
      "Error in execution of plot script {.file {script}}!"
    )

    # Ensure the result is sound (the plot variable should be in `p`)
    makeFIgures.BM.project::try_or_error(
      {
        if (!rlang::env_has(env = exec_env, nms = "p")) {
          rlang::abort("No object named {.var {p}}")
        }

        plot_object = rlang::env_get(exec_env, nm = "p")
        if (!patchwork:::is_valid_plot(plot_object)) {
          rlang::abort(c(
            "The variable {.var {p}} is not a correct plot object",
            "{.var {p}} must be either a ggplot or grob object..."
          ))
        }
      },
      "Invalid plot object after execution of plot plot script {.file {script}}"
    )

    return(plot_object)
  }

  ## call anonymous function with callr
  plot_script = file.path(global$individual_plot_dir, paste0(plot, ".R"))
  try_or_error(
    {
      plot_object = r(
        .f,
        args = list(
          script = plot_script,
          libraries = fig$library,
          params = fig$params,
          data = fig$data
        ), 
      )
    },
    "Error while running plot script {.field {plot_script}}"
  )

  if (
    inherits(plot_object$theme, "theme") && isTRUE(plot_object$theme$complete)
  ) {
    cli_warn(c(
      "!" = "The plot {.field {plot}} uses a complete theme",
      i = "A global theme will be applied instead"
    ))
  }

  return(plot_object)
}
