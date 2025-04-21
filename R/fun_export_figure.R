#' Generate figure from a configuration file
#'
#' @param config_file `string` Path to the configuration file
#' @param figure `string` figure to generate
#' @param theme  `ggplot2::theme` Theme object
#'
#' @return Nothing
#'
#' @details Generate figures and save them as image on disk
#'
#' @export
#' @importFrom furrr furrr_options
make_figure = function(
  config_file,
  figure = NULL,
  theme = ggplot2::theme_get()
) {
  try_or_error(
    {
      config_raw = read_yaml(config_file)
    },
    "Unable to open configuration file {.file {config_file}}!"
  )

  try_or_error(
    {
      config = parse_figure_config(config_raw)
      global = .get(config, "global")
      figures = .get(config, "figures")
    },
    "Unable to parse config from file {.file {config_file}}!"
  )

  if (is.null(figure)) {
    cli_inform("Processing all available figures...")

    progress_bar = progressor(length(figures))
    future_imap(figures, \(figure_config, figure_name) {
      progress_bar(
        glue("Generating figure {figure_name}"),
        class = "sticky",
        amount = 0
      )

      plot = .generate_figure(figure_config, global, theme)
      .save_figures(plot, figure_config, global)

      progress_bar()
    }, .options = furrr_options(seed = NULL))

    cli_inform("All the figures have been generated!")
  } else if (figure %in% names(figures)) {
    figure_config = figures[[figure]]
    plot = .generate_figure(figure_config, global)

    .save_plots(plot, figure_config, global)
  } else {
    cli_error(c(
      x = "Figure {.field {figure}} is not configured!",
      i = "Check configuaration file {.file {config_file}}"
    ))
  }
}