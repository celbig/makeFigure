% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fun_make_figure.R
\name{make_figure}
\alias{make_figure}
\title{Generate figure from a configuration file}
\usage{
make_figure(config_file, figure = NULL, theme = ggplot2::theme_get())
}
\arguments{
\item{config_file}{\code{string} Path to the configuration file}

\item{figure}{\code{string} figure to generate}

\item{theme}{\code{ggplot2::theme} Theme object}
}
\value{
Nothing
}
\description{
Generate figure from a configuration file
}
\details{
Generate figures and save them as image on disk
}
