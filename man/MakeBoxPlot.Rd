% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakeBoxPlot.R
\name{MakeBoxPlot}
\alias{MakeBoxPlot}
\title{MakeBoxPlot}
\usage{
MakeBoxPlot(
  SampleStatistics,
  AxisLimits_y = c(NA_integer_, NA_integer_),
  AxisTitle_x = "",
  AxisTitle_y = "",
  TickLabelWidth_x = 10,
  Decimals = 0,
  ggTheme = function(...) dsCCPhosClient::ggTheme_CCP(...),
  ThemeArguments = list(),
  FillPalette = NULL,
  ...
)
}
\arguments{
\item{SampleStatistics}{Data frame | Sample statistics as generated by \code{\link{ds.GetSampleStatistics}}}

\item{AxisLimits_y}{Vector | Containing two integer values | Default: Auto y axis limits}

\item{Decimals}{Number of decimals of y axis number format}

\item{ggTheme}{Pass custom theme}

\item{ThemeArguments}{Pass custom theme arguments}

\item{...}{}
}
\value{
a ggplot2 object
}
\description{
Make one or more box plots on the basis of sample statistics as generated by \code{\link{ds.GetSampleStatistics}}. Makes use of ggplot2.
}
\author{
Bastian Reiter
}
