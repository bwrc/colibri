% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizations.R
\name{plot_all_blocks}
\alias{plot_all_blocks}
\title{Plot signal data and also show block limits and tasks types for each of the blocks.}
\usage{
plot_all_blocks(recording, signal, type = "seconds", interval = 15,
  new.plot = TRUE, filename = NULL)
}
\arguments{
\item{recording}{A recording structure.}

\item{signal}{The name of the signal in the recording structure to be plotted.}

\item{type}{The type of the x-axis (optional, default is seconds):
\describe{
\item{seconds}{Plot seconds on the x-axis.}
\item{timestamp}{Plot timestamps on the x-axis.}
}}

\item{interval}{The interval at which to plot x-axis markers. Optional, default is 15.}

\item{new.plot}{Boolean indicating whether a new plot window should be opened (optional, default is TRUE).}

\item{filename}{The full path and filename in which to save the figure. If NULl (default), the figure is not saved. The format of the figure (PDF or PNG) is automatically determined from the filename extension. The size of the figure is an A4 in landscape mode.}
}
\value{
Nothing. Produces a plot or saves the figure.
}
\description{
Plot signal data and also show block limits and tasks types for each of the blocks.
}
\seealso{
Other visualizations: \code{\link{plot_block}},
  \code{\link{plot_check_rr_detection}},
  \code{\link{plot_ecg_r_peak}}, \code{\link{plot_metric}},
  \code{\link{plot_spectrum}},
  \code{\link{shade_frequency_band}}
}

