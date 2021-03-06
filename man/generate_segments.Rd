% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{generate_segments}
\alias{generate_segments}
\title{Generate analysis segments given a start and stop time.}
\usage{
generate_segments(time.start, time.stop, segment.length, segment.overlap,
  tolerance = 1)
}
\arguments{
\item{time.start}{A start time in seconds.}

\item{time.stop}{A stop time in seconds.}

\item{segment.length}{The segment length in seconds.}

\item{segment.overlap}{The segment overlap in seconds.}

\item{tolerance}{The tolerance of the last segment length in seconds. Optional, default is 1.}
}
\value{
A two-column matrix with the start time (in seconds) of
each segment in the first column and the stop time (in seconds) in
the second column.
}
\description{
If \code{segmentlength} is zero, the entire block length is used
and no segments are created. The tolerance is given in seconds, and
defaults to 1 second. The tolerance affects the last segment. If
the last segment would, e.g., extend past the data, the tolerance
parameter can be used to adjust when a slightly-shorter segment is
accepted, and when the entire segment must be discarded.
}
\seealso{
Other utilities: \code{\link{defactor}},
  \code{\link{ensure_path}},
  \code{\link{extract_segment_block}},
  \code{\link{extract_segment_s}},
  \code{\link{extract_segment_timestamp}},
  \code{\link{extract_segment}},
  \code{\link{generate_result_row}},
  \code{\link{generate_segments_from_block}},
  \code{\link{get_file_extension}},
  \code{\link{get_filename_parts}},
  \code{\link{hr_from_ibi}}, \code{\link{ibi_from_ecg}},
  \code{\link{integrate_power}},
  \code{\link{num_to_timestamp}},
  \code{\link{replicate_df_row}},
  \code{\link{seconds_to_indices}},
  \code{\link{str_to_timestamp}},
  \code{\link{timeintervals_to_seconds}},
  \code{\link{timestamp_to_seconds}},
  \code{\link{timestamp_to_str}}
}

