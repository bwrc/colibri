#' Separate the filename and the file extenions
#'
#' @param filename A filename with an extension.
#'
#' @return A list containing the filename and the file extension.
#'
#' @family utilities
#'
#' @keywords internal
get_filename_parts <- function(filename) {
    unlist(strsplit(basename(filename), "[.]"))
}


#' Get the file extension.
#'
#' @param filename A filename with an extension.
#'
#' @return The file extension.
#'
#' @family utilities
#'
#' @keywords internal
get_file_extension <- function(filename) {
    rev(tolower(get_filename_parts(filename)))[1]
}


#' Convert a string to a timestamp
#'
#' @param s A string containing a timestamp.
#' @param timeformat The timeformat used to parse the string. Default is "\%Y\%m\%dT\%H\%M\%S".
#'
#' @return A timestamp.
#'
#' @family utilities
#'
#' @export
str_to_timestamp <- function(s, timeformat = "%Y%m%dT%H%M%S") {
    if (is.null(timeformat))
        stop("Missing time format.")
    as.POSIXct(strptime(as.character(s), timeformat), tz="UTC")
}


#' Convert a numeric value to a timestamp
#' Assumes the numeric values come from a call to as.numeric() on a POSIXct with 
#' time zone tz="UTC"!
#'
#' @param tn A numeric vector representing time in seconds since the epoch ‘1970-01-01 00:00.00 UTC’
#'
#' @return A vector of timestamps of class POSIXct
#'
#' @family utilities
#'
#' @export
num_to_timestamp <- function(tn) {
  as.POSIXct(tn, origin = as.POSIXct("1970-01-01", tz = "UTC"), tz = "UTC")
}


#' Convert a timestamp to a string
#'
#' @param ts A POSIXct timestamp.
#' @param timeformat The timeformat used to format the timestamp as a string. Default is "\%Y\%m\%dT\%H\%M\%S".
#'
#' @return A string.
#'
#' @family utilities
#'
#' @export
timestamp_to_str <- function(ts, timeformat = "%Y%m%dT%H%M%S") {
    strftime(ts, format = timeformat, tz="UTC")
}


#' Convert a factor to a string or numeric representation.
#'
#' @param x A factor
#' @param type A string indicating the type of the factor: "numeric" (default) or "string).
#'
#' @return A string representation of the factor.
#'
#' @family utilities
#'
#' @export
defactor <- function(x, type = "numeric") {
    switch(type,
           numeric = return(as.numeric(as.character(x))),
           string  = return(as.character(x))
           )
}


#' Convert a timestamp to seconds from the start of the recording.
#'
#' @param recording A recording structure.
#' @param t0 The timestamp (as a string or as a POSIXct timestamp).
#' @param timeformat The timeformat used to parse the string (optional). Default is "\%Y\%m\%dT\%H\%M\%S".
#' @param return.indices Boolean indicating whether the indices in the
#' \code{signal} corresponding to the timestamp should be
#' returned. Optional, default is FALSE.
#' @param signal A signal name found in the recording. Optional, default is NULL.
#'
#' @return The number of seconds corresponding to the timestamp, or if
#' return.indices = TRUE, a two-element list containing the seconds
#' and the indices corresponding to the timestamp.
#'
#' @family utilities
#'
#' @export
timestamp_to_seconds <- function(recording, t0, timeformat = "%Y%m%dT%H%M%S", return.indices = FALSE, signal = NULL) {
    ## Calculate differences in seconds
    if (class(t0)[1] != "POSIXct")
        t0 <- str_to_timestamp(t0, timeformat)

    ## Some sanity checks
    if (t0 < recording$properties$time.start)
        stop("Start timestamp too early, recording has not begun yet.")
    if (t0 > recording$properties$time.stop) {
        warning("Stop timestamp exceeds recording, using recording end time instead.")
        t0 <- recording$properties$time.stop
    }

    t0s <- difftime(t0, recording$properties$time.start, units = "secs")
    out <- as.numeric(t0s)

    ## Find the indices in the IBI vector corresponding to these times
    if (return.indices) {
        if (is.null(signal))
            stop("No signal name provided!")
        out <- list(out, seconds_to_indices(recording, t0s, t1s, signal = signal))
    }

    out

}


#' Convert a time interval defined by two timestamps to seconds from the start of the recording.
#'
#' @param recording A recording structure.
#' @param t0 The start timestamp (as a string or as a POSIXct timestamp).
#' @param t1 The stop timestamp (as a string or as a POSIXct timestamp).
#' @param timeformat The timeformat used to parse the strings (optional). Default is "\%Y\%m\%dT\%H\%M\%S".
#' @param return.indices Boolean indicating whether the indices in the
#' \code{signal} corresponding to the time intervalshould be
#' returned. Optional, default is FALSE.
#' @param signal A signal name found in the recording. Optional, default is NULL.
#'
#' @return A two-element list with The number of seconds corresponding to the start and stop timestamp, or if
#' return.indices = TRUE, a four-element list containing the seconds
#' and the indices corresponding to the time interval.
#'
#' @family utilities
#'
#' @export
timeintervals_to_seconds <- function(recording, t0, t1, timeformat = NULL, return.indices = FALSE, signal = NULL) {
    ## Some sanity checks
    if (t0 < recording$properties$time.start)
        stop("Start timestamp too early, recording has not begun yet.")
    if (t1 > recording$properties$time.stop) {
        warning("Stop timestamp exceeds recording, using recording end time instead.")
        t1 <- recording$properties$time.stop
    }

    ## Calculate differences in seconds
    if (class(t0)[1] != "POSIXct")
        t0 <- str_to_timestamp(t0, timeformat)
    if (class(t1)[1] != "POSIXct")
        t1 <- str_to_timestamp(t1, timeformat)

    t0s <- difftime(t0, recording$properties$time.start, units = "secs")
    t1s <- difftime(t1, recording$properties$time.start, units = "secs")
    out <- c(t0s, t1s)

    ## Find the indices in the IBI vector corresponding to these times
    if (return.indices) {
        if (is.null(signal))
            stop("No signal name provided!")
        out <- list(out, seconds_to_indices(recording, t0s, t1s, signal = signal))
    }

    out
}


#' Get the indices corresponding to the times t0 and t1 given in seconds.
#'
#' @param recording A recording structure.
#' @param t0 The start in seconds, from the start of the recording.
#' @param t1 The stop timestamp (as a string or as a POSIXct timestamp). Optional, default is NULL
#' @param signal A signal name found in the recording. Optional, default is NULL.
#'
#' @return The index corresponding to t0, or if t1 is also given, a two-element list with indices corresponding to t0 and t1.
#'
#' @family utilities
#'
#' @export
seconds_to_indices <- function(recording, t0, t1 = NULL, signal = NULL) {
    if (is.null(signal))
        stop("No signal name provided!")

    ind1 <- which(recording$signal[[signal]]$t >= t0)[1]

    if (! is.null(t1)) {
        ind2 <- rev(which(recording$signal[[signal]]$t <= t1))[1]
        c(ind1, ind2)
    } else {
        ind1
    }

}


#' Extract a segment of a signal corresponding to the given indices.
#'
#' @param recording A recording structure.
#' @param ind1 Start index or a two-element list giving the start and stop index.
#' @param ind2 Stop index. Optional, default is NULL, in which case ind1 must a two-element list.
#' @param signal A signal name found in the recording. Optional, default is NULL.
#'
#' @return A named list with the fields "data" and "t" from \code{signal} corresponding to the given indices.
#'
#' @family utilities
#'
#' @export
extract_segment <- function(recording, ind1, ind2 = NULL, signal = NULL) {
    if (is.null(signal))
        stop("No signal name provided!")

    if (length(ind1) == 2) {
        ind2 <- ind1[2]
        ind1 <- ind1[1]
    }

    out           <- recording$signal[[signal]]

    if (all(is.na(out))) {
        out[["data"]] <- c()
        out[["t"]]    <-c()
    } else {
        out[["data"]] <- recording$signal[[signal]]$data[ind1:ind2]
        out[["t"]]    <- recording$signal[[signal]]$t[ind1:ind2]
    }

    out
}


#' Extract a segment of a signal corresponding to the given times t0 and t1, given as seconds from the start of the recording.
#'
#' @param recording A recording structure.
#' @param t0 Start time (in seconds) or a two-element list giving the start and stop time.
#' @param t1 Stop time. Optional, default is NULL, in which case ind1 must a two-element list.
#' @param signal A signal name found in the recording. Optional, default is NULL.
#'
#' @return A named list with the fields "data" and "t" from \code{signal} corresponding to the given times.
#'
#' @family utilities
#'
#' @export
extract_segment_s <- function(recording, t0, t1, signal = NULL) {
    if (is.null(signal))
        stop("No signal name provided!")

    if (length(t0) == 2) {
        t1 <- t0[2]
        t0 <- t0[1]
    }

    ind <- seconds_to_indices(recording, t0, t1, signal = signal)
    extract_segment(recording, ind, signal = signal)
}


#' Extract a segment of a signal corresponding to the given timestamps ts0 and ts1.
#'
#' @param recording A recording structure.
#' @param ts0 Start timestamp or a two-element list giving the start and stop timestamps.
#' @param ts1 Stop timestamp. Optional, default is NULL, in which case ind1 must a two-element list.
#' @param signal A signal name found in the recording. Optional, default is NULL.
#'
#' @return A named list with the fields "data" and "t" from \code{signal} corresponding to the given timestamps.
#'
#' @family utilities
#'
#' @export
extract_segment_timestamp <- function(recording, ts0, ts1 = NULL, signal = NULL) {
    if (is.null(signal))
        stop("No signal name provided!")

    if (length(ts0) == 2) {
        ts1 <- ts0[[2]]
        ts0 <- ts0[[1]]
    }

    res <- timeintervals_to_seconds(recording, ts0, ts1, return.indices = TRUE, signal = signal)
    ind <- res[[2]]
    extract_segment(recording, ind, signal = signal)
}


#' Extract the segment of a signal corresponding to the given block ID.
#'
#' @param recording A recording structure.
#' @param blockid A numeric block ID.
#' @param signal A signal name found in the recording. Optional, default is NULL.
#'
#' @return A named list with the fields "data" and "t" from \code{signal} corresponding to the given block ID.
#'
#' @family utilities
#'
#' @export
extract_segment_block <- function(recording, blockid, signal = NULL) {
    if (is.null(signal))
        stop("No signal name provided!")

    blockid.tmp <- blockid
    block <- subset(recording$conf$blocks, blockid == blockid.tmp)

    ## Need to convert all timestamps to seconds for the extraction
    ## --- start time
    if (block$starttype == "timestamp")
        t0 <- timestamp_to_seconds(recording = recording, t0 = block$starttime, signal = signal)
    if (block$starttype == "time")
        t0 <- block$starttime

    ## --- stop time
    if (block$stoptype == "timestamp")
        t1 <- timestamp_to_seconds(recording = recording, t0 = block$stoptime, signal = signal)
    if (block$stoptype == "time")
        t1 <- block$stoptime

    ind <- seconds_to_indices(recording, t0, t1, signal = signal)
    extract_segment(recording, ind, signal = signal)
}


#' Generate analysis segments given a block.
#'
#' If \code{segmentlength} is zero, the entire block length is used
#' and no segments are created. The tolerance is given in seconds, and
#' defaults to 1 second. The tolerance affects the last segment. If
#' the last segment would, e.g., extend past the data, the tolerance
#' parameter can be used to adjust when a slightly-shorter segment is
#' accepted, and when the entire segment must be discarded. The
#' segment length and overlap are read from the \code{settings}
#' structure.
#'
#' @param block A block structure
#' @param settings The settings structure used for HRV analysis.
#' @param tolerance The tolerance of the last segment length in seconds. Optional, default is 1.
#'
#' @return A two-column matrix with the start time (in seconds) of
#' each segment in the first column and the stop time (in seconds) in
#' the second column.
#'
#' @family utilities
#'
#' @export
generate_segments_from_block <- function(block, settings, tolerance = 1) {
    generate_segments(block$starttime, block$stoptime, segment.length = settings$segment.length, segment.overlap = settings$segment.overlap)
}


#' Generate analysis segments given a start and stop time.
#'
#' If \code{segmentlength} is zero, the entire block length is used
#' and no segments are created. The tolerance is given in seconds, and
#' defaults to 1 second. The tolerance affects the last segment. If
#' the last segment would, e.g., extend past the data, the tolerance
#' parameter can be used to adjust when a slightly-shorter segment is
#' accepted, and when the entire segment must be discarded.
#'
#' @param time.start A start time in seconds.
#' @param segment.length The segment length
#' @param segment.overlap The segment overlap
#' @param time.stop A stop time in seconds.
#' @param blockid A numeric block ID.
#' @param tolerance The tolerance of the last segment length in seconds. Optional, default is 1.
#'
#' @return A two-column matrix with the start time (in seconds) of
#' each segment in the first column and the stop time (in seconds) in
#' the second column.
#'
#' @family utilities
#'
#' @export
generate_segments <- function(time.start, time.stop, segment.length, segment.overlap, tolerance = 1) {

    if (tolerance > 0)
        time.stop <- time.stop + tolerance

    if (time.start < 0)
        time.stop <- time.stop - time.start - segment.length

    if (segment.length == 0) {

        segments      <- matrix(c(time.start, time.stop), nrow = 1, ncol = 2, byrow = TRUE)

    } else {

        ## Generate non-overlapping segments
        seg.start <- seq(from = time.start, to = time.stop - segment.length, by = segment.length)

        ## Generate overlapping segments
        if (segment.overlap > 0) {
            seg.start <- sort(seq(from = time.start, to = time.stop - segment.length, by = (segment.length - segment.overlap)))
        }

        segments     <- matrix(data = seg.start, nrow = length(seg.start), ncol = 2, byrow = FALSE)
        segments[,2] <- segments[,2] + segment.length

        ## Adjust the time limits of the last block
        if (tolerance > 0)
            segments[nrow(segments), 2] <- time.stop - tolerance
    }

    segments
}


#' Generate a result row to be used as a template for the results, based on the given block.
#'
#' @param block A block structure.
#'
#' @return A data frame row to be used as a template for results.
#'
#' @family utilities
#'
#' @export
generate_result_row <- function(block) {
    drop.column.list <- c("dataid", "starttype", "starttime", "stoptype", "stoptime")
    block$segment    <- NA
    block$variable   <- NA
    block$value      <- NA
    block[, ! names(block) %in% drop.column.list, drop = FALSE]
}


#' Replicate a row in a data frame N times returning a new data frame
#'
#' @param row A data frame row.
#' @param N An integer giving the number of times the data frame row should be repeated.
#'
#' @return A data frame with N rows.
#'
#' @family utilities
#'
#' @export
replicate_df_row <- function(row, N) {
    row[rep(seq_len(nrow(row)), N), ]
}


#' Integrate using the trapezoid rule
#'
#' @param f A vector of frequencies.
#' @param p A vector of power values corresponding to the frequency values.
#' @param fmin The minimum frequency value.
#' @param fmax The maximum frequency value.
#'
#' @return The integrated power.
#'
#' @family utilities
#'
#' @export
integrate_power <- function(f, p, fmin, fmax) {
    if (length(fmin) == 2) {
        fmax <- fmin[2]
        fmin <- fmin[1]
    }

    imin <- head(which(f >= fmin), 1)
    imax <- head(which(f >= fmax), 1)

    if (FALSE) {
        h <- (fmax - fmin) / (imax - imin)
        n <- length(p)

        ## h * sum(p[1:(n-1)] + p[2:(n)]) / 2
        h * sum(p[imin:(imax-1)] + p[(imin + 1):(imax)]) / 2
    }
    require(caTools)
    trapz(f[imin:imax], p[imin:imax])
}


#' Given an ECG signal, form the IBI series
#'
#' @param recording A recording structure.
#' @param signal A signal name found in the recording. Optional, default is "ECG".
#' @param interpolate.qrs Should the position of the R-peak be interpolated. See \code{\link{qrs_interpolate}} for details.
#' @param ... Additional arguments to \code{\link{qrs_detect}}.
#'
#' @return The recording structure with the new signals "ibi" and "ibi.amp" added.
#'
#' @family utilities
#'
#' @export
ibi_from_ecg <- function(recording, signal = "ECG", interpolate.qrs = TRUE, ...) {
    r.peaks   <- qrs_detect(recording$signal[[signal]][["data"]], recording$signal[[signal]][["samplingrate"]], interpolate = interpolate.qrs, ...)
    ibi       <- diff(1000 * (r.peaks[,1] / recording$signal$ECG$samplingrate))

    ibi.t <- r.peaks[,1] / recording$signal$ECG$samplingrate
    ibi.t <- ibi.t[-length(ibi)]

    recording <- recording_set_ibi(recording, ibi = ibi, ibi.t = ibi.t)

    recording$signal$ibi.amp$data <- r.peaks[,2]
    recording$signal$ibi.amp$t    <- r.peaks[,1] / recording$signal$ECG$samplingrate

    recording
}


#' Add a time series with the instantaneous heart rate to a recording.
#'
#' @param recording A recording structure.
#' @param signal A signal name containing interbeat intervals in the recording. Optional, default is "ibi".
#'
#' @return The recording structure with the new signal "hr" added.
#'
#' @family utilities
#'
#' @export
hr_from_ibi <- function(recording, signal = "ibi") {
    if (signal %in% names(recording$signal)) {
        recording$signal$hr$data <- 60e3 / recording$signal$ibi$data
        recording$signal$hr$t    <- recording$signal$ibi$t
        recording$signal$hr$unit <- "bpm"
    } else {
        stop("Signal name not found!")
    }

    recording
}


#' Ensure that a directory path exists. If the given path does not exist, it is created.
#'
#' @param x A path
#' @param recursive Boolean indicating whether the path should be created recursively. Default is FALSE.
#'
#' @return Nothing.
#'
#' @family utilities
#'
#' @export
ensure_path <- function(x, recursive = FALSE) {
    x <- gsub("//", "/", x)
    if (! file.exists(x))
        dir.create(x, recursive = TRUE)
}
