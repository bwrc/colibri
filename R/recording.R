#' Return and initialise an empty recording structure.
#' The recording structure is a list.
#'
#' @return An empty recording structure.
#'
#' @family recording
#' 
#' @export
new.recording <- function() {
    ## Create containers
    recording            <- list()
    recording$properties <- list()
    recording$conf       <- list()
    recording$signal     <- list()
    recording$results    <- list()

    recording$properties$time.start.raw <- NA
    recording$properties$time.start     <- NA
    recording$properties$time.stop.raw  <- NA
    recording$properties$time.stop      <- NA

    ## Set subject and casename information
    recording$properties$subject        <- NA
    recording$properties$casename       <- NA

    ## Database information
    recording$properties$db.filename    <- NA

    ## Information on the data format, e.g., if the data
    ## is from a Suunto Device etc.
    recording$properties$format         <- NA
    recording$properties$format.long    <- NA
    recording$properties$device.type    <- NA
    recording$properties$device.version <- NA

    ## The length of the recording in seconds
    recording$properties$length         <- NA

    ## The zerotime that anchors time operations on the recording
    recording$properties$zerotime   <- NA
    recording$properties$zerotime.s <- 0

    ## Store block information
    recording$conf$blocks   <- NA
    recording$conf$events   <- NA
    recording$conf$segments <- NA

    recording

}


#' Store the interbeat interval data series in the recording structure
#'
#' @param recording The recording structure
#' @param ibi An array with the interbeat intervals
#' @param ibi.t An optional array with the times of occurrence of the interbeat intervals.
#'
#' @return The recording structure with the interbeat intervals added.
#' 
#' @family recording
#' 
#' @export
recording.set.ibi <- function(recording, ibi, ibi.t = NULL) {
    ## Set the ibi
    recording$signal$ibi$data <- ibi

    if (sum(recording$signal$ibi$data[1:2]) < 100)
        recording$signal$ibi$data <- 1000 * recording$signal$ibi$data

    ## Create time vector
    if (! is.null(ibi.t))
        recording$signal$ibi$t <- ibi.t
    else
        recording$signal$ibi$t <- c(0, cumsum(recording$signal$ibi$data[1:(length(recording$signal$ibi$data)-1)])) / 1000

    ## Set unit
    recording$signal$ibi$unit <- "ms"

    ## Calculate the length of the recording
    recording$properties$length <- (sum(recording$signal$ibi$data) - recording$signal$ibi$data[1]) / 1000

    if (is.null(recording$properties$time.stop))
        recording$properties$time.stop <- recording$properties$time.start + recording$properties$length

    recording
}


#' Store the block information inside the recording structure
#'
#' @param filename The database filename. Optional. If not provided it is determined from the field \code{recording$properties$db.filename}.
#' @param casename The casename. Optional. If not provided it is determined from the field \code{recording$properties$casename}.
#'
#' @return The recording structure with the block information added.
#' 
#' @family recording
#' 
#' @export
recording.set.blocks <- function(recording, filename = NULL, casename = NULL) {
    if (is.null(filename))
        filename <- recording$properties$db.filename

    if (is.null(casename))
        casename <- recording$properties$casename

    recording$conf$blocks <- read.block.event.data(filename, casename = casename, data.type = "blocks")
    recording
}


#' #' Store the event information inside the recording structure
#'
#' @param filename The database filename. Optional. If not provided it is determined from the field \code{recording$properties$db.filename}.
#' @param casename The casename. Optional. If not provided it is determined from the field \code{recording$properties$casename}.
#' 
#' @return The recording structure with the event information added.
#' 
#' @family recording
#' 
#' @export
recording.set.events <- function(recording, filename = NULL, casename = NULL) {
    if (is.null(filename))
        filename <- recording$properties$db.filename

    if (is.null(casename))
        casename <- recording$properties$casename

    recording$conf$events <- read.block.event.data(filename, casename = casename, data.type = "events")
    recording
}


#' Set the zerotime of the recording, i.e., the timestamp that corresponds to t = 0.
#'
#' Given the zerotime as a timestamp, calculate the zerotime in seconds and store both the timestamp
#' and the number of seconds from the start of the recording when the zerotime occurs.
#'
#' @param timestamp The timestamp in ISO-8601 format: YYYYMMDDTHHMMSS. If the timestamp is not given, it is read from the recording.
#'
#' @return The recording structure with the zerotime added.
#' 
#' @family recording
#' 
#' @export
recording.set.zerotime <- function(recording, timestamp = NULL) {

    if (is.null(timestamp))
        timestamp <- recording.get.zerotime(recording)

    recording$properties$zerotime   <- timestamp
    recording$properties$zerotime.s <- as.numeric(difftime(timestamp, recording$properties$time.start, units = "secs"))
    
    recording
}


#' Get the zerotime of the recording, i.e., the timestamp that corresponds to t = 0.
#'
#' @param A recording.
#'
#' @return The zerotime as a timestamp (\code{POSIXct}).
#' 
#' @family recording
#' 
#' @export
recording.get.zerotime <- function(recording) {
    if(is.null(recording$conf$events))
        stop("No event information. Cannot continue.")
    timeformat <- "%Y%m%dT%H%M%S"
    zerotime <- subset(recording$conf$events, eventtype = "zerotime", select = "timestamp", drop = TRUE)
    zerotime <- str.to.timestamp(zerotime, timeformat)
    zerotime
}


#' Given a recording collection, find the overlapping times of all the recordings.
#'
#' @param collection A recording collection (a list of recordings).
#'
#' @return A list containing the start and stop times fo 
#' 
#' @family recording
#' 
#' @export
find.recording.overlap <- function(collection) {
    N <- length(collection)
    if (N < 2)
        stop("Need at least 2 recordings to find overlap. Cannot continue.")

    time.start <- collection[[1]]$properties$time.start
    time.stop  <- collection[[1]]$properties$time.stop

    for (i in 2:N) {
        if (collection[[i]]$properties$time.start > time.start)
            time.start <- collection[[i]]$properties$time.start
        if (collection[[i]]$properties$time.stop < time.stop)
            time.stop <- collection[[i]]$properties$time.stop
    }

    list(time.start, time.stop)
}


#' Cut a recording to the given time interval, given as timestamps.
#'
#' Also realign the time vector so that t = 0 is at the new start
#' time and recalculate the length of the recording.  All signals in
#' the recording are processed.
#'
#' @param recording A recording.
#' @param ts A time interval given as a two-element list with two timestamps. The timestamps are given as strings in ISO-8601 format or as \code{POSIXct} timestamps.
#'
#' @return The recording, with all signals cut to the given time interval.
#' 
#' @family recording
#' 
#' @export
cut.recording <- function(recording, ts = NULL) {
    signals   <- names(recording$signal)

    for (s in signals) {
        recording$signal[[s]]   <- extract.segment.timestamp(recording, ts, signal = s)
        recording$signal[[s]]$t <- recording$signal[[s]]$t - recording$signal[[s]]$t[1]
    }

    recording$properties$time.start     <- ts[[1]]
    recording$properties$time.stop      <- ts[[2]]
    recording$properties$time.start.raw <- NA
    recording$properties$time.stop.raw  <- NA
    recording$properties$zerotime       <- ts[[1]]
    recording$properties$length         <- as.numeric(difftime(ts[[2]], ts[[1]], units = "secs"))

    recording
}


#' Cut the given recording to the time interval ts, given in sconds.
#'
#' Also realign the time vector so that t = 0 is at the new start
#' time and recalculate the length of the recording.  All signals in
#' the recording are processed.
#'
#' @param recording A recording.
#' @param ts A time interval given as a two-element list where the elements correspond to the number of seconds from the zerotime of the recording.
#'
#' @return The recording, with all signals cut to the given time interval.
#' 
#' @family recording
#' 
#' @export
cut.recording.s<- function(recording, ts = NULL) {
    signals   <- names(recording$signal)

    for (s in signals) {
        recording$signal[[s]]   <- extract.segment.s(recording, ts, signal = s)
        recording$signal[[s]]$t <- recording$signal[[s]]$t - recording$signal[[s]]$t[1]
    }

    recording$properties$time.start     <- recording$properties$time.start + ts[1]
    recording$properties$time.stop      <- recording$properties$time.start + ts[2]
    recording$properties$time.start.raw <- NA
    recording$properties$time.stop.raw  <- NA
    recording$properties$zerotime       <- recording$properties$time.start + ts[1]
    recording$properties$length         <- ts[2] - ts[1]

    recording
}


#' Cut the recordings in a collection so that they represent the same time intervals.
#'
#' Also realign the time vector so that t = 0 is at the new start
#' time. Also recalculate the length of the recordings.  All signals in
#' all recordings in the collection are processed.
#'
#' @param collection A recording collection.
#' @param ts A time interval given as a two-element list with two timestamps. The timestamps are given as strings in ISO-8601 format or as \code{POSIXct} timestamps.
#'
#' @return The recording collection, with all signals in all recordings cut to the given time interval.
#' 
#' @family recording
#' 
#' @export
cut.recordings <- function(collection, ts) {
    N <- length(collection)

    for (i in 1:N) {
        collection[[i]] <- cut.recording(collection[[i]], ts)
    }

    collection
}


#' Cut the recordings in a collection so that they represent the same time intervals.
#'
#' Also realign the time vector so that t = 0 is at the new start
#' time. Also recalculate the length of the recordings.  All signals in
#' all recordings in the collection are processed.
#'
#' @param collection A recording collection.
#' @param ts A time interval given as a two-element list where the elements correspond to the number of seconds from the zerotime of the recording. 
#'
#' @return The recording collection, with all signals in all recordings cut to the given time interval.
#' 
#' @family recording
#' 
#' @export
cut.recordings.s <- function(collection, ts) {
    N <- length(collection)

    for (i in 1:N) {
        collection[[i]] <- cut.recording.s(collection[[i]], ts)
    }

    collection
}


#' Collect results.
#' 
#' Get the analysis results, which are internally stored as a list of matrices,
#' from a recording and return them as a data frame or as a matrix.
#' 
#' @param recording A recording.
#' @param format. Output formmat. Either \code{data.frame} (default) or \code{matrix}.
#'
#' @return The analysis results either as a data frame or as a matrix.
#' 
#' @family recording
#' 
#' @export
collect.results <- function(recording, format = "data.frame") {
    ## sanity check
    if ("results" %in% names(recording))
        if (length(recording$results) < 1)
            stop("No results present. Cannot continue.")

    ## Return results as a numeric matrix, without extra factors etc
    if (format == "matrix") {
        out <- do.call("rbind", do.call("c", recording$results))
    }


    ## Return results as a data frame
    if (format == "data.frame") {
        data           <- do.call("rbind", do.call("c", recording$results))
        rownames.tmp   <- rownames(data)
        rownames(data) <- NULL
        out            <- as.data.frame(data)

        out$variable   <- factor(as.character(rownames.tmp))
        out$value      <- as.numeric(data[,"value"])

        out$segmentid  <- as.numeric(data[,"segment"])
        out$blockid    <- as.numeric(data[,"block"])
        out$block      <- factor(as.numeric(data[,"block"]))
        out$segment    <- factor(as.numeric(data[,"segment"]))

        ## add metadata from the block information in the recording
        resultrow.template <- generate.result.row(recording$conf$blocks)
        new.columns        <- setdiff(names(resultrow.template), names(out))
        out                <- merge(out, resultrow.template[,c(new.columns, "blockid")], by = "blockid")
    }

    out
}


#' Collect the results from all recordings in a recording collection.
#' 
#' Get the analysis results, which are internally stored as a list of matrices,
#' from a recording and return them as a data frame.
#' 
#' @param collection A recording collection
#'
#' @return The analysis results as a data frame.
#' 
#' @family recording
#' 
#' @export
collect.results.collection <- function(collection) {
    ## container for the results
    out <- data.frame()

    for (recording in collection) {
        out <- rbind(out, collect.results(recording, format = "data.frame"))
    }

    out
}


#' Add timestamps to the segments in the results collected from a recording
#' 
#' @param recording A recording.
#' @param results The results collected from a recording as a data frame.
#'
#' @return The results with the segment timestamps added.
#' 
#' @family recording
#' 
#' @export
add.segment.timestamp <- function(recording, results) {
    for (i in levels(results$block)) {
        ind <- (results$block == as.numeric(as.character(i)))
        block.tmp <- subset(recording$conf$blocks, blockid == i)
        results$timestamp[ind] <- as.character(block.tmp$starttime)
    }
    results$timestamp <- str.to.timestamp(results$timestamp)
    results$timestamp <- results$timestamp + results$segmentid * (recording$conf$settings$segment.length / 2)

    results
}


#' Save a recording
#' 
#' @param recording A recording.
#' @param filename The filename in which to save the recording.
#'
#' @family recording
#' 
#' @export
save.recording <- function(recording, filename) {
    saveRDS(recording, file = filename, compress = "xz")
}


#' Load a recording
#' 
#' @param filename The filename from which to load the recording.
#'
#' @family recording
#' 
#' @export
load.recording <- function(filename) {
    readRDS(filename)
}
