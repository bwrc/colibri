#' Read data in the European Data Format.
#'
#' @param filename The name of the file to be read.
#'
#' @return A recording structure (a list) containing the header information and signal data.
#' 
#' @family read_data
#'
#' @export
read.data.edf <- function(filename) {
    ## Load the EDF library
    require(edf)
    
    ## Initialise the recording structure
    recording            <- new.recording()

    ## Read the edf data
    edf <- read.edf(filename)

    ## Set the start and stop time of the recording
    timeformat <- "%d.%m.%y %H.%M.%S"
    recording$properties$time.start.raw <- paste(edf$header.global$startdate, edf$header.global$starttime, sep = " ")
    recording$properties$time.start     <- str.to.timestamp(recording$properties$time.start.raw, timeformat)

    recording$properties$length <- edf$header.global$n.data.records * edf$header.global$data.record.duration
    
    recording$properties$time.stop.raw <- NA
    recording$properties$time.stop     <- recording$properties$time.start + recording$properties$length

    recording$properties$subject       <- edf$header.global$patient.id
    
    ## Set additional attributes
    recording$properties$format         <- "edf"
    recording$properties$format.long    <- "European Data Format"
    recording$properties$device.type    <- "Generic"
    recording$properties$device.version <- 1

    ## Set the zerotime that anchors time operations on the recording
    recording <- recording.set.zerotime(recording, recording$properties$time.start)

    ## Store the signals
    recording$signal <- edf$signal

    ## Store the sampling rates
    for (s in names(edf$header.signal)) {
        recording$signal[[s]]$samplingrate <- edf$header.signal[[s]]$samplingrate
    }

    recording
}

