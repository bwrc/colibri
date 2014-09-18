#' Read data in the Firstbeat Data Format
#'
#' @param filename The name of the file to be read.
#'
#' @return A recording structure (a list) containing the header information and signal data.
#'
#' @family read_data
#'
#' @export
read.data.firstbeat <- function(filename) {
    ## Required package
    require(XML)

    ## Initialise the recording structure
    recording  <- new_recording()

    ## Read the IBI data
    datadir  <- tempdir()
    datafile <- paste(datadir, "/data.xml", sep = "")

    unzip(filename, exdir = datadir)

    doc      <- xmlTreeParse(datafile, useInternal = TRUE)
    top      <- xmlRoot(doc)
    ibi      <- as.numeric(unlist(xmlApply(top[["Person"]][["Measurement"]][["IBIData"]], xmlValue)))

    ## Get the start time of the measurement
    timeformat <- "%Y%m%dT%H%M%S"
    start.date <- xmlValue(top[["Person"]][["Measurement"]][["MeasurementInfo"]][["StartDate"]])
    start.time <- xmlValue(top[["Person"]][["Measurement"]][["MeasurementInfo"]][["StartTime"]])

    recording$properties$time.start.raw <- paste(start.date, start.time, sep = " ")

    start.time <- paste(paste(rev(strsplit(start.date, "[.]")[[1]]), sep ="", collapse = ""), "T", gsub(":", "", start.time), sep = "")
    start.time <- str_to_timestamp(start.time, timeformat)

    recording$properties$time.start     <- str_to_timestamp(start.time, timeformat)
    
    ## Set the zerotime that anchors time operations on the recording
    recording <- recording_set_zerotime(recording, recording$properties$time.start)

    ## Set additional attributes
    recording$properties$format         <- "fbe"
    recording$properties$format.long    <- "Firstbeat Data Format"
    recording$properties$device.type    <- "Firstbeat"
    recording$properties$device.version <- 1

    ## Read the ibi data and scale it to milliseconds
    recording <- recording_set_ibi(recording, ibi)
    
    recording

}
