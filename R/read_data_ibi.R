#' Read data in generic IBI format with one IBI in milliseconds per line.
#'
#' @param filename The name of the file to be read.
#'
#' @return A recording structure (a list) containing the header information and signal data.
#' 
#' @family read_data
#'
#' @export
read.data.ibi <- function(filename) {
    ## Initialise the recording structure
    recording <- new_recording()

    ## Set additional attributes
    recording$properties$format         <- "ibi"
    recording$properties$format.long    <- "Generic IBI format"
    recording$properties$device.type    <- "Generic"
    recording$properties$device.version <- 1

    ## Read the ibi data and scale it to milliseconds
    ibi       <- scan(filename, skip = 0, quiet = TRUE)
    recording <- recording_set_ibi(recording, ibi)
    
    recording
}
