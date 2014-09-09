#' Read data in the Suunto Data Format
#'
#' @param filename The name of the file to be read.
#'
#' @return A recording structure (a list) containing the header information and signal data.
#' 
#' @seealso read.data, new.recording
#' 
#'
#' @family read_data
#'
#' @export
read.data.suunto <- function(filename) {
    ## Initialise the recording structure
    recording <- new.recording()

    ## Process the header
    header <- scan(filename, skip = 1, nlines = 7, what = "character", sep = "\n", quiet = TRUE)

    ## Determine the time format
    timeformat.1 <- "%d.%m.%Y %H:%M.%S"
    timeformat.2 <- "%d.%m.%Y %H:%M:%S"
    
    for (line in header) {
        line <- iconv(line, from = "ISO_8859-1", to = "utf8")
        line <- unlist(strsplit(line, "[=]"))

        if (tolower(line[1]) == "starttime") {
            if (substr(line[2], 17, 17) == ".") {
                timeformat <- timeformat.1
            }
            if (substr(line[2], 17, 17) == ":") {
                timeformat <- timeformat.2
            }
            recording$properties$time.start.raw <- line[2]
            recording$properties$time.start     <- str.to.timestamp(line[2], timeformat)
        }

        if (tolower(line[1]) == "endtime") {
            if (substr(line[2], 17, 17) == ".") {
                timeformat <- timeformat.1
            }
            if (substr(line[2], 17, 17) == ":") {
                timeformat <- timeformat.2
            }

            recording$properties$time.stop.raw <- line[2]
            recording$properties$time.stop     <- str.to.timestamp(line[2], timeformat)
        }

        if (tolower(line[1]) == "name")
            recording$properties$subject <- line[2]
    }

    ## Set additional attributes
    recording$properties$format         <- "sdf"
    recording$properties$format.long    <- "Suunto Data Format"
    recording$properties$device.type    <- "Suunto"
    recording$properties$device.version <- 1

    ## Set the zerotime that anchors time operations on the recording
    recording <- recording.set.zerotime(recording, recording$properties$time.start)

    ## Read the ibi data and scale it to milliseconds
    ibi       <- scan(filename, skip = 9, quiet = TRUE)
    recording <- recording.set.ibi(recording, ibi)

    recording
}
