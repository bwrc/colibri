#' Generic function for reading data.
#'
#' The input format is determined automatically from the file extension or from the provided \code{format} option.
#'
#' @param filename The name of the file to be read.
#' @param format The format of the data (optional). The supported data formats are:
#'
#' \describe{
#' \item{ibi}{Generic IBI (one IBI per line, plaintext file, no header).}
#' \item{sdf}{Suunto data format.}
#' \item{fbe}{Firstbeat data format.}
#' \item{edf}{European Data Format (using the EDF-package).}
#' }
#'
#' @return A recording structure (a list) containing the header information and signal data.
#' 
#' @family read_data
#'
#' @export
read.data <- function(filename, format = NULL) {

    if (is.null(format)) {
        file.extension <- rev(tolower(get_filename_parts(filename)))[1]

        if (file.extension == "sdf")
            format <- "suunto"

        if (file.extension == "fbe")
            format <- "firstbeat"

        if (file.extension == "edf")
            format <- "european_data_format"

        if (file.extension %in% c("ibi", "txt"))
            format <- "generic_ibi"

    }

    ## Process various formats
    if (format == "suunto")
        recording <- read.data.suunto(filename)

    if (format == "firstbeat")
        recording <- read.data.firstbeat(filename)

    if (format == "european_data_format")
        recording <- read.data.edf(filename)

    if (format == "generic_ibi")
        recording <- read.data.ibi(filename)
    
    recording

}
