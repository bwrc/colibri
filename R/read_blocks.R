#' Read block information from a file or from a database and return
#' the given blocks as a dataframe.
#'
#' @param filename The name of the file containing the the block data.
#' @param data.type The type of data to be read: either \code{block} or \code{event}.
#' @param casename The casename.
#' #' @param data.format The data format of the blocks or events:
#' 
#' \describe{
#' \item{csv}{CSV format.}
#' \item{sqlite, db, db3}{SQLite 3 format.}
#' \item{xlsx}{Excel XML format.}
#' }
#' 
#' @return The blocks or events as a data frame.
#' 
#' @family recording
#' 
#' @export
read.block.event.data <- function(filename, data.format = NULL, casename = NULL, data.type = NULL) {

    if (is.null(data.type))
        stop("Data type (events or blocks) not provided. Cannot continue")

    ## Try to determine the file format
    if (is.null(data.format)) {
        file.extension <- tolower(get_filename_parts(filename)[2])

        if (file.extension == "csv")
            data.format <- "csv"
        if (file.extension %in% c("sqlite", "db", "db3"))
            data.format <- "sqlite"
        if (file.extension == "xlsx")
            data.format <- "excel"
        if (is.null(data.format))
            stop("Unknown data format.")
    }

    ## Read block data based on the block format
    switch(data.format,
           sqlite = read.dbtable.sqlite(filename, dbtable = data.type, casename = casename),
           excel = read.dbtable.excel(filename, sheetname = data.type, casename = casename)
           )

}


#' Read blocks or events from an sqlite database.
#'
#' @param filename The name of the file containing the sqlite database.
#' @param dbtable The name of the database table to be raed.
#' @param casename The casename.
#' @param query.string An optional query string.
#' 
#' @return The blocks or events as a data frame.
#' 
#' @family recording
#' 
#' @keywords internal
read.dbtable.sqlite <- function(filename, dbtable = NULL, casename = NULL, query.string = NULL) {
    if (is.null(casename))
        stop("Cannot read blocks from sqlite database without casename")
    if (is.null(dbtable))
        stop("Database table not provided. Unable to continue.")

    ## We need the RSQLite database interface
    require("RSQLite")

    ## Read the block data
    sqlite       <- dbDriver("SQLite")
    db           <- dbConnect(sqlite, filename)

    if (is.null(query.string))
        query.string <- paste("SELECT * FROM ", dbtable, " WHERE casename = '", casename, "'", sep = "")

    data         <- dbGetQuery(db, query.string)

    ## Disconnect database connection
    dbDisconnect(db)

    ## Convert any strings to factors
    ## ind         <- sapply(data, is.character)
    ## data[ind]   <- lapply(data[ind], factor)
    
    data
}


#' Read blocks or events from an Excel file.
#'
#' @param filename The name of the Excel file.
#' @param sheetname The name of the sheet to read.
#' @param casename The casename.
#' 
#' @return The blocks or events as a data frame.
#' 
#' @family recording
#' 
#' @keywords internal
read.dbtable.excel <- function(filename, sheetname = NULL, casename = NULL) {
    if (is.null(casename))
        stop("Cannot read blocks from Excel sheet without casename")
    if (is.null(sheetname))
        stop("Excel sheet name not provided. Unable to continue.")

    ## We need the xlsx package
    require("xlsx")

    ## Read the block data
    casename.tmp <- casename
    data <- read.xlsx2(filename, sheetName = sheetname)
    data <- subset(data, casename == casename.tmp)

    ## Convert dataid and blockid to numeric
    if ("dataid" %in% names(data))
        data$dataid  <- defactor(data$dataid, type = "numeric")
    if ("blockid" %in% names(data))
        data$blockid <- defactor(data$blockid, type = "numeric")
    ## data$starttime <- as.character(data$starttime)
    ## data$stoptime  <- as.character(data$stoptime)

    data
}


#' Return a block structure where any timestamps have been converted
#' to seconds with respect to the start time of the recording.
#'
#' @param recording A recording.
#' @param block A block structure. If not given, it is read from the field \code{recording$conf$blocks}.
#' @param timeformat The time format to use. The default is "\%Y\%m\%dT\%H\%M\%S".
#' 
#' @return The block structure with the timestamps replaced by seconds
#' from the beginning of the recording.
#' 
#' @family recording
#' 
#' @export
block_to_seconds <- function(recording, block = NULL, timeformat = "%Y%m%dT%H%M%S") {
    if (is.null(block))
        block <- recording$conf$blocks
    
    time.start <- recording$properties$time.start #start of recording
    zerotime   <- recording$properties$zerotime #POSIXct, time zero timestamp for blocks specified in seconds
    zerotime.s <- recording$properties$zerotime.s #numeric, same in seconds from time.start
        
    block.s           <- block
    block.s$starttime <- NA
    block.s$stoptime  <- NA
    block.s$starttype <- "time"
    block.s$stoptype  <- "time"
    
    if (block$starttype == "time")
        block.s$starttime <- zerotime.s + defactor(block$starttime, type = "numeric")

    if (block$stoptype == "time")
        block.s$stoptime <- zerotime.s + defactor(block$stoptime, type = "numeric")

    if (block$starttype == "timestamp") {
        ts                <- str_to_timestamp(defactor(block$starttime, type = "string"), timeformat = timeformat)
        block.s$starttime <- as.numeric(difftime(ts, time.start, units = "secs"))
    }

    if (block$stoptype == "timestamp") {
        ts                <- str_to_timestamp(defactor(block$stoptime, type = "string"), timeformat = timeformat)
        block.s$stoptime  <- as.numeric(difftime(ts, time.start, units = "secs"))
    }

    block.s
}


#' Create an empty block structure.
#'
#' @param recording A recording.
#' @param extra.factors Extra factors to add to the block structure.
#' 
#' @return An empty block structure (a data frame) with the following columns:
#' \describe{
#' \item{blockid}{A numeric ID for the block}
#' \item{casename}{The casename (string)}
#' \item{subject}{The subject ID (string)}
#' \item{part}{The part (string).}
#' \item{meas}{The measurement (string).}
#' \item{starttype}{The startttype. Can be either \code{time} (seconds from start of recording) or \code{timestamp} (an absolute timestamp in ISO-8601 format). }
#' \item{starttime}{The starttime; see starttype for details (string).}
#' \item{stoptype}{See starttype.}
#' \item{stoptime}{See starttime.}
#' \item{tasktype}{The task type (string).}
#' }
#' 
#' @family recording
#' 
#' @export
create_block_structure <- function(recording, extra.factors = NULL) {
    blocks <- data.frame(blockid   = numeric(),
                         casename  = character(),
                          subject   = character(),
                          part      = character(),
                          meas      = character(),
                          starttype = character(),
                          starttime = character(),
                          stoptype  = character(),
                          stoptime  = character(),
                          tasktype  = character())

     if (! is.null(extra.factors)) {
         for (ef in extra.factors) {
             tmp           <- data.frame(tmp = character)
             colnames(tmp) <- ef
             blocks        <- cbind(blocks, tmp)
         }
     }
     recording$conf$blocks <- blocks
     recording
 }


#' Create a simple block.
#'
#' @param starttime The start time as a string.
#' @param starttype Either \code{time} or \code{timestamp}. Default is \code{timestamp}.
#' @param stoptime The stop time as a string.
#' @param stoptype Either \code{time} or \code{timestamp}. Default is \code{timestamp}.
#' @param tasktype The task type as a string. Default is \code{none}.
#' @param part An optional string describing the part.
#' @param meas An optional string describing the measurement
#' @param blockid An optional integer denoting the block ID.
#' @param subject An optional string describing the subject ID.
#' @param casename An optional string describing the casename.
#' @param recording An optional recording. If the recording is given,
#' the subject and casename are read from the recording and the
#' arguments for these are ignored.
#' 
#' @return A block structure.
#' 
#' @family recording
#' 
#' @export
create_block_simple <- function(starttime, starttype = "timestamp", stoptime, stoptype = "timestamp", tasktype = "none", part = NA, meas = NA, blockid = NA, subject = NA, casename = NA, recording = NULL) {
    if (! is.null(recording)) {
        subject <- recording$properties$subject
        casename <- recording$properties$casename

        if (nrow(recording$conf$blocks) > 0) {
            blockid <- max(recording$conf$blocks$blockid) + 1
        } else {
            blockid <- 1
        }

    }

    data.frame(blockid   = blockid,
               casename  = casename,
               subject   = subject,
               part      = part,
               meas      = meas,
               starttype = starttype,
               starttime = starttime,
               stoptype  = stoptype,
               stoptime  = stoptime,
               tasktype  = tasktype)
}


#' Add a new block to the block structure in a recording, automatically incrementing the block id.
#' 
#' @param recording A recording.
#' @param block A block structure.
#' 
#' @return The recording with the given block added to the block structure.
#' 
#' @family recording
#' 
#' @export
add_block <- function(recording, block) {
    if (nrow(recording$conf$blocks) == 0) {
        blockid <- 1
    } else {
        blockid <- max(recording$conf$blocks$blockid) + 1
    }

    block$blockid <- blockid
    recording$conf$blocks <- rbind(recording$conf$blocks, block)

    recording
}
