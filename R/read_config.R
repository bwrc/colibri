#' Read the measurement configuration from a file.
#' 
#' @param filename The file containing the measurement configuration.
#' @param config.format Optional. Give the configuration format as a
#' string. Possible values are
#' \describe{
#' \item{sqlite}{SQLite 3 format}
#' \item{excel}{Excel XML format}
#' #' }
#' 
#' @return The measurement configuration as a data frame.
#' 
#' @family recording
#' 
#' @export
read.config <- function(filename, config.format = NULL) {

    ## Try to determine the block file format
    if (is.null(config.format)) {
        file.extension <- tolower(get.filename.parts(filename)[2])

        if (file.extension == "csv")
            config.format <- "csv"
        if (file.extension %in% c("sqlite", "db", "db3"))
            config.format <- "sqlite"
        if (file.extension == "xlsx")
            config.format <- "excel"
        if (is.null(config.format))
            stop("Unknown config file format.")
    }

    ## Read the measurement configuration based on the format
    switch(config.format,
           sqlite     = read.config.sqlite(filename),
           excel      = read.config.excel(filename)
           )
}


#' Read the measurement configuration from an SQLite 3 database.
#' 
#' @param filename The database file containing the measurement configuration.
#' 
#' @return The measurement configuration as a data frame.
#' 
#' @family recording
#' 
#' @keywords internal
read.config.sqlite <- function(filename) {
    ## We need the RSQLite database interface
    require("RSQLite")

    ## Read the data
    sqlite       <- dbDriver("SQLite")
    db           <- dbConnect(sqlite, filename)
    query.string <- "SELECT * FROM mc"
    mc           <- dbGetQuery(db, query.string)
    dbDisconnect(db)

    ## Convert any strings to factors
    ind     <- sapply(mc, is.character)
    mc[ind] <- lapply(mc[ind], factor)

    mc

}

#' Read the measurement configuration from an Excel file.
#' 
#' @param filename The Excel file containing the measurement configuration.
#' 
#' @return The measurement configuration as a data frame.
#' 
#' @family recording
#' 
#' @keywords internal
read.config.excel <- function(filename) {
    ## We need the xlsx package
    require("xlsx")

    ## Read the data
    mc <- read.xlsx2(mc.filename, sheetName = "mc")

    ## Change the type of some of the columns
    if ("dataid" %in% names(mc))
        mc$dataid <- as.numeric(as.character(mc$dataid))
    if ("include" %in% names(mc))
        mc$include <- as.numeric(as.character(mc$include))
    if ("done" %in% names(mc))
        mc$done <- as.numeric(as.character(mc$done))
    if ("physiodata" %in% names(mc))
        mc$physiodata <- as.character(mc$physiodata)

    mc
}
