#' Template for analysis settings.
#'
#' Return a list of lists, containing default analysis settings.
#'
#' @section Analysis options:
#' \describe{
#'   \item{$analysis$time}{Perform time-domain analyses. Default is TRUE.}
#'   \item{$analysis$frequency}{Perform frequency-domain analyses. Default is TRUE.}
#'    \item{$analysis$geometric}{Perform geometric analyses. Default is TRUE.}
#'    \item{$analysis$nonlinear}{Perform nonlinear analyses. Default is TRUE.}
#' }
#' 
#' @section Segment settings:
#' \describe{
#'   \item{$segment.length}{Segment length in seconds. Default is 300}
#'   \item{$segment.overlap}{Segment overlap in seconds. Default is 0}
#' }
#' 
#' @section Time domain settings:
#' \describe{
#'   \item{$timedomain$metric.list}{A list of the time domain HRV metrics that should be analyzed. The default is c("mean", "meanhr", "rmssd", "var", "stdev", "pnnx")}
#'   \item{$timedomain$parameters$pnnx}{A list indicating which pNNx values should be analysed. The default is c(50), so that the pNN50 is analyzed.}
#' }
#' 
#'
#' @section Frequency domain settings:
#' \describe{
#' \item{$frequencydomain$metric.list}{A list of the frequency domain metrics that should be analyzed. The default is c("standard") and if some custom bands are added, this must be set to c("standard", "custom").}
#' }
#' 
#' @section Standard frequency bands:
#' \describe{
#'   \item{$frequencydomain$parameters$band.names.standard.prefix}{String used to prefix the standard VLF, LF and HF bands. Default is c("")}
#'   \item{$frequencydomain$parameters$band.vlf}{VLF band limits. Default is c(0.00, 0.04)}
#'   \item{$frequencydomain$parameters$band.lf}{LF band limits. Default is c(0.04, 0.15)}
#'   \item{$frequencydomain$parameters$band.hf}{HF band limits. Default is c(0.15, 0.40)}
#' }
#' 
#' @section Custom frequency bands:
#' \describe{
#'   \item{$frequencydomain$parameters$band.names.custom}{A list of names with the custom frequency bands. E.g. c("custom1", "custom2")}
#'   \item{$frequencydomain$parameters$band.lower}{Lower limits for the custom frequency bands, in order, e.g., c(0.1, 0.2)}
#'   \item{$frequencydomain$parameters$band.upper}{Upper limits for the custom frequency bands, in order, e.g., c(0.3, 0.5)}
#' }
#' 
#' @section General spectrum parameters:
#' \describe{
#'   \item{$frequencydomain$parameters$f.limits}{A list giving the upper and lower limits for frequency estimation. The default is c(0, 0.4)}
#'
#'   \item{$frequencydomain$parameters$demean}{Boolean indicating whether the spectrum should be demeaned. Default is TRUE.}
#'   \item{$frequencydomain$parameters$normalization}{The type of spectrum normalization to be use.  Possible values are 'variance' and 'none' (default).}
#'
#'   \item{$frequencydomain$parameters$smooth}{Boolean indicating whether the spectrum should be smoothed. Default is FALSE.}
#'   \item{$frequencydomain$parameters$kernel}{String giving the smoothing kernel. Default is "daniell".}
#'   \item{$frequencydomain$parameters$smooth.degree}{Degree of smoothing. Default is 5.}
#' }
#' 
#'@section Geometric settings:
#' \describe{
#'   \item{$geometric$metric.list}{A list of geometric metrics. Default is c("sd1", "sd2", "triangular.index", "tinn").}
#'   \item{$geometric$triangular.index$binwidth}{Bin width for the triangular index. Default is 128.}
#'   \item{$geometric$tinn$binwidth}{Bin width for the TINN metric. Default is 128.}
#' }
#' 
#'@section Nonlinear settings:
#' \describe{
#'   \item{$nonlinear$metric.list}{A list of nonlinear metrics. Default is c("sampen", "apen") for both Sample Entropy and Approximate Entropy.}
#'   \item{$geometric$embedding_dimension}{The embedding dimension. Default is 2.}
#' }
#'
#' @return A list with default analysis settings.
#'
#' @family settings
#'
#' @export
settings_template_hrv <- function() {
    settings                            <- list()

    ## Define what analyses should be performed
    settings$analysis$time             <- TRUE
    settings$analysis$frequency        <- TRUE
    settings$analysis$geometric        <- TRUE
    settings$analysis$nonlinear        <- TRUE

    ## Segment length and overlap
    settings$segment.length             <- 300
    settings$segment.overlap            <- 0
    
    ## Time domain
    settings$timedomain$metric.list     <- c("min", "max", "median", "mean", "meanhr", "rmssd", "var", "stdev", "pnnx")
    settings$timedomain$parameters$pnnx <- c(10, 20, 30, 40, 50)

    ## Frequency domain
    settings$frequencydomain$metric.list         <- c("standard", "custom")

    ## ... band limits for standard bands
    settings$frequencydomain$parameters$band.names.standard.prefix <- c("")
    settings$frequencydomain$parameters$band.vlf            <- c(0.00, 0.04)
    settings$frequencydomain$parameters$band.lf             <- c(0.04, 0.15)
    settings$frequencydomain$parameters$band.hf             <- c(0.15, 0.40)

    ## ... additional custom frequency bands
    settings$frequencydomain$parameters$band.names.custom <- c("custom1", "custom2")
    settings$frequencydomain$parameters$band.lower        <- c(0.1, 0.2)
    settings$frequencydomain$parameters$band.upper        <- c(0.2, 0.3)

    ## ... set limits for the frequency estimation
    settings$frequencydomain$parameters$f.limits <- c(0, 0.4)

    ## ... spectral smoothing
    settings$frequencydomain$parameters$demean        <- TRUE
    settings$frequencydomain$parameters$normalization <- "none"

    settings$frequencydomain$parameters$smooth        <- FALSE
    settings$frequencydomain$parameters$kernel        <- "daniell"
    settings$frequencydomain$parameters$smooth.degree <- 5

    ## Geometric
    settings$geometric$metric.list                    <- c("sd1", "sd2", "triangular.index", "tinn")
    settings$geometric$triangular.index$binwidth      <- 128
    settings$geometric$tinn$binwidth                  <- 128

    ## Nonlinear
    settings$nonlinear$metric.list                    <- c("sampen", "apen")
    settings$nonlinear$embedding_dimension            <- 2

    settings
}





