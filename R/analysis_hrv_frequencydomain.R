#' Frequency-domain HRV analysis.
#'
#' @param metric.list  A list of frequency-domain metrics as strings.
#' @param ibi A vector with the interbeat intervals (IBIs).
#' @param t.ibi A vector with the times of occurrence of the interbeat intervals (IBIs).
#' @param settings The settings structure used for HRV analysis with the following fields:
#'
#' \describe{
#' \item{$frequencydomain$parameters$demean}{Should the mean be subtraced from the spectrum. Default is TRUE.}
#' \item{$frequencydomain$parameters$smooth}{Should the spectrum be smoothed. Default is FALSE.}
#' \item{$frequencydomain$parameters$smooth.kernel}{The smoothing kernel. The default is to use a 'daniell' kernel.}
#' \item{$frequencydomain$parameters$smooth.degree}{The degree of smoothing. Default is 5.}
#' \item{$frequencydomain$parameters$normalization}{How to normalize the spectrum. Possible values are 'variance' (default) and 'none'.}
#' }
#' 
#' @return A list with the values of the metrics.
#' 
#' @family HRV frequency-domain
#' 
#' @export
analyse.frequencydomain <- function(metric.list, ibi, t.ibi, settings) {

    ## Calculate the spectrum
    fmin    <- settings$frequencydomain$parameters$f.limits[1]
    fmax    <- settings$frequencydomain$parameters$f.limits[2]
    f       <- seq(from = fmin, to = fmax, length.out = 1000)

    spec    <- lombscargle(t.ibi, ibi, f,
                           demean        = settings$frequencydomain$parameters$demean,
                           normalization = settings$frequencydomain$parameters$normalization,
                           smooth        =  settings$frequencydomain$parameters$smooth,
                           smooth.kernel = settings$frequencydomain$parameters$kernel,
                           smooth.degree = settings$frequencydomain$parameters$smooth.degree)

    res  <- lapply(metric.list, analyse.frequencydomain.helper, spec, settings)
}


#' Helper function for frequency-domain HRV analysis.
#'
#' @param metric A frequency-domain metric to be estimated.
#' @param spec The Lomb-Scargle spectrum estimated from the IBI series using the function lombscargle.
#' @param settings The settings structure used for HRV analysis.
#' 
#' @return The value of the metric.
#'
#' @family HRV frequency-domain
#' 
#' @keywords internal
analyse.frequencydomain.helper <- function(metric, spec, settings) {

    switch(metric,
           standard = ibi.band.standard(spec, settings),
           custom = ibi.band.custom(spec, settings)
           )
}


#' Calculate the power in the standard HRV frequency bands (VLF, LF, HF).
#'
#' @param spec The Lomb-Scargle spectrum estimated from the IBI series using the function lombscargle.
#' @param settings The settings structure used for HRV analysis with the following fields:
#' \describe{
#' \item{$frequencydomain$parameters$f.limits}{A two-element list with the upper and lower frequency bounds for the spectrum estimation.}
#' \item{$frequencydomain$parameters$band.vlf)}{A two-element list with the upper and lower frequency bounds for the VLF band.}
#' \item{$frequencydomain$parameters$band.lf)}{A two-element list with the upper and lower frequency bounds for the LF band.}
#' \item{$frequencydomain$parameters$band.hf)}{A two-element list with the upper and lower frequency bounds for the HF band.}
#' }
#' 
#' @return The input structure res with the following fields added:
#'
#' @family HRV frequency-domain
#' 
#' @export
ibi.band.standard <- function(spec, settings) {

    tot <- integrate.power(spec$f, spec$Px, settings$frequencydomain$parameters$f.limits)

    vlf <- integrate.power(spec$f, spec$Px, settings$frequencydomain$parameters$band.vlf)
    lf  <- integrate.power(spec$f, spec$Px, settings$frequencydomain$parameters$band.lf)
    hf  <- integrate.power(spec$f, spec$Px, settings$frequencydomain$parameters$band.hf)

    lf.norm <- 100 * exp(log(lf) - log(tot - vlf))
    hf.norm <- 100 * exp(log(hf) - log(tot - vlf))
    lfhf    <- exp(log(lf) - log(hf))

    res           <- matrix(data = c(vlf, lf, hf, lf.norm, hf.norm, lfhf), byrow = TRUE, ncol = 1, nrow = 6)

    ## Prefix the names if some string is given
    res.names <- c("vlf", "lf", "hf", "lf.norm", "hf.norm", "lfhf")
    if (settings$frequencydomain$parameters$band.names.standard.prefix != "")
        res.names <- paste(settings$frequencydomain$parameters$band.names.standard.prefix, res.names, sep = "")

    rownames(res) <- res.names
    colnames(res) <- "value"
    res
}

#' Calculate the power in custom HRV frequency bands.
#'
#' @param spec The Lomb-Scargle spectrum estimated from the IBI series using the function lombscargle.
#' @param settings The settings structure used for HRV analysis with the following fields:
#' \describe{
#' \item{$frequencydomain$parameters$band.lower}{A list with the lower frequency bound for the custom bands.}
#' \item{$frequencydomain$parameters$band.upper}{A list with the upper frequency bound for the custom bands.}
#' \item{$frequencydomain$parameters$band.names.custom}{A list with the names of the custom bands.}
#' }
#' 
#' @return The frequencies in the custom bands.
#'
#' @family HRV frequency-domain
#' 
#' @export
ibi.band.custom <- function(spec, settings) {
    N <- length(settings$frequencydomain$parameters$band.names.custom)
    res           <- matrix(data = NA, ncol = 1, nrow = N)
    rownames(res) <- settings$frequencydomain$parameters$band.names.custom
    colnames(res) <- "value"
    
    for (i in seq(N)) {
        band   <- c(settings$frequencydomain$parameters$band.lower[i], settings$frequencydomain$parameters$band.upper[i])
        res[i] <- integrate.power(spec$f, spec$Px, band)
    }

    res
}