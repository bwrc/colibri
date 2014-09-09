#' Lomb-Scargle Periodogram estimation
#'
#' This code is adapted from the function lomb.m from
#' http://www.mit.edu/~gari/CODE/HRV/CliffordHRVtools.zip.
#' 
#' @param t A vector with the time instants of the events.
#' @param x A vector with 
#' @param f A vector of frequencies at which the spectrum should be esimated.
#' @param demean Should the mean be subtraced from the spectrum. Default is TRUE.
#' @param smooth Should the spectrum be smoothed. Default is FALSE.
#' @param smooth.kernel The smoothing kernel. The default is to use a 'daniell' kernel.
#' @param smooth.degree The degree of smoothing. Default is 5.
#' @param normalization How to normalize the spectrum. Possible values are 'variance' (default) and 'none'.
#'
#' @return A list with the following components.
#' \describe{
#' \item{f}{The frequencies at which the spectrum was estimated.}
#' \item{Px}{The power at the frequencies in f.}
#' \item{Prob}{The probability of the power in a particular frequency band}
#' }
#' 
#' @seealso
#' http://www.ltrr.arizona.edu/~dmeko/notes_6.pdf
#' https://onlinecourses.science.psu.edu/stat510/?q=book/export/html/57
#' http://reference.wolfram.com/applications/timeseries/UsersGuideToTimeSeries/SpectralAnalysis/1.8.4.html
#'
#' @family HRV frequency-domain
#'
#' @export
lombscargle <- function(t, x, f = NULL, demean = TRUE, smooth = FALSE, smooth.kernel = "daniell", smooth.degree = 5, normalization = "variance") {
    ## When using this for HRV analyses,
    ## t is the ibi series
    ## and x is the time

    if (is.null(f)) {
        maxfreq <- 1/min(diff(t))
        f       <- seq(from = 1/512, to = maxfreq, by = 1/512)
    }


    ## subtract mean, compute variance, initialize Px
    if (demean)
        z  <- x - mean(x)
    else
        z <- x

    xvar <- var(x)
    N    <- length(f)
    Px   <- vector(mode = "numeric", length = length(f))
    Prob <- vector(mode = "numeric", length = length(f))

    ##  compute power by looping over all frequencies
    for (i in seq(length(f))) {
        w <- 2 * pi * f[i]

        if (w > 0) {
            twt   <- 2 * w * t
            tau   <- atan2(sum(sin(twt)), sum(cos(twt))) / 2 / w
            wtmt  <- w * (t - tau)
            Px[i] <- (sum(z * cos(wtmt))^2) / sum(cos(wtmt)^2) + (sum(z * sin(wtmt))^2) / sum(sin(wtmt)^2)
        }  else {
            Px[i] <- (sum(z * t)^2) / sum(t^2)
        }
    }

    ## normalize by variance or some other method
    if (normalization == "variance")
        Pxnorm <- Px / 2 / xvar^2
    if (normalization == "none")
        Pxnorm <- Px

    for (i in seq(length(Px))) {
        if (xvar != 0) {
            Prob[i] <- 1 - (1 - exp(-Px[i]))^N
        }    else {
            Px[i]   <- Inf
            Prob[i] <- 1
        }
        if (Prob[i] < 0.001)  ## allow for possible roundoff error
            Prob[i] <- N * exp(-Px[i])
    }

    ## Kernel smoothing
    if (smooth) {
        ks <- kernel(smooth.kernel, smooth.degree)
        Px <- kernapply(c(rep(0, smooth.degree), Px, rep(0, smooth.degree)), ks, circular = FALSE)
    }

    list("Px" = Pxnorm, "f" = f, "Prob" = Prob)
}
