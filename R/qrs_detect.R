#' Improve the identification of the R-peak using cubic spline interpolation
#'
#' @param ind.r A vector with the indices of the R-peaks
#' @param sig A vector containing the ECG signal
#' @param window.length The length of the window in samples. Default is 6.
#' @param n.spline The number of splines to use.
#' 
#' @return A matrix with two columns. The x-index of the R-peaks are in the first column and the
#'         y-values of the ECG signal are in the second column
#' 
#' http://www.ncbi.nlm.nih.gov/pubmed/9302678
#'
#' @references Daskalov, I and Christov, I. (1997) Improvement of resolution in measurement of electrocardiogram RR intervals by interpolation. Medical engineering & Physics, 19;4;375-379. Elsevier.
#' 
#' @family HRV
#'
#' @export
qrs_interpolate <- function(ind.r, sig, window.length = 6, n.spline = 1000) {
    x.tmp    <- (ind.r - (window.length / 2)):(ind.r + (window.length / 2))
    sp       <- spline(x = x.tmp, y = sig[x.tmp], n = n.spline)
    ind.r    <- which.max(sp$y)
    c(sp$x[ind.r], sp$y[ind.r])
}


#' Calculate the moving average of a vector.
#'
#' @param x A vector with data.
#' @param n Window width in samples.
#' 
#' @return The data in x, averaged in a window of width n.
#'
#' @export
moving_average <- function(x, n = 10) {
    stats::filter(x, rep(1/n, n), sides = 2)
}


#' Adjust two vectors with indices so that these form
#' pairs of indices, where for each pairthe index in the first
#' vector always is smaller than the index in the second vector,
#'
#' @param ind.start start index
#' @param ind.stop stop index
#' 
#' @return A list with the start and stop indices in the.
#'
#' @export
adjust_indices <- function(ind.start, ind.stop) {
    if (ind.stop[1] <= ind.start[1])
        ind.stop <- ind.stop[-1]

    n.min <- min(length(ind.start), length(ind.stop))

    list("ind.start" = ind.start[1:n.min], "ind.stop" = ind.stop[1:n.min])
}



#' R-peak detection
#'
#' @param ecg A vector with the ECG signal
#' @param samplingrate The sampling rate of the ECG signal
#' @param interpolate Should the position of the R-peak be interpolated for improved accuracy. Default is TRUE.
#' 
#' @return A matrix with two columns. The x-index of the R-peaks are in the first column and the
#'         y-values of the ECG signal are in the second column
#' 
#' @family HRV
#'
#' @export
qrs_detect <- function(ecg, samplingrate, interpolate = TRUE) {
    ecg.orig <- ecg
    
    debug <- FALSE
    if (debug) {
        ecg <- ecg.orig[1:(30*500)]
        plot(ecg[1:(30*500)], type = "l", col = "blue")

    }
    
  
    ## Baseline removal
    ecg.med <- stats::runmed(ecg, k = 31)
    ecg.new <- ecg - ecg.med

    ## ecg.new <- diff(ecg.new)
    ecg.new <- (ecg.new)^2

    ## The length of the moving average filter should be about 125 millisconds
    nma <- ceiling((125 / 1000) * samplingrate)
    ## ecg.new <- moving.average(ecg.new, n = 15)
    ## ecg.new <- moving.average(ecg.new, n = 31)
    ecg.new <- moving_average(ecg.new, n = nma)

    ## Determine threshold
    if (length(ecg.new) > (5 * samplingrate)) {
        thr <- (max(ecg.new[1:(3*250)], na.rm = TRUE) - min(ecg.new[1:(3*250)], na.rm = TRUE)) / 4
    } else {
        thr <- 3*mean(ecg.new[1:samplingrate], na.rm = TRUE)
    }

    ## Make the signal binary
    ecg.new[ecg.new < thr]  <- 0
    ecg.new[ecg.new >= thr] <- 1

    ## Check if the signal starts within an R-peak window and fix this

    ind.first <- which.min(ecg.new >= 0) 
    if (ecg.new[ind.first] == 1)
        ecg.new[ind.first] <- 0
    
    ## Find rising and falling edges used to window the R-peak
    ind.rising  <- which(diff(ecg.new) == 1)
    ind.falling <- which(diff(ecg.new) == -1)

    ## Adjust the indices so, that there are only matching pairs of rising and falling indices
    tmp <- adjust_indices(ind.rising, ind.falling)
    ind.rising <- tmp$ind.start
    ind.falling <- tmp$ind.stop

    ind.r   <- sapply(seq.int(length(ind.rising)), function(i) which.max(ecg[ind.rising[i]:ind.falling[i]]) + ind.rising[i] - 1)

    ## The matrix returned contains the x-index of the r-peaks in the
    ## first column and the y-value of the ECG signal in the second column
    r.peaks     <- matrix(nrow = length(ind.r), ncol = 2)
    r.peaks[,1] <- ind.r
    r.peaks[,2] <- ecg[ind.r]
    
    ## Improve R-peak detection using interpolation
    
    if (interpolate)
        r.peaks <- t(sapply(ind.r, function(i) qrs_interpolate(i, ecg)))


    debug <- FALSE
    if (debug) {
        plot(ecg.new[1:500], type = "l")
        lines(ecg/1000, type = "l", col = "blue")
        plot(ecg, type = "l")
        ## abline(v= ind.r, col = "red")
        points(ind.r, ecg[ind.r], col = "red")
        browser()
        1 == 1

        plot(ecg[1:(30*500)], type = "l", col = "blue")
        points(ind.r, ecg[ind.r], col = "red")
    }

    r.peaks
}
