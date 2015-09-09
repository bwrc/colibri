#' Artifact detection from interbeat interval (IBI) time series.
#'
#' Implements a sligthly modified method presented by Xu and
#' Schuckers.  Each RR interval is compared with the median of 24
#' surrounding non-artefactual RR intervals and the last accepted RR
#' interval. Only if both differences are outside +- 20% the interval
#' will be marked as an artifact.
#'
#' @param x A vector with the interbeat intervals (IBIs)
#' 
#' @return A list with the values of the metrics.
#' 
#' @references Xu, X., Schuckers, S.: Automatic Detection of Artifacts in Heart
#'             Period Data. J Electrocardiol. 2001;34 Suppl:205-10.
#'
#' @family HRV
#' 
#' @export
detect_artifacts_xu <- function(x) {

    N             <- length(x)
    last.accepted <- x[1]
    art_ind       <- rep(NA, N)
    ind_before    <- rep(NA, 12)
    thr           <- 0.2

    j <- 1
    ## loop over the IBI series in windows
    for(i in seq.int(N)) {

        ## calculate current median
        w_stop  <- min(N, i + 12)
        
        cm      <- median(c(x[ind_before], x[i:w_stop]), na.rm = TRUE)
        ## check if we accept the current ibi or not
        c1 <- abs(x[i] - last.accepted) > (thr * last.accepted)
        c2 <- abs(x[i] - cm) > (thr * cm)

        if (c1 & c2) {
            art_ind[i] <- 1
        } else {
            art_ind[i]                  <- 0
            last.accepted               <- x[i]
            ind_before[max(1, j %% 13)] <- i
            j <- j + 1

        }
    }

    which(art_ind == 1)
}


#' Visualize artifacts in IBI time series.
#'
#' Artifact detection is performed using the method presented by Xu and Schuckers
#' \code{\link{detect_artifacts_xu}} and the result is visualized.
#'
#' @param ibi A vector with the interbeat intervals (IBIs)
#' @param ibi.t A vector with the times of occurrence of the interbeat intervals (IBIs)
#' 
#' @return Nothing.
#' 
#' @family HRV
#' 
#' @export
plot_artifacts <- function(ibi, ibi.t) {
    ## Detect artifacts
    ind <- detect_artifacts_xu(ibi)
    ibi <- 60e3 / ibi
    
    ## Plot raw ibi data
    plot(ibi.t, ibi, type = "b", col = "black", xlab = "time [s]", ylab = "heart rate [bpm]")

    ## Show artifacts
    points(ibi.t[ind], ibi[ind], col = "red", pch = 16)

    ## Add smoother
    sm <- loess(y ~ x, data = list(x = ibi.t, y = ibi), span = 0.08)
    lines(ibi.t, sm$fitted, col = "blue", lwd = 2)
}
