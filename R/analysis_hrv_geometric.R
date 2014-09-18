#' Geometric HRV analysis.
#'
#' @param metric.list  A list of geometric metrics as strings.
#' @param ibi A vector with the interbeat intervals (IBIs).
#' @param t.ibi A vector with the times of occurrence of the interbeat intervals (IBIs).
#' @param settings The settings structure used for HRV analysis with the following fields:
#'
#' \describe{
#' \item{$geometric$triangular.index$bindwidth}{The bin width used in the calculation of the triangular index.}
#' }
#' 
#' @return A list with the values of the metrics.
#' 
#' @family HRV geometric
#' 
#' @export
analyse_geometric <- function(metric.list, ibi, t.ibi = NULL, settings) {
    res <- lapply(metric.list, analyse_geometric_helper, ibi, settings)
}


#' Helper function for geometric HRV analysis.
#'
#' @param metric A metric to be estimated.
#' @param spec The Lomb-Scargle spectrum estimated from the IBI series using the function lombscargle.
#' @param settings The settings structure used for HRV analysis.
#' 
#' @return The value of the metric.
#'
#' @family HRV geometric
#' 
#' @keywords internal
analyse_geometric_helper <- function(metric, x, settings) {

    switch(metric,
           sd1              = matrix(dimnames = list(metric, "value"), ibi_sd1(x)),
           sd2              = matrix(dimnames = list(metric, "value"), ibi_sd2(x)),
           triangular.index = matrix(dimnames = list(metric, "value"), ibi_triangular_index(x, binwidth = settings$geometric$triangular.index$binwidth)),
           tinn             = matrix(dimnames = list(metric, "value"), ibi_tinn(x, binwidth = settings$geometric$tinn$binwidth))
           )
}


#' Determine the SD1 metric from a Poincare plot.
#'
#' @param ibi A vector with the interbeat intervals (IBIs).
#' 
#' @return The value of the SD1 metric.
#'
#' @family HRV geometric
#' 
#' @export
ibi_sd1 <- function(ibi) {
    sqrt(0.5 * var(diff(ibi)))
}


#' Determine the SD2 metric from a Poincare plot.
#'
#' @param ibi A vector with the interbeat intervals (IBIs).
#' 
#' @return The value of the SD2 metric.
#'
#' @family HRV geometric
#'   
#' @export
ibi_sd2 <- function(ibi) {
    sqrt(2 * (sd(ibi)^2) - 0.5 * var(diff(ibi)))
}


#'  Determine the HRV Triangular index as defined in HRV Guidelines:
#'
#' (1) IBI histogram using bins of width 1 / 128 seconds and.
#' (2) Determine the number of intervals in the modal bin
#' (3) Triangular index:
#' (total number of NN intervals) /(number of NN intervals in the
#' modal bin).
#'
#' @param ibi A vector with the interbeat intervals (IBIs).
#' @param binwidth The reciprocal of the width of the histogram bins. Default is 128.
#' 
#' @return The HRV triangular index.
#'
#' @family HRV geometric
#'   
#' @export
ibi_triangular_index <- function(ibi, binwidth = 128) {
    ## Construct histogram
    tmp     <- hist(ibi, breaks = seq(from = min(ibi), to = (max(ibi) + (1000/binwidth)), by = (1000 / binwidth)), plot = FALSE)
    length(ibi) / max(tmp$counts)
}


#' Determine the HRV Triangular Interpolation Index (TINN)
#' as defined in HRV Guidelines.
#'
#' @param ibi A vector with the interbeat intervals (IBIs).
#' @param binwidth The reciprocal of the width of the histogram bins. Default is 128.
#' 
#' @return The HRV triangular interpolat index (TINN).
#'
#' @family HRV geometric
#'   
#' @export
ibi_tinn <- function(ibi, binwidth = 128) {
    ## Construct histogram
    tmp     <- hist(ibi, breaks = seq(from = min(ibi), to = max(ibi)+(1000/binwidth), by = 1000/binwidth), plot = FALSE)

    ## the height of the histogram
    h <- max(tmp$counts)

    ## Get the index of the modal bin
    ind    <- which.max(tmp$counts)
    nmax   <- length(tmp$counts)

    ## Left side
    atot   <- sum(tmp$counts[1:ind])
    ind.l  <- which.min(abs(atot - (0.5 * h * (seq(from = (ind-1), to = 1)))))
    bin.l  <- tmp$mids[ind.l]

    ## Right side
    atot   <- sum(tmp$counts[ind:nmax])
    ind.r  <- ind + which.min(abs(atot - (0.5 * h * (seq(from = (ind+1), to = nmax)-ind))))
    bin.r  <- tmp$mids[ind.r]

    ## Debug plotting
    if (FALSE) {
        plot(tmp$mids, tmp$counts, type = "b")
        segments(x0 = tmp$mids[ind], y0 = h, x1 = bin.l, y1 = 0, col = "red")
        segments(x0 = tmp$mids[ind], y0 = h, x1 = bin.r, y1 = 0, col = "red")
    }
    
    ## Calculate the TINN-value
    bin.r - bin.l

}


#' Produce a Poincare plot.
#'
#' @param ibi A vector with the interbeat intervals (IBIs).
#' 
#' @return Nothing
#' 
#' @export
plot_poincare <- function(ibi) {
    N <- length(ibi)
    x <- ibi[1:(N-1)]
    y <- ibi[2:N]

    ## Determine the centre of mass
    xm <- mean(x)
    ym <- mean(y)

    ## Get the values for SD1 and SD2
    sd1 <- ibi_sd1(ibi)
    sd2 <- ibi_sd2(ibi)

    ## Calculate coordinates for the sd1 and sd2 lines
    sd2d  <- sqrt(2 * sd2^2 / sqrt(2))
    sd1d  <- sqrt(2 * sd1^2 / sqrt(2))

    x.sd2.1 <- xm - sd2d
    y.sd2.1 <- ym - sd2d
    x.sd2.2 <- xm + sd2d
    y.sd2.2 <- ym + sd2d

    x.sd1.1 <- xm - sd1d
    y.sd1.1 <- ym + sd1d
    x.sd1.2 <- xm + sd1d
    y.sd1.2 <- ym - sd1d

    plot(x, y, cex = 0.5, pch = 20)
    points(xm, ym, pch = 21, col = "black", bg = "red", lwd = 3, cex = 3)

    segments(x.sd2.1, y.sd2.1, x.sd2.2, y.sd2.2, col = "red", lwd = 2)
    segments(x.sd1.1, y.sd1.1, x.sd1.2, y.sd1.2, col = "red", lwd = 2)

    text(0.9*x.sd1.1, 1.1*y.sd1.1, "SD1", cex = 1, col = "red")
    text(1.1*x.sd2.2, 1.1*y.sd2.2, "SD2", cex = 1, col = "red")

    require(plotrix)
    draw.ellipse(x = xm, y = ym, a = (2 / sqrt(2))*sd1d, b = (2 / sqrt(2))*sd2d, angle = -45, border = "red", lwd = 3)
    ## abline(a = 0, b = 1, col = "blue")
}
