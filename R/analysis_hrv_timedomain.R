#' Time-domain HRV analysis.
#'
#' @param metric.list  A list of time-domain metrics as strings.
#' @param ibi A vector with the interbeat intervals (IBIs).
#' @param t.ibi A vector with the times of occurrence of the interbeat intervals (IBIs).
#' @param settings The settings structure used for HRV analysis (see \code{\link{settings.template}}).
#' 
#' @return A list with the values of the metrics.
#' 
#' @family HRV time-domain
#' 
#' @export
analyse.timedomain <- function(metric.list, settings, ibi, t.ibi = NULL) {

    res <- lapply(metric.list, analyse.timedomain.helper, ibi, settings)
}


#' Helper function for time-domain HRV analysis.
#'
#' @param metric A time-domain metric to be estimated.
#' @param ibi An array with interbeat intervals.
#' @param settings The settings structure used for HRV analysis.
#' 
#' @return The value of the metric.
#'
#' @family HRV timey-domain
#' 
#' @keywords internal
analyse.timedomain.helper <- function(metric, ibi, settings) {

    switch(metric,
           mean    = matrix(dimnames = list(metric, "value"), ibi.mean(ibi)),
           meanhr  = matrix(dimnames = list(metric, "value"), ibi.mean(ibi, type = "hr")),
           stdev   = matrix(dimnames = list(metric, "value"), ibi.stdev(ibi)),
           stdevhr = matrix(dimnames = list(metric, "value"), ibi.stdev(ibi, type = "hr")),
           rmssd   = matrix(dimnames = list(metric, "value"), ibi.rmssd(ibi)),
           var     = matrix(dimnames = list(metric, "value"), ibi.var(ibi)),
           nnx     = ibi.pnnx.helper(ibi, settings$timedomain$parameters$pnnx, type = "number"),
           pnnx    = ibi.pnnx.helper(ibi, settings$timedomain$parameters$pnnx, type = "percentage")
           )
}

#' Calculate the average of interbeat intervals or heart rate.
#' The time series is in miliseconds
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param type String denoting whether to calculate the average interbeat interval (type = "ibi", default) or the average heart rate (type = "hr").
#' 
#' @return The average interbeat interval or the average heart rate.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi.mean <- function(ibi, type = "ibi") {
    if (type == "ibi")
        res <- mean(ibi)
    
    if (type == "hr")
        res <- mean(6e4 / ibi)

    res
}


#' Calculate the root-mean squared of successive differences (RMSSD).
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' 
#' @return The RMSSD value.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi.rmssd <- function(ibi) {
    sqrt(sum(diff(ibi)^2) / (length(ibi) - 1))
}


#' Calculate the variance of interbeat intervals.
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' 
#' @return The variance of interbeat intervals.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi.var <- function(ibi) {
    var(ibi)
}


#' Calculate the standard deviation of interbeat intervals or heart rate.
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param type String denoting whether to calculate the average interbeat interval (type = "ibi", default) or the average heart rate (type = "hr").
#' 
#' @return The standard deviation of interbeat intervals or heart rate.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi.stdev <- function(ibi, type = "ibi") {
    if (type == "ibi")
        res <- sd(ibi)

    if (type == "hr")
        res <- sd(6e4 / ibi)

    res
}


#' Helper function for calculating the pNNx value.
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param pnnx.list A list of numbers with the limits in milliseconds (the x in pNNx and NNx) for calculating the pNNx and NNx.
#' @param type A string denoting what to calculate; type =
#' "percentage" to calculate the percentage of NN values (default), or
#' type = "number" to calculate the number of NN intervals.
#' 
#' @return A list with the pNNx or NNx values.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi.pnnx.helper <- function(ibi, pnnx.list, type = "percentage") {

    res           <- matrix(data = NA, ncol = 1, nrow = length(pnnx.list))
    rownames(res) <- paste("pnnx", pnnx.list, sep = "")
    colnames(res) <- "value"

    if (type == "percentage")
        tmp.func <- ibi.pnnx
    if (type == "number")
        tmp.func <- ibi.nnx
    
    for (i in seq(length(pnnx.list))) {
        res[i,] <- tmp.func(ibi, x = pnnx.list[i])
    }

    res
}


#' Calculate the NNx value.
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param x A number with the limit in milliseconds (the x in NNx) for calculating the NNx; the number of interbeat intervals differing more than x ms. Default is 50.
#' 
#' @return The NNx value.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi.nnx <- function(ibi, x = 50) {
    sum(abs(diff(ibi)) >= x)
}


#' Calculate the NNx value.
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param x A number with the limit in milliseconds (the x in pNNx) for calculating the NNx; the percentage of interbeat intervals differing more than x ms. Default is 50.
#' 
#' @return The pNNx value.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi.pnnx <- function(ibi, x = 50) {
    100 * ibi.nnx(ibi, x) / length(ibi)
}
