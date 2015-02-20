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
analyse_timedomain <- function(metric.list, settings, ibi, t.ibi = NULL) {

    res <- lapply(metric.list, analyse_timedomain_helper, ibi, settings)
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
analyse_timedomain_helper <- function(metric, ibi, settings) {

    switch(metric,
           min     = matrix(dimnames = list(metric, "value"), ibi_min(ibi)),
           max     = matrix(dimnames = list(metric, "value"), ibi_max(ibi)),
           median  = matrix(dimnames = list(metric, "value"), ibi_max(ibi)),

           minhr     = matrix(dimnames = list(metric, "value"), ibi_min(ibi, type = "hr")),
           maxhr     = matrix(dimnames = list(metric, "value"), ibi_max(ibi, type = "hr")),
           medianhr  = matrix(dimnames = list(metric, "value"), ibi_max(ibi, type = "hr")),

           mean    = matrix(dimnames = list(metric, "value"), ibi_mean(ibi)),
           meanhr  = matrix(dimnames = list(metric, "value"), ibi_mean(ibi, type = "hr")),

           stdev   = matrix(dimnames = list(metric, "value"), ibi_stdev(ibi)),
           stdevhr = matrix(dimnames = list(metric, "value"), ibi_stdev(ibi, type = "hr")),

           rmssd   = matrix(dimnames = list(metric, "value"), ibi_rmssd(ibi)),
           var     = matrix(dimnames = list(metric, "value"), ibi_var(ibi)),

           nnx     = ibi_pnnx_helper(ibi, settings$timedomain$parameters$pnnx, type = "number"),
           pnnx    = ibi_pnnx_helper(ibi, settings$timedomain$parameters$pnnx, type = "percentage")
           )
}

#' Calculate the minimum of interbeat intervals or heart rate.
#' The time series is in miliseconds
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param type String denoting whether to use ibi (type = "ibi", default) or heart rate (type = "hr").
#' 
#' @return The minimum interbeat interval or heart rate.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi_min <- function(ibi, type = "ibi") {
    if (type == "ibi")
        res <- min(ibi)
    
    if (type == "hr")
        res <- min(6e4 / ibi)

    res
}


#' Calculate the maximum of interbeat intervals or heart rate.
#' The time series is in miliseconds
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param type String denoting whether to use ibi (type = "ibi", default) or heart rate (type = "hr").
#' 
#' @return The maximum interbeat interval or heart rate.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi_max <- function(ibi, type = "ibi") {
    if (type == "ibi")
        res <- max(ibi)
    
    if (type == "hr")
        res <- max(6e4 / ibi)

    res
}


#' Calculate the median of interbeat intervals or heart rate.
#' The time series is in miliseconds
#'
#' @param ibi An array with interbeat intervals in milliseconds.
#' @param type String denoting whether to calculate the median interbeat interval (type = "ibi", default) or the median heart rate (type = "hr").
#' 
#' @return The median interbeat interval or the median heart rate.
#'
#' @family HRV time-domain
#' 
#' @keywords internal
ibi_median <- function(ibi, type = "ibi") {
    if (type == "ibi")
        res <- median(ibi)
    
    if (type == "hr")
        res <- median(6e4 / ibi)

    res
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
ibi_mean <- function(ibi, type = "ibi") {
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
ibi_rmssd <- function(ibi) {
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
ibi_var <- function(ibi) {
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
ibi_stdev <- function(ibi, type = "ibi") {
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
ibi_pnnx_helper <- function(ibi, pnnx.list, type = "percentage") {

    if (type == "percentage")
        rtype <- "pnnx"
    if (type == "number")
        rtype <- "nnx"

    res           <- matrix(data = NA, ncol = 1, nrow = length(pnnx.list))
    rownames(res) <- paste(rtype, pnnx.list, sep = "")
    colnames(res) <- "value"

    if (type == "percentage")
        tmp.func <- ibi_pnnx
    if (type == "number")
        tmp.func <- ibi_nnx
    
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
ibi_nnx <- function(ibi, x = 50) {
    sum(abs(diff(ibi)) > x)
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
ibi_pnnx <- function(ibi, x = 50) {
    100 * ibi_nnx(ibi, x) / (length(ibi) - 1)
}
