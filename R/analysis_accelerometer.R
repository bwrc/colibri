
#' Accelerometer analysis.
#'
#' @param metric.list  A list of time-domain metrics as strings.
#' @param settings The settings structure used for analysis.
#' @param acc A vector with the accelerometer signal.
#' @param t.acc A time vector for the signal.
#' 
#' @return A list with the values of the metrics.
#' 
#' @family accelerometer
#' 
#' @export
analyse_acceleration <- function (metric.list, settings, acc, t.acc = NULL){
  res <- lapply(metric.list, analyse_acceleration_helper, acc, settings)
}


#' Helper function for accelerometer analysis
#'
#' @param metric A time-domain metric to be estimated.
#' @param acc A vector with the accelerometer signal.
#' @param settings The settings structure used for analysis. (Not used but reserved for future.)
#' 
#' @return The value of the metric.
#'
#' @family accelerometer
#' 
#' @keywords internal
analyse_acceleration_helper <- function (metric, acc, settings){
  switch( metric,
          min = matrix(dimnames = list(metric, "value"), min(acc)),
          max = matrix(dimnames = list(metric, "value"), max(acc)),
          mean = matrix(dimnames = list(metric, "value"), mean(acc)),
          median = matrix(dimnames = list(metric, "value"), median(acc)),
          sd = matrix(dimnames = list(metric, "value"), sd(acc)),
          se = matrix(dimnames = list(metric, "value"), se(acc)),
  )
}
