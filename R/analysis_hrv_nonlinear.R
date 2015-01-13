#' Nonelinear HRV analysis.
#'
#' @param metric.list  A list of frequency-domain metrics as strings.
#' @param ibi A vector with the interbeat intervals (IBIs).
#' @param t.ibi A vector with the times of occurrence of the interbeat intervals (IBIs).
#' @param settings The settings structure used for HRV analysis with the following fields:
#' 
#' \describe{
#' \item{$nonlinear$embedding.dimension}{The embedding dimension used in the calculation of entropy}
#' }
#' 
#' @return A list with the values of the metrics.
#' 
#' @family HRV nonlinear
#' 
#' @export
analyse_nonlinear <- function(metric.list, ibi, t.ibi = NULL, settings) {
    res <- lapply(metric.list, analyse_nonlinear_helper, ibi, settings)
}


#' Helper function for nonlinear HRV analysis.
#'
#' @param metric A frequency-domain metric to be estimated.
#' @param x A vector with the interbeat intervals (IBIs).
#' @param settings The settings structure used for HRV analysis.
#' 
#' @return The value of the metric.
#'
#' @family HRV nonlinear
#' 
#' @keywords internal
analyse_nonlinear_helper <- function(metric, x, settings) {

    switch(metric,

           sampen = matrix(dimnames = list(metric, "value"), ibi_entropy(x, embedding.dimension = settings$nonlinear$embedding.dimension, type = "sample")),
           apen   = matrix(dimnames = list(metric, "value"), ibi_entropy(x, embedding.dimension = settings$nonlinear$embedding.dimension, type = "approximate"))

           )
}


#' Calculate entropy of the IBI series
#'
#' @param x A vector with the interbeat intervals (IBIs).
#' @param embedding.dimension The embedding dimension of the entropy. The default is 2.
#' @param type The type of entropy as a string: \code{approximate} or \code{sample}
#' 
#' @return The entropy.
#'
#' @family HRV nonlinear
#' 
#' @export
ibi_entropy <- function(x, embedding.dimension = 2, type = NULL) {
    if (is.null(type))
        stop("Entropy type not defined.")

    if (type == "approximate")
        approx_entropy(x, edim = edim)

    if (type == "sample")
        sample_entropy(x, edim = edim)
}
