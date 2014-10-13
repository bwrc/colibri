#' Automatically detect and remove artifacts.
#'
#' Artifact detection is performed using the method presented by Xu
#' and Schuckers \code{\link{detect_artifacts_xu}}. The interbeat
#' intervals (IBIs) that are categorized as artifactual are removed
#' from the IBI series and from the time vector.
#'
#' @param recording A recording structure.
#' @param signal A signal name found in the recording. Optional, default is \code{ibi}.
#' 
#' @return The recording with the artifacts removed.
#' 
#' @family HRV
#' 
#' @export
remove_ibi_artifacts <- function(recording, signal = "ibi") {
    ## Remove the artifacts
    ind.artifact                    <- detect_artifacts_xu(recording$signal[[signal]]$data)
    recording$signal[[signal]]$data <- recording$signal[[signal]]$data[-ind.artifact]
    recording$signal[[signal]]$t    <- recording$signal[[signal]]$t[-ind.artifact]

   recording 
}
