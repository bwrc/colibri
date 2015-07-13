#' Automatically detect and remove artifacts.
#'
#' Artifact detection is performed using the method presented by Xu
#' and Schuckers \code{\link{detect_artifacts_xu}}. The interbeat
#' intervals (IBIs) that are categorized as artifactual are removed
#' from the IBI series and from the time vector.
#'
#' @param recording A recording structure.
#' @param signal A signal name found in the recording. Optional, default is \code{ibi}.
#' @param signal2 An additional signal from which the indices found to be artifacts in \code{signal} are removed too. Default is \code{ibi.amp}.
#'
#' @return The recording with the artifacts removed.
#'
#' @family HRV
#'
#' @export
remove_ibi_artifacts <- function(recording, signal = "ibi", signal2 = "ibi.amp") {
    ## Remove the artifacts
    ind.artifact                    <- detect_artifacts_xu(recording$signal[[signal]]$data)

    if (length(ind.artifact) > 0) {
        recording$signal[[signal]]$data <- recording$signal[[signal]]$data[-ind.artifact]
        recording$signal[[signal]]$t    <- recording$signal[[signal]]$t[-ind.artifact]

        if (signal2 %in% names(recording$signal)) {
            recording$signal[[signal2]]$data <- recording$signal[[signal2]]$data[-ind.artifact]
            recording$signal[[signal2]]$t    <- recording$signal[[signal2]]$t[-ind.artifact]
        }
    }

    recording
}
