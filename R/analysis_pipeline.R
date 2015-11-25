#' Analyze all recordings in a recording collection.
#'
#' All blocks in all recordings in the recording collection  are analyzed.
#'
#' @param recording A recording structure
#' @param settings The settings structure used for HRV analysis (see \code{\link{settings.template}}).
#' @param signal A string or a list with the names of the signals to analyze.
#' @param analysis.pipeline.function The pipeline function used in the
#' analysis, i.e., a funtion containing the individual analysis steps.
#'
#' @return The recording collection with the result added to each of the individual recordings.
#' 
#' @family pipeline
#'
#' @export
analyze_recording_collection <- function(recording.collection, settings, signal, analysis.pipeline.function) {
    lapply(recording.collection, function(i) analyze_recording(i, settings, signal, analysis.pipeline.function))
}


#' Analyze a recording.
#'
#' All blocks in the recording are analyzed.
#'
#' @param recording A recording structure
#' @param settings The settings structure used for HRV analysis (see \code{\link{settings.template}}).
#' @param signal A string or a list with the names of the signals to analyze.
#' @param analysis.pipeline.function The pipeline function used in the
#' analysis, i.e., a funtion containing the individual analysis steps.
#'
#' @return The recording structure with the result from the individual blocks added to the results structure in the recording.
#' 
#' @family pipeline
#'
#' @export
analyze_recording <- function(recording, settings, signal, analysis.pipeline.function) {
    n.blocks          <- nrow(recording$conf$blocks)
    
    for (i in seq.int(n.blocks)) {
        block     <- recording$conf$blocks[i,]
        recording <- analyze_block(recording, settings, signal, block, analysis.pipeline.function = analysis.pipeline.function)
    }

    recording
}


#' Analyze results in a block.
#'
#' Analyze the results in a particular block.
#' 
#' @param recording A recording structure
#' @param settings The settings structure used for the analysis (see e.g., \code{\link{settings_template_hrv}}).
#' @param signal A string or a list with the names of the signals to analyze.
#' @param block A block structure.
#' @param signal The name 
#' @param analysis.pipeline.function The pipeline function used in the
#' analysis, i.e., a funtion containing the individual analysis steps.
#'
#' @return The recording structure with the result from the block added to the results structure in the recording.
#' 
#' @family pipeline
#'
#' @export
analyze_block <- function(recording, settings, signal, block, analysis.pipeline.function) {
    block.s       <- block_to_seconds(recording, block = block)
    data.segments <- generate_segments_from_block(block.s, settings)
    nsegments     <- nrow(data.segments)

    ## Now loop over the segments applying the analysis pipeline to each segment
    res.list <- vector(mode = "list", length = nsegments)

    for (i in seq.int(nsegments)) {
        ## Extract the data corresponding to this segment
        res           <- extract_segment_s(recording, data.segments[i,], signal = signal)

        res.seg       <- analysis.pipeline.function(settings, res)
        res.seg       <- do.call(rbind, res.seg)

        tmp           <- matrix(ncol = 3, nrow = nrow(res.seg))
        colnames(tmp) <- c("block", "segment", "timestamp")
        tmp[, 1]       <- block$blockid
        tmp[, 2]       <- i
        tmp[, 3]       <- as.numeric( recording$properties$zerotime + data.segments[i,1] +
                                     (recording$conf$settings$segment.length / 2) )

        res.seg       <- cbind(res.seg, tmp)

        res.list[[i]] <- res.seg
    }

    recording$results[[as.character(block$blockid)]] <- res.list
    
    recording
}


#' Calculate results.
#'
#' This function defines the analysis pipeline for HRV analysis. If new analysis
#' functions are incorporated these analysis steps should be added to
#' this function as well.
#'
#' @param settings The settings structure used for HRV analysis (see \code{\link{settings.template}}).
#' @param ibi A vector with the interbeat intervals (IBIs).
#' @param t.ibi A vector with the times of occurrence of the interbeat intervals (IBIs).
#'
#' @return The analysis results as a named list.
#' 
#' @family pipeline
#'
#' @export
analysis_pipeline_ibi <- function(settings, data) {
    ## Unpack the data
    ibi   <- data$data
    t.ibi <- data$t

    ## container for results
    res <- c()
    
    ## Time domain analysis
    if (settings$analysis$time)
        res <- c(res, analyse_timedomain(settings$timedomain$metric.list, settings, ibi))

    ## Frequency domain analysis
    if (settings$analysis$frequency)
        res <- c(res, analyse_frequencydomain(settings$frequencydomain$metric.list, ibi, t.ibi, settings))
    
    ## Geometric analysis
    if (settings$analysis$geometric)
        res <- c(res, analyse_geometric(settings$geometric$metric.list, ibi, t.ibi, settings))

    ## Nonlinear analysis
    if (settings$analysis$nonlinear)
        res <- c(res, analyse_nonlinear(settings$nonlinear$metric.list, ibi, t.ibi, settings))

    ## Return results
    res
}
