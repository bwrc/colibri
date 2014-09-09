#' Analyze all recordings in a recording collection.
#'
#' All blocks in all recordings in the recording collection  are analyzed.
#'
#' @param recording A recording structure
#' @param settings The settings structure used for HRV analysis (see \code{\link{settings.template}}).
#' @param analysis.pipeline.function The pipeline function used in the
#' analysis, i.e., a funtion containing the individual analysis steps.
#'
#' @return The recording collection with the result added to each of the individual recordings.
#' 
#' @family pipeline
#'
#' @export
analyze.recording.collection <- function(recording.collection, settings, analysis.pipeline.function = analysis.pipeline) {
    lapply(recording.collection, function(i) analyze.recording(i, settings, analysis.pipeline))
}


#' Analyze a recording.
#'
#' All blocks in the recording are analyzed.
#'
#' @param recording A recording structure
#' @param settings The settings structure used for HRV analysis (see \code{\link{settings.template}}).
#' @param analysis.pipeline.function The pipeline function used in the
#' analysis, i.e., a funtion containing the individual analysis steps.
#'
#' @return The recording structure with the result from the individual blocks added to the results structure in the recording.
#' 
#' @family pipeline
#'
#' @export
analyze.recording <- function(recording, settings, analysis.pipeline.function = analysis.pipeline) {
    n.blocks          <- nrow(recording$conf$blocks)
    
    for (i in seq.int(n.blocks)) {
        block     <- recording$conf$blocks[i,]
        recording <- analyze.block(recording, settings, block, analysis.pipeline.function = analysis.pipeline.function)
    }

    recording
}


#' Analyze results in a block.
#'
#' Analyze the results in a particular block.
#' 
#' @param recording A recording structure
#' @param settings The settings structure used for HRV analysis (see \code{\link{settings.template}}).
#' @param block A block structure.
#' @param analysis.pipeline.function The pipeline function used in the
#' analysis, i.e., a funtion containing the individual analysis steps.
#'
#' @return The recording structure with the result from the block added to the results structure in the recording.
#' 
#' @family pipeline
#'
#' @export
analyze.block <- function(recording, settings, block, analysis.pipeline.function) {
    block.s       <- block.to.seconds(recording, block = block)
    data.segments <- generate.segments.from.block(block.s, settings)
    nsegments     <- nrow(data.segments)

    ## Now loop over the segments applying the analysis pipeline to each segment
    res.list <- vector(mode = "list", length = nsegments)

    for (i in seq(nsegments)) {
        ## Extract the data corresponding to this segment
        res           <- extract.segment.s(recording, data.segments[i,], signal = "ibi")

        res.seg       <- analysis.pipeline.ibi(settings, ibi = res$data, t.ibi = res$t)
        res.seg       <- do.call(rbind, res.seg)

        tmp           <- matrix(ncol = 2, nrow = nrow(res.seg))
        colnames(tmp) <- c("block", "segment")
        tmp[, 1]       <- block$blockid
        tmp[, 2]       <- i

        res.seg       <- cbind(res.seg, tmp)

        res.list[[i]] <- res.seg
    }

    recording$results[[as.character(block$blockid)]] <- res.list
    
    recording
}


#' #' Calculate results.
#'
#' This function defines the analysis pipeline. If new analysis
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
analysis.pipeline.ibi <- function(settings, ibi, t.ibi) {
    ## container for results
    res <- c()
    
    ## Time domain analysis
    if (settings$analysis$time)
        res <- c(res, analyse.timedomain(settings$timedomain$metric.list, settings, ibi))

    ## Frequency domain analysis
    if (settings$analysis$frequency)
        res <- c(res, analyse.frequencydomain(settings$frequencydomain$metric.list, ibi, t.ibi, settings))
    
    ## Geometric analysis
    if (settings$analysis$geometric)
        res <- c(res, analyse.geometric(settings$geometric$metric.list, ibi, t.ibi, settings))

    ## Nonlinear analysis
    if (settings$analysis$frequency)
        res <- c(res, analyse.nonlinear(settings$nonlinaer$metric.list, ibi, t.ibi, settings))

    ## Return results
    res
}
