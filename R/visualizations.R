#' Plot the signal data in a measurement block.
#'
#' @param recording A recording structure.
#' @param blockid The ID of the measurement block.
#' @param signal The name of the signal in the recording structure to be plotted.
#' @param type The type of the x-axis (optional, default is seconds):
#' \describe{
#' \item{seconds}{Plot seconds on the x-axis.}
#' \item{timestamp}{Plot timestamps on the x-axis.}
#' }
#'
#' @param interval The interval at which to plot x-axis markers. Optional, default is 10.
#' @param new.plot Boolean indicating whether a new plot window should be opened (optional, default is TRUE).
#' @param filename The full path and filename in which to save the figure. If NULl (default), the figure is not saved. The format of the figure (PDF or PNG) is automatically determined from the filename extension. The size of the figure is an A4 in landscape mode.
#'
#' @return Nothing. Produces a plot or saves the figure.
#' 
#' @family visualizations
#'
#' @export
plot_block <- function(recording, blockid, signal, type = "seconds", interval = 10, new.plot = TRUE, filename = NULL) {
    ## Should the figure be saved
    if (! is.null(filename)) {
        format <- get_file_extension(filename)

        if (format == "pdf")
            pdf(file = filename, width = 29.7 / 2.54, height = 21.0 / 2.54, paper = "a4r")

        if (format == "png")
            png(file = filename, width = 297, height = 210, units = "mm", res = 300)

        new.plot <- FALSE
    }

    if (new.plot)
        plot.new()

    blockid.tmp <- blockid
    block       <- subset(recording$conf$blocks, blockid == blockid.tmp)
    tmp         <- extract_segment_block(recording, blockid = blockid.tmp, signal = signal)

    ## unit for y-axis label
    if ("unit" %in% names(recording$signal[[signal]])) {
        unit <- recording$signal[[signal]][["unit"]]
    }  else {
        unit <- ""
    }

    plot(tmp$t, tmp$data,
         type = "l",
         xaxt = "n",
         main = paste("Case: ", block$casename, "     Task: ", block$tasktype, " [block: ", block$blockid, "]", sep = ""),
         ylab = paste(signal, " [", unit, "]", sep = ""),
         xlab = ""
         )

    grid(10, lty = 2, col = "blue")
    if (type == "seconds") {
        ts <- seq(from = min(tmp$t), to = max(tmp$t), by = interval)
        axis(1, at = ts, labels = round(ts))
        mtext(text = "time [seconds]", side = 1, line = 3)

    }

    if (type == "timestamp") {
        ts <- seq(from = min(tmp$t), to = max(tmp$t), by = 60)
        tt <- tmp$t + recording$properties$time.start
        lb <-format.POSIXct(seq(min(tt), max(tt), by = "min"), format = "%H:%M")
        axis(1, at = ts, labels = lb)
        mtext(text = "time", side = 1, line = 3)
    }

    if (! is.null(filename))
        dev.off()
}


#' Plot signal data and also show block limits and tasks types for each of the blocks.
#'
#' @param recording A recording structure.
#' @param signal The name of the signal in the recording structure to be plotted.
#' @param type The type of the x-axis (optional, default is seconds):
#' \describe{
#' \item{seconds}{Plot seconds on the x-axis.}
#' \item{timestamp}{Plot timestamps on the x-axis.}
#' }
#'
#' @param interval The interval at which to plot x-axis markers. Optional, default is 15.
#' @param new.plot Boolean indicating whether a new plot window should be opened (optional, default is TRUE).
#' @param filename The full path and filename in which to save the figure. If NULl (default), the figure is not saved. The format of the figure (PDF or PNG) is automatically determined from the filename extension. The size of the figure is an A4 in landscape mode.
#'
#' @return Nothing. Produces a plot or saves the figure.
#' 
#' @family visualizations
#'
#' @export
plot_all_blocks <- function(recording, signal, type = "seconds", interval = 15, new.plot = TRUE, filename = NULL) {
    ## Should the figure be saved
    if (! is.null(filename)) {
        format <- get_file_extension(filename)

        if (format == "pdf")
            pdf(file = filename, width = 29.7 / 2.54, height = 21.0 / 2.54, paper = "a4r")

        if (format == "png")
            png(file = filename, width = 297, height = 210, units = "mm", res = 300)

        new.plot <- FALSE
    }

    if (new.plot)
        plot.new()
    
    ## Plot the signal
    y <- recording$signal[[signal]][["data"]]
    x <- recording$signal[[signal]][["t"]]

    ## unit for y-axis label
    if ("unit" %in% names(recording$signal[[signal]])) {
        unit <- recording$signal[[signal]][["unit"]]
    }  else {
        unit <- ""
    }

    ## Plot the data
    plot(x, y,
         type = "l",
         xaxt = "n",
         main = paste("case: ", recording$properties$casename, sep = ""),
         ylab = paste(signal, " [", unit, "]", sep = ""),
         xlab = "")

    ## Plot the block limits and the task types
    for (i in seq.int(nrow(recording$conf$blocks))) {
        block <- block_to_seconds(recording, recording$conf$blocks[i,])

        abline(v = block$starttime, col = "red", lty = 1)
        abline(v = block$stoptime, col = "red", lty = 1)

        level <- mean(y) * 1.5 + 0.1*mean(y) * rnorm(1)
        level <- level * 1.2 * 0.4
        text(x = block$starttime, y = level*0.95, labels = block$tasktype, adj = 0, col = "red")
        segments(block$starttime, level, block$stoptime, level, col = "red", lty = 2)
        segments(block$starttime, level*0.91, block$stoptime, level*0.91, col = "red", lty = 2)
    }

    if (type == "seconds") {
        ts <- seq(from = min(x), to = max(x), by = interval)
        axis(1, at = ts, labels = round(ts))
        mtext(text = "time [seconds]", side = 1, line = 3)

    }

    if (type == "timestamp") {
        ts <- seq(from = min(x), to = max(x), by = 60)
        tt <- as.POSIXct(x + recording$properties$time.start)
        lb <-format.POSIXct(seq(min(tt), max(tt), by = "min"), format = "%H:%M")
        axis(1, at = ts, labels = lb)
        mtext(text = "time", side = 1, line = 3)
    }

    if (! is.null(filename))
        dev.off()

}


#' Check the quality of R-peak detection.
#'
#' Plot the raw ECG data together with interbeat-interval data. This allows the quality of the R-peak detection to be checked visually.
#'
#' @param recording A recording structure.
#' @param blockid The ID of the measurement block.
#' @param ecg.signal.name  The name of the ECG signal in the recording structure (default is 'ECG')
#' @param ibi.signal.name  The name of the interbeat interval (IBI) signal in the recording structure (default is 'ibi.amp', to ensure tha the R-peaks are plotted at the correct height.)
#' @param type The type of the x-axis (optional, default is seconds):
#' \describe{
#' \item{seconds}{Plot seconds on the x-axis.}
#' \item{timestamp}{Plot timestamps on the x-axis.}
#' }
#'
#' @param interval The interval at which to plot x-axis markers. Optional, default is 15.
#' @param new.plot Boolean indicating whether a new plot window should be opened (optional, default is TRUE).
#' @param filename The full path and filename in which to save the figure. If NULl (default), the figure is not saved. The format of the figure (PDF or PNG) is automatically determined from the filename extension. The size of the figure is an A4 in landscape mode.
#'
#' @return Nothing. Produces a plot or saves the figure.
#' 
#' @family visualizations
#' 
#' @export
plot_check_rr_detection <- function(recording, blockid, ecg.signal.name = "ECG", ibi.signal.name = "ibi.amp", type = "seconds", interval = 10, new.plot = TRUE, filename = NULL) {
    ## Should the figure be saved
    if (! is.null(filename)) {
        format <- get_file_extension(filename)

        if (format == "pdf")
            pdf(file = filename, width = 29.7 / 2.54, height = 21.0 / 2.54, paper = "a4r")

        if (format == "png")
            png(file = filename, width = 297, height = 210, units = "mm", res = 300)

        new.plot <- FALSE
    }

    if (new.plot)
        plot.new()

    blockid.tmp <- blockid
    block       <- subset(recording$conf$blocks, blockid == blockid.tmp)
    tmp.ecg     <- extract_segment_block(recording, blockid = blockid.tmp, signal = ecg.signal.name)
    tmp.ibi     <- extract_segment_block(recording, blockid = blockid.tmp, signal = ibi.signal.name)

    ## unit for y-axis label
    signal <- ecg.signal.name
    if ("unit" %in% names(recording$signal[[signal]])) {
        unit <- recording$signal[[signal]][["unit"]]
    }  else {
        unit <- ""
    }

    plot(tmp.ecg$t, tmp.ecg$data,
         type = "l",
         xaxt = "n",
         main = paste("Task: ", block$tasktype, " [block: ", block$blockid, "]", sep = ""),
         ylab = paste("ECG", " [", unit, "]", sep = ""),
         xlab = ""
         )

    points(tmp.ibi$t, tmp.ibi$data, col = "red",
           type = "p",
           xaxt = "n",
           main = paste("Task: ", block$tasktype, " [block: ", block$blockid, "]", sep = ""),
           ylab = paste("IBI", " [", unit, "]", sep = ""),
           xlab = ""
           )

    grid(10, lty = 2, col = "blue")
    if (type == "seconds") {
        ts <- seq(from = min(tmp.ecg$t), to = max(tmp.ecg$t), by = interval)
        axis(1, at = ts, labels = round(ts))
        mtext(text = "time [seconds]", side = 1, line = 3)

    }

    if (type == "timestamp") {
        ts <- seq(from = min(tmp.ecg$t), to = max(tmp.ecg$t), by = 60)
        tt <- as.POSIXct(tmp.ecg$t + str_to_timestamp(block$starttime))
        lb <-format.POSIXct(seq(min(tt), max(tt), by = "min"), format = "%H:%M")
        axis(1, at = ts, labels = lb)
        mtext(text = "time", side = 1, line = 3)
    }

    if (! is.null(filename))
        dev.off()
}


#' Produce a multipage PDF file with plots showing the entire raw ECG signal together with markers for the R-peaks.
#'
#' Plot the raw ECG data together with R-peaks. This allows the quality of the R-peak detection to be checked visually for the entire recording.
#'
#' @param recording A recording structure.
#' @param filename The full path and filename in which to save the figure. The format is always PDF.
#' @param n The number of seconds of ECG data to show per row in the plot (default is 60 seconds).
#' @param signal The name of the ECG signal in the recording structure (default is 'ECG').
#' @param signal.color The line color of the ECG signal. Default is 'blue'.
#' @param signal2 The name of the second signal in the recording structure (default is NULL, but 'ibi.amp' would be a typical choice).
#' @param signal2.color The color of the points for the secondary signal. Default is 'red'.
#' @param signal2.pch The point type used to plot the secondary signal. Default is 10.
#' @param signal2.cex The point size used to plot the secondary signal. Default is 2.
#' 
#' @return Nothing. Saves the figure as multipage PDF.
#' 
#' @family visualizations
#' 
#' @export
plot_ecg_r_peak <- function(recording, filename, n = 60, signal = "ECG", signal.color = "blue",
                            signal2 = NULL, signal2.color = "red", signal2.pch = 10, signal2.cex = 2,
                            x.axis.type = "numeric", blockIDfield = 'task') {

    require(gplots)

    fs <- recording$signal[[signal]]$samplingrate
    ns <- n * fs
    np <- ceiling(length(recording$signal[[signal]]$data) / ns)

    ## blocks
    if (nrow(recording$conf$blocks) > 0) {
        blocks <- do.call(rbind,  lapply(seq.int(nrow(recording$conf$blocks)), function(i) block_to_seconds(recording, recording$conf$blocks[i,])))
    }

    pdf(filename, width = (21.0 / 2.54), height = (29.7 / 2.54), paper = "a4")
    par(mfrow = c(5, 1))

    textplot(paste("subject:", recording$properties$subject), valign = "top", halign = "left", cex = 3, fixed.width = TRUE)
    textplot(paste("casename:", recording$properties$casename), valign = "top", halign = "left", cex = 3, fixed.width = TRUE)
    textplot(paste("signal:", signal), valign = "top", halign = "left", cex = 3, fixed.width = TRUE)
    textplot(paste("start time:", recording$properties$time.start), valign = "top", halign = "left", cex = 3)
    textplot(paste("end time:", recording$properties$time.stop), valign = "top", halign = "left", cex = 3)

    par(mfrow = c(10,1))
    ## bottom, left, top, right
    par(mai = c(0.25, 0.25, 0.15, 0.15))

    i.start <- 1

    for(i in seq.int(np)) {
        ## plot a window of data
        w.start <- (i.start / fs)
        w.stop  <- (i.start + ns - 1) / fs

        ## ecg signal
        signal.type <- "l"
        
        if (x.axis.type == "numeric"){
            # x -axis in seconds
            plot(recording$signal[[signal]]$t[i.start:(i.start + ns - 1)],
                 recording$signal[[signal]]$data[i.start:(i.start + ns - 1)],
                 type = signal.type,
                 col = signal.color)
          
            if (! is.null(signal2)) {
              ind <- which((recording$signal[[signal2]]$t >= w.start) & (recording$signal[[signal2]]$t <= w.stop))
              points(recording$signal[[signal2]]$t[ind], recording$signal[[signal2]]$data[ind], col = signal2.color, pch = signal2.pch, cex = signal2.cex)
            }
          
          ## add block start and stop if present in the window
          if (nrow(recording$conf$blocks) > 0) {
            ## block start
            b.ind <- which((blocks$starttime >= w.start) & (blocks$starttime <= w.stop))
            if (length(b.ind) > 0) {
              for(b.tmp in b.ind) {
                abline(v = blocks[b.tmp,]$starttime, col = "magenta", lwd = 2, lty = 1)
                rect(blocks[b.tmp,]$starttime, -200, blocks[b.tmp,]$starttime+10, 200, col = rgb(1, 0, 1, alpha = 0.5), border = NA)
                text(x = blocks[b.tmp,]$starttime, y = par('yaxp')[1], labels = paste("START: ", blocks[b.tmp,]$tasktype, sep = ""), adj = 0, col = "magenta")
              }
            }
            ## block stop
            b.ind <- which((blocks$stoptime >= w.start) & (blocks$stoptime <= w.stop))
            if (length(b.ind) > 0) {
              for(b.tmp in b.ind) {
                abline(v = blocks[b.tmp,]$stoptime, col = "magenta", lwd = 2, lty = 1)
                rect(blocks[b.tmp,]$stoptime-10, -200, blocks[b.tmp,]$stoptime, 200,  col = rgb(1, 0, 1, alpha = 0.5), border = NA)
                text(x = blocks[b.tmp,]$stoptime, y = par('yaxp')[1], labels = paste("STOP: ", blocks[b.tmp,]$tasktype, sep = ""), adj = 1, col = "magenta")
              }
            }
            
          }
          
          
        } else if (x.axis.type == "timestamp") {
          # x -axis in hh:mm:ss units
          tmpTimeVec <- recording$properties$time.start + recording$signal[[signal]]$t[i.start:(i.start + ns - 1)]
          plot(tmpTimeVec,
               recording$signal[[signal]]$data[i.start:(i.start + ns - 1)],
               type = signal.type,
               col = signal.color,
               xaxt = "n")
          r <- as.POSIXct(round(range(tmpTimeVec, na.rm=T), "secs"))
          if (diff(r) > 20 ){
              axis.POSIXct(1, at = seq(r[1], r[2], by = 5), format = "%H:%M:%S")  
          } else {
              axis.POSIXct(1, at = seq(r[1], r[2], by = 'sec'), format = "%H:%M:%S")
          }
       
          ## add r-peaks
          if (! is.null(signal2)) {
              ind <- which((recording$signal[[signal2]]$t >= w.start) & (recording$signal[[signal2]]$t <= w.stop))
              tmpTimeVec <- recording$properties$time.start +
                            recording$signal[[signal2]]$t[ind]
              points(tmpTimeVec, recording$signal[[signal2]]$data[ind], col = signal2.color, pch = signal2.pch, cex = signal2.cex)
          }
          
          ## add block start and stop if present in the window
          if (nrow(recording$conf$blocks) > 0) {
            ## block start
            b.ind <- which((blocks$starttime >= w.start) & (blocks$starttime <= w.stop))
            if (length(b.ind) > 0) {
              for(b.tmp in b.ind) {
                b.start.time <- recording$properties$time.start + blocks[b.tmp,]$starttime
                abline(v = b.start.time, col = "magenta", lwd = 2, lty = 1)
                rect(b.start.time, -200, b.start.time+10, 200, col = rgb(1, 0, 1, alpha = 0.5), border = NA)
                text(x = b.start.time, y = par('yaxp')[1], labels = blocks[b.tmp, blockIDfield], adj = 0, col = "magenta", cex=1.2)
              }
            }
            ## block stop
            b.ind <- which((blocks$stoptime >= w.start) & (blocks$stoptime <= w.stop))
            if (length(b.ind) > 0) {
              for(b.tmp in b.ind) {
                b.stop.time <- recording$properties$time.start + blocks[b.tmp,]$stoptime
                abline(v = b.stop.time, col = "magenta", lwd = 2, lty = 1)
                rect(b.stop.time-10, -200, b.stop.time, 200,  col = rgb(1, 0, 1, alpha = 0.5), border = NA)
                text(x = b.stop.time, y =  par('yaxp')[1], labels = blocks[b.tmp, blockIDfield], adj = 1, col = "magenta", cex=1.2)
              }
            }
            
          }
        
        } else {stop(sprintf('Unrecognized option "%s" for x.axis.type. Use one of the following: {"numeric", "timestamp"}.', x.axis.type))}
        
        i.start <- i.start + ns
    }

    dev.off()
}


#' Plot a metric.
#'
#' @param recording A recording structure. That must contain the results structure.
#' @param metric The metric to plot. The metric must be found in the results structure.
#' @param filename The full path and filename in which to save the figure. If NULl (default), the figure is not saved. The format of the figure (PDF or PNG) is automatically determined from the filename extension. The size of the figure is an A4 in landscape mode.
#' @param new.plot Boolean indicating whether a new plot window should be opened (optional, default is TRUE).
#' @param blockid The ID of the measurement block (optional, default is NULL, which corresponds to plotting all data).
#'
#' @return Nothing. Produces a plot or saves the figure.
#' 
#' @family visualizations
#' 
#' @export
plot_metric <- function(recording, metric, filename, new.plot = TRUE, blockid = NULL) {
    if (! "results" %in% names(recording))
        stop("Results structure not found in the recording. Cannot continue!")

    if (length(recording$results) < 1)
        stop("Results structure is empty. Cannot continue!")
    
    if (! metric %in% levels(results$variable))
        warning("Metric not present in results! Cannot continue!")

    ## Should the figure be saved
    if (! is.null(filename)) {
        format <- get_file_extension(filename)

        if (format == "pdf")
            pdf(file = filename, width = 29.7 / 2.54, height = 21.0 / 2.54, paper = "a4r")

        if (format == "png")
            png(file = filename, width = 297, height = 210, units = "mm", res = 300)

        new.plot <- FALSE
    }

    if (new.plot)
        plot.new()

    ## Get the data
    if (is.null(blockid)) {
        res.tmp <- subset(results, variable == metric)
        title.tmp <- paste("case: ", recording$properties$casename, sep = "")
    } else {
        blockid.tmp <- blockid
        res.tmp <- subset(results, variable == metric & blockid == blockid.tmp)
        task <- as.character(subset(recording$conf$blocks, blockid == 1, select = tasktype, drop = TRUE))
        title.tmp <- paste("case: ", recording$properties$casename, "     task = ", task, sep = "")
    }

    ## Plot the data
    plot(res.tmp$timestamp, res.tmp$value,
         type = "l",
         col = "black",
         main = title.tmp,
         ylab = metric,
         xlab = "time")

    points(res.tmp$timestamp, res.tmp$value,
           col = "black",
           bg = "white",
           pch = 21,
           cex = 0.3)

    grid(5, lty = 2, col = "blue")

    if (! is.null(filename))
        dev.off()

}


#' Shade the area under the power spectrum corresponding to a frequency band.
#'
#' @param spec A spect
#' @param band A two-element vector with the upper and lower
#' frequencies of the frequency band to shade.
#' @param col The colour to use for shading. Default is \code{red}.
#'
#' @return Nothing. Shades a frequency band.
#' 
#' @family visualizations
#' 
#' @keywords internal
shade_frequency_band <- function(spec, band, col = "red") {

    f1  <- band[1]
    f2  <- band[2]

    ind <- which((spec$f >= f1) & (spec$f <= f2))

    sx  <- spec$f[ind]
    sy  <- spec$Px[ind]

    polygon(c(sx,rev(sx)), c(rep(0,length(sx)), rev(sy)), col = col)
}


#' Plot the HRV frequency spectrum.
#'
#' @param recording A recording structure. This structure must also contain the settings structure.
#'
#' @return Nothing. Produces a plot.
#' 
#' @family visualizations
#' 
#' @export
plot_spectrum <- function(recording, signal = "ibi") {

    settings <- recording$conf$settings

    t.sig <- recording$signal[[signal]]$t
    sig  <-  recording$signal[[signal]]$data

    ## Calculate the spectrum
    ## fmin    <- settings$frequencydomain$parameters$f.limits[1]
    ## fmax    <- settings$frequencydomain$parameters$f.limits[2]
    ## f       <- seq(from = fmin, to = fmax, length.out = 1000)

    spec    <- lombscargle(sig, t.sig,
                           normalization = settings$frequencydomain$parameters$normalization,
                           smooth        =  settings$frequencydomain$parameters$smooth,
                           smooth.kernel = settings$frequencydomain$parameters$kernel,
                           smooth.degree = settings$frequencydomain$parameters$smooth.degree)


    ## plot the spectrum
    xmin <- settings$frequencydomain$parameters$f.limits[1]
    xmax <- settings$frequencydomain$parameters$f.limits[2]
    plot(spec$f, spec$Px, type = "l", col = "black", lwd = 2, xlim = c(xmin, xmax), xlab = "Frequency [Hz]", ylab = "PSD")
    
    ## shade an area
    shade_frequency_band(spec, settings$frequencydomain$parameters$band.vlf, col = "red")
    shade_frequency_band(spec, settings$frequencydomain$parameters$band.lf, col = "blue")
    shade_frequency_band(spec, settings$frequencydomain$parameters$band.hf, col = "green")
    

}
