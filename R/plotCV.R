#' Plot Cyclic Voltammograms
#'
#' Plots 1--5 cyclic voltammograms on a single set of axes. The
#' default plot does not include a legend or a title, but
#' providing a vector of character strings to legend_text adds a
#' legend to the final plot, and adding a character string for
#' main_title adds a title to the plot. Line widths, line types,
#' line colors, pointsymbols, and point colors have default
#' values that can be adjusted. Note: this function accepts both
#' full data files created using \code{cvSim} or reduced data
#' files created \code{sampleCV}.
#'
#' @param filenames A list giving the names of 1--5 files that contain the results of a simulated cyclic voltammetry experiment.
#' @param legend_text Optional vector that contains text to include in a legend. Default is NULL, which supresses the legend.
#' @param legend_position One of \code{topleft}, \code{topright}, \code{bottomleft}, or \code{bottomright}; defaults to \code{topleft}.
#' @param main_title An optional main title.
#' @param line_widths A vector of line widths for the individual cyclic voltammograms; defaults to a common line width of 2, but can be adjusted by supplying a vector with desired values.
#' @param line_types A vector of line types for the individual cyclic voltammograms; defaults to a set of different line types, but can be adjusted by supplying a vector with desired values.
#' @param line_colors A vector of colors for the individual cyclic voltammograms, whether displayed as lines or as points; defaults to a common color, but can be adjusted by supplying a vector with desired values.
#' @param point_symbols A vector of pch values for plotting points.
#'
#' @importFrom graphics plot grid lines points legend
#'
#' @export
#'
#' @return Returns a plot that shows the applied potential on the \emph{x}-axis and current on the \emph{y}-axis.
#'
#
#' @examples
#'
#' cv_ex1 = simulateCV(ko = 1, e.switch = -0.8, e.form = -0.4,
#'    x.units = 100, t.units = 1000)
#' cv_ex2 = simulateCV(ko = 0.01, e.switch = -0.8, e.form = -0.4,
#'   x.units = 100, t.units = 1000)
#' cv_ex3 = simulateCV(ko = 0.001, e.switch = -0.8, e.form = -0.4,
#'   x.units = 100, t.units = 1000)
#' cv_ex4 = simulateCV(ko = 0.0001, e.switch = -0.8, e.form = -0.4,
#'   x.units = 100, t.units = 1000)
#' plotCV(filenames = list(cv_ex1, cv_ex2, cv_ex3, cv_ex4),
#'   legend_text = c("ko = 1 cm/s", "ko = 0.01 cm/s",
#'   "ko = 0.001 cm/s", "ko = 0.0001 cm/s"))
#'
#' cv_ex1sample = sampleCV(cv_ex1, data.reduction = 5)
#' plotCV(filenames = list(cv_ex1, cv_ex1sample),
#'   line_colors = c("blue", "red"))

plotCV = function(filenames = list(file1 = NULL, file2 = NULL),
                        legend_text = NULL,
                        legend_position = c("topleft", "topright",
                                            "bottomleft", "bottomright"),
                        main_title = NULL,
                        line_widths = c(2, 2, 2, 2, 2),
                        line_types = c(1, 2, 3, 4, 5),
                        point_symbols = c(21, 22, 23, 24, 25),
                        line_colors = c("blue", "blue", "blue",
                                        "blue", "blue")) {

# determine the number of voltammograms and verify that the
# number does not exceed the defined limit of five

  numfiles = length(filenames)
  if (numfiles > 5){
    stop("Function is limited to plotting five voltammograms")
  }

# verify that files are for a CV

  for (i in 1:numfiles){
    if(filenames[[i]]$expt != "CV"){
      stop("All files must be for a cyclic voltammetry simulation.")
    }
  }

# determine the position of the legend

  legend_position = match.arg(legend_position)

# determine the minimum and maximum units for the x-axis and the
# y-axis by examining the data for all voltammograms

  xmin = rep(0, numfiles)
  xmax = rep(0, numfiles)
  ymin = rep(0, numfiles)
  ymax = rep(0, numfiles)

  for (i in 1:numfiles) {
    xmin[i] = min(filenames[[i]]$potential)
    xmax[i] = max(filenames[[i]]$potential)
    ymin[i] = min(filenames[[i]]$current)
    ymax[i] = max(filenames[[i]]$current)
  }

  xmin_id = which.min(xmin)
  xmax_id = which.max(xmax)
  ymin_id = which.min(ymin)
  ymax_id = which.max(ymax)

# create the plot using lines for full data and points for
# reduced data; point colors are defined using line_color

  if (filenames[[1]]$file_type == "full") {
    plot(x = filenames[[1]]$potential,
         y = filenames[[1]]$current,
         xlim = c(xmax[xmax_id], xmin[xmin_id]),
         ylim = c(ymin[ymin_id], ymax[ymax_id]),
         type = "l", lwd = line_widths[1] , lty = line_types[1],
         col = line_colors[1],
         main = main_title,
         xlab = "potential (V)", ylab = expression(paste("current (", mu, "A)"))
    )
  } else {
    plot(x = filenames[[1]]$potential,
         y = filenames[[1]]$current,
         xlim = c(xmax[xmax_id], xmin[xmin_id]),
         ylim = c(ymin[ymin_id], ymax[ymax_id]),
         type = "p", lwd = line_widths[1],
         pch = point_symbols[1], col = line_colors[1],
         bg = line_colors[1],
         xlab = "potential (V)", ylab = expression(paste("current (", mu, "A)"))
    )
  }

  grid()

  if (numfiles > 1) {
    for (i in 2:numfiles) {
      if (filenames[[i]]$file_type == "full") {
        lines(x = filenames[[i]]$potential,
              y = filenames[[i]]$current,
              lty = line_types[i], lwd = line_widths[i],
              col = line_colors[i])
      } else {
        points(x = filenames[[i]]$potential,
               y = filenames[[i]]$current,
               pch = point_symbols[i], col = line_colors[i],
               bg = line_colors[i])
      }
    }
  }
  if (is.null(legend_text) == FALSE) {
    legend(x = legend_position, legend = legend_text, lwd = line_widths,
           lty = line_types, col = line_colors,bty = "n")
  }
}