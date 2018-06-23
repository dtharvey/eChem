#' Plot Applied Potential
#'
#' Plots the applied potential as a function of time for an
#' object created using an object created with one of the
#' package's simulation functions: \code{cvSim} for cyclic
#' voltammetry, \code{lsvSim} for linear sweep voltammetry,
#' \code{caSim} for chronoamperometry, or \code{ccSim} for
#' chronocoulometry. Note: this function will not work with the
#' reduced data file created using \code{sampleAmpgram},
#' \code{sampleCoulgram}, or \code{sampleVoltgram}.
#'
#' @param filename Name of the file that contains the results of a simulated electrochemistry experiment.
#' @param main_title An optional main title.
#'
#' @return Returns a line plot that shows time on the \emph{x}-axis and the applied potential on the \emph{y}-axis.
#'
#' @importFrom graphics plot grid
#'
#' @export
#'
#' @examples
#'
#' ex_cv = simulateCV(e.start = 0.25, e.switch = -0.25, e.form = 0,
#'   x.units = 100, t.units = 1000)
#' plotPotential(ex_cv,
#'   main_title = "Applied Potential for a Cyclic Voltammetry Simulation")
#'
#' ex_ca = simulateCA(e.start = 0.25, e.pulse = -0.25, e.form = 0,
#'   pulses = "double", t.2 = 20, x.units = 100, t.units = 1000)
#' plotPotential(ex_ca,
#'    main_title = "Applied Potential for a Chronoamperometry Simulation")

plotPotential = function(filename, main_title = NULL){

# check that file_type is full

  if(filename$file_type != "full"){
    stop("This function will not work with a reduced data set.")
  }

# create plot

  plot(x = filename$time, y = filename$potential, lwd = 2, col = "blue",
       type = "l", xlab = "time (sec)", ylab = "potential (V)",
       main = main_title)
  grid()
}