#' Plot Diffusion Profiles
#'
#' Plots a set of diffusion profiles (concentration as a function
#' of distance from the electrode's surface) for an object
#' created using an object created with one of the package's
#' simulation functions: \code{cvSim} for cyclic voltammetry,
#' \code{lsvSim} for linear sweep voltammetry, \code{caSim} for
#' chronoamperometry, or \code{ccSim} for chronocoulometry. The
#' plot includes a default title that gives the time and the
#' potential for the diffusion profile. Note: this function will
#' not work with the reduced data file created using
#' \code{sampleAmpgram}, \code{sampleCoulgram}, or
#' \code{sampleVoltgram}.
#'
#' @param filename Name of the file that contains the results of a simulated electrochemistry experiment.
#' @param t The time for which the diffusion profile is desired.
#'
#' @return Returns a line plot that shows distance from the electrode's surface on the \emph{x}-axis and the concentrations of Ox, Red, and, for an EC or a CE mechanism, Z on the \emph{y}-axis.
#'
#' @importFrom graphics plot lines legend grid
#'
#' @export
#'
#' @examples
#'
#' ex_cv = simulateCV(e.start = 0.25, e.switch = -0.25, e.form = 0,
#'   x.units = 100, t.units = 1000)
#' plotDiffusion(ex_cv, t = 0.5)
#'
#' ex_ca = simulateCA(e.start = 0.25, e.pulse = -0.25, e.form = 0,
#'   pulses = "double", t.2 = 20, x.units = 100, t.units = 1000)
#' plotDiffusion(ex_ca, t = 21)

plotDiffusion = function(filename, t = 1) {

# check that file_type is full

  if(filename$file_type != "full"){
    stop("This function will not work with a reduced data set.")
  }

# check that the selected time falls within the limits of the
# simulation's actual times

  if (t < min(filename$time) | t > max(filename$time)) {
    stop(paste0("Time is limited to a value between ", min(filename$time), " s and ", max(filename$time), " s."))
  }

# identify index that corresponds to selected time

  index = which.min(abs(filename$time - t))

# set upper limit for y-axis

  ymax = filename$conc.bulk * 1000

# create plot

  if (filename$mechanism == "E") {
    plot(x = filename$distance, y = filename$oxdata[index, ],
         type = "l", lwd = 3,
         col = "blue", ylim = c(0, ymax),
         xlab = "distance from electrode (cm)",
         ylab = "concentration (mM)",
         main = paste0(round(filename$time[index], digits = 3),
                       " sec & ",
                       round(filename$potential[index],
                             digits = 3), " V"))
    lines(x = filename$distance, y = filename$reddata[index, ],
          lwd = 3, col = "red")
    legend(x = "right", legend = c("Ox", "Red"),
           fill = c("blue", "red"), border = "white",
           bty = "n", inset = 0.05)

    grid()
  } else {
    plot(x = filename$distance, y = filename$oxdata[index, ],
         type = "l", lwd = 3,
         col = "blue", ylim = c(0, ymax),
         xlab = "distance from electrode (cm)",
         ylab = "concentration (mM)",
         main = paste0(round(filename$time[index], digits = 3),
                       " sec & ",
                       round(filename$potential[index],
                             digits = 3), " V"))
    lines(x = filename$distance, y = filename$reddata[index, ],
          lwd = 3, col = "red")
    lines(x = filename$distance,
          y = filename$chemdata[index, ],
          lwd = 3, col = "green")
    legend(x = "right", legend = c("Ox", "Red", "Z"),
           fill = c("blue", "red", "green"),
           bty = "n", inset = 0.05)
    grid()
  }
}