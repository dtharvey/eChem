#' Grid of Diffusion Profiles
#'
#' Plots eight diffusion profiles---at times that are 10%, 20%,
#' 30%, 40%, 60%, 70%, 80%, and 90% of the total time for the
#' experiment---around a central plot that shows the
#' corresponding voltammogram, chronoamperogram, or
#' chronocoulogram using an object created with one of the
#' package's simulation functions: \code{cvSim} for cyclic
#' voltammetry, \code{lsvSim} for linear sweep voltammetry,
#' \code{caSim} for chronoamperometry, or \code{ccSim} for
#' chronocoulometry. Note: this function will not work with the
#' reduced data file created using \code{sampleAmpgram},
#' \code{sampleCoulgram}, or \code{sampleVoltgram}.
#'
#' @param filename Name of the file that contains the results of a simulated electrochemistry experiment.
#'
#' @return Returns a 3 by 3 grid of individual plots.
#'
#' @importFrom graphics par plot points text grid
#'
#' @export
#'
#' @examples
#'
#' ex_cv = simulateCV(e.start = 0.25, e.switch = -0.25, e.form = 0,
#'   x.units = 100, t.units = 1000)
#' plotGrid(ex_cv)
#'
#' ex_ca = simulateCA(e.start = 0.25, e.pulse = -0.25, e.form = 0,
#'   pulses = "double", t.2 = 20, x.units = 100, t.units = 1000)
#' plotGrid(ex_ca)

plotGrid = function(filename) {

# check that file_type is full

  if(filename$file_type != "full"){
    stop("This function will not work with a reduced data set.")
  }

# adjust the graphical parameters to create a 3 by 3 grid of
# graphical windows and to adjust the margins for each graphical
# window

  par(mfrow = c(3,3), mar = c(4.1, 4.1, 2.1, 2.1))

# set the times for the diffusion profiles

  t = round(c(0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9) * (filename$tunits) + 1, digits = 0)

# screen(1): diffusion profile at 20% of scan time

  plotDiffusion(filename, t = filename$time[t[2]])

# screen(2): diffusion profile at 30% of scan time

  plotDiffusion(filename, t = filename$time[t[3]])

# screen(3): diffusion profile at 40% of scan time

  plotDiffusion(filename, t = filename$time[t[4]])

# screen(4): diffusion profile at 10% of scan time

  plotDiffusion(filename, t = filename$time[t[1]])

# screen(5): voltammogram, chronoamperogram, or chronocoulogram

  if (filename$expt == "CV" | filename$expt == "LSV") {
    plot(x = filename$potential, y = filename$current,
         lwd = 2, col = "blue", type = "l",
         xlab = "potential (V)", ylab = expression(paste("current (", mu, "A)")),
         xlim = c(max(filename$potential), min(filename$potential)),
         ylim = c(1.1 * min(filename$current), 1.1 * max(filename$current)),
         main = "points show times")
    points(x = filename$potential[t + 1],
           y = filename$current[t + 1], pch = 19,
           col = "blue", cex = 1.5)
    for (i in 1:8) {
      if(t[i] < length(filename$time)/2){
        text(x = filename$potential[t[i] + 1],
             y = filename$current[t[i] + 1],
             labels = as.character(filename$time[t[i]]),
             pos = 3, cex = 0.75)
      } else {
        text(x = filename$potential[t[i] + 1],
             y = filename$current[t[i] + 1],
             labels = as.character(filename$time[t[i]]),
             pos = 1, cex = 0.75)
      }
    }
    grid()
  } else if (filename$expt == "CA") {
    plot(x = filename$time, y = filename$current,
         lwd = 2, col = "blue", type = "l",
         xlab = "timel (s)", ylab = expression(paste("current (", mu, "A)")),
         main = "points show times")
    points(x = filename$time[t + 1],
           y = filename$current[t + 1],
           pch = 19, col = "blue", cex = 1.5)
    for (i in 1:8) {
      text(x = filename$time[t[i] + 1],
           y = filename$current[t[i] + 1],
           labels = as.character(filename$time[t[i]]),
           pos = 3, cex = 0.75)
    }
    grid()
  } else {
    plot(x = filename$time, y = filename$charge,
         lwd = 2, col = "blue", type = "l",
         xlab = "timel (s)", ylab = expression(paste("charge (", mu, "C)")),
         main = "points show times")
    points(x = filename$time[t + 1],
           y = filename$charge[t + 1],
           pch = 19, col = "blue", cex = 1.5)
    for (i in 1:8) {
      text(x = filename$time[t[i] + 1],
           y = filename$charge[t[i] + 1],
           labels = as.character(filename$time[t[i]]),
           pos = 3, cex = 0.75)
    }
    grid()
  }

# screen(6): diffusion profiles at 60% of scan time

  plotDiffusion(filename, t = filename$time[t[5]])

# screen(7): diffusion profiles at 90% of scan time

  plotDiffusion(filename, t = filename$time[t[8]])

# screen(8): diffusion profiles at 80% of scan time

  plotDiffusion(filename, t = filename$time[t[7]])

# screen(9): diffusion profiles at 70% of scan time

  plotDiffusion(filename, t = filename$time[t[6]])

# reestablish original graphical parameters

  par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))

}