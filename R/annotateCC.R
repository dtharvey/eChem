#' Annotate Chronocoulogram
#'
#' Plots a chronocoulogam and annotates it with either the charge
#' for a single pulse experiment, or, for a double pulse
#' experiment, with the charge following the forward and the
#' reverse pulse, and the charge ratio. The charges are displayed
#' for a designated time after a pulse, which defaults to the
#' length of the pulse if a value is not provided.
#'
#' @param filename Name of the file that contains the results of a simulated chronocoulometry experiment.
#' @param time.delay Time after the application of a pulse for which the current is reported.
#' @param scale.factor Setting to a value less than 1 adjusts the \emph{y}-axis limits so that the limits are not defined by the current spike.
#' @param main_title An optional main title.
#'
#' @return Returns a plot of the chronocoulogram with annotations.
#'
#' @importFrom graphics plot grid abline arrows text
#'
#' @export
#'
#' @examples
#'
#' ex_ca = simulateCA(e.start = 0.25, e.pulse = -0.25, e.form = 0,
#'   pulses = "double", t.2 = 20, x.units = 100, t.units = 1000)
#' ex_cc = simulateCC(ex_ca)
#' annotateCC(ex_cc)

annotateCC = function(filename, time.delay,
                      scale.factor = 1, main_title = NULL) {

# verify that the file is for a chronocoulometry simulation

  if (filename$expt != "CC") {
    stop("This file is not from a chronocoulometry simulation.")
  }

# set time.delay if not provided

  if (missing(time.delay)) {
    if (filename$pulses == "double"){
      time.delay = filename$time_pulse2 - filename$time_pulse1
    }
    else {
      time.delay = filename$time_end - filename$time_pulse1
    }
  }

# plot the chronoamperogram, add baseline, and annotate plot

  plot(filename$time, filename$charge, type = "l",
       xlab = "time (s)", ylab = expression(paste("charge (", mu, "C)")),
       ylim = c(min(filename$charge)/scale.factor,
                max(filename$charge)/scale.factor),
       main = main_title)
  grid()

  delta.y = max(filename$charge)/scale.factor - min(filename$charge)/scale.factor

  abline(h = 0, lty = 2)
  if (filename$pulses == "single") {
    index = filename$tunits * (filename$time_pulse1 + time.delay)/filename$time_end
    arrows(x0 = filename$time[index], y0 = 0,
           x1 = filename$time[index], y1 = filename$charge[index],
           code = 3, length = 0.1, angle = 15)
    text(x = 0.1 * max(filename$time),
         y = max(filename$charge)/scale.factor - 0.10 * delta.y,
         substitute(paste("Q: ", q, " ", mu, "C"), list(q = noquote(formatC(filename$charge[index], format = "f", digits = 2)))),
         adj = c(0, NA), cex = 0.80)
  } else {
    index1 = filename$tunits * (filename$time_pulse1 + time.delay)/filename$time_end
    index2 = filename$tunits * (filename$time_pulse2 + time.delay)/filename$time_end
    arrows(x0 = filename$time[index1], y0 = 0,
           x1 = filename$time[index1], y1 = filename$charge[index1],
           code = 3, length = 0.1, angle = 15)
    arrows(x0 = filename$time[index2], y0 = 0,
           x1 = filename$time[index2], y1 = filename$charge[index2],
           code = 3, length = 0.1, angle = 15)
    text(x = 0.1 * max(filename$time),
         y = max(filename$charge)/scale.factor - 0.10 * delta.y,
         substitute(paste(Q[f], ": ", qfor, " ", mu, "C"), list(qfor = noquote(formatC(filename$charge[index1], format = "f", digits = 2)))),
         adj = c(0, NA), cex = 0.80)
    text(x = 0.1 * max(filename$time),
         y = max(filename$charge)/scale.factor - 0.175 * delta.y,
         substitute(paste(Q[r], ": ", qrev, "", mu, "C"), list(qrev = noquote(formatC(filename$charge[index2], format = "f", digits = 2)))),
         adj = c(0, NA), cex = 0.80)
    text(x = 0.1 * max(filename$time),
         y = max(filename$charge)/scale.factor - 0.25 * delta.y,
         substitute(paste(Q[r]/Q[f], ": ", ratio), list(ratio = noquote(formatC(abs(filename$charge[index2]/filename$charge[index1]), format = "f", digits = 3)))),
         adj = c(0, NA), cex = 0.80)
  }
}