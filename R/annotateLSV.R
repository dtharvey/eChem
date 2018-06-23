#' Annotate Linear Sweep Voltammogram
#'
#' Plots a linear sweep voltammogram and annotates it with values
#' for either the cathodic peak potential (Epc) and cathodic peak
#' current (ip,c), or the anodic peak potential (Epa) and the
#' anodic peak current (ip,a). The baseline for deteriming the
#' peak currents is set using a defined percentage of points at
#' the beginning of potential scan.
#'
#' @param filename Name of the file that contains the results of a simulated linear sweep voltammetry experiment.
#' @param potential.per The percentage of points from the beginning of the potential scan used to set the baseline for measuring the peak current.
#' @param main_title An optional main title.
#'
#' @return Returns a plot of the linear sweep voltammogram with annotations.
#'
#' @importFrom graphics plot grid abline arrows text
#'
#' @export
#'
#' @examples
#' ex_lsv = simulateLSV(e.start = 0.25, e.end = -0.25, e.form = 0,
#'   stir.rate = "fast", x.units = 100, t.units = 1000)
#' annotateLSV(ex_lsv)

annotateLSV = function(filename, potential.per = 5,
                       main_title = NULL) {

# verify that the file was created using lsvSim

  if (filename$expt != "LSV") {
    stop("This file is not from a linear sweep voltammetry simulation.")
  }

# plot the linear sweep voltammogram

  plot(x = filename$potential, y = filename$current,
       type = "l", main = main_title,
       xlab = "potential (V)", ylab = expression(paste("current (", mu, "A)")),
       xlim = c(max(filename$potential), min(filename$potential)))
  grid()

# identify the peak potential

  if (filename$direction == -1) {
    epc.id = which.max(filename$current)
    epc = filename$potential[epc.id]
  } else {
    epa.id = which.min(filename$current)
    epa = filename$potential[epa.id]
  }

# calculate the baseline and add baseline to plot

  n.points = (potential.per/100) * filename$tunits
  lm = lm(filename$current[1:n.points] ~ filename$potential[1:n.points])
  abline(lm, lwd = 1, lty = 2, col = "black")

# add arrow showing the peak potential and the peak current

  if (filename$direction == -1) {
    arrows(x0 = epc, y0 = lm$coefficients[1] + lm$coefficients[2] * filename$potential[epc.id], x1 = epc, y1 = filename$current[epc.id], code = 3, length = 0.1, angle = 15)
  } else {
    arrows(x0 = epa, y0 = lm$coefficients[1] + lm$coefficients[2] * filename$potential[epa.id], x1 = epa, y1 = filename$current[epa.id], code = 3, length = 0.1, angle = 15)
  }

# calcualte the peak current

  if (filename$direction == -1) {
    ipc = filename$current[epc.id] - (lm$coefficients[1] + lm$coefficients[2] * filename$potential[epc.id])
  } else {
    ipa = -(filename$current[epa.id] - (lm$coefficients[1] + lm$coefficients[2] * filename$potential[epa.id]))
  }

# annotate the plot

  delta.y = max(filename$current) - min(filename$current)
  if (filename$direction == -1) {
    if(filename$stir_rate == "off"){
    text(x = max(filename$potential),
         y = max(filename$current) - 0.05 * delta.y,
         substitute(paste(E[pc], ": ", epc, " V"), list(epc = noquote(formatC(epc, format = "f", digits = 3)))),
         adj = c(0, NA), cex = 0.80)}
    text(x = max(filename$potential),
         y = max(filename$current) - 0.1 * delta.y,
         substitute(paste(i[pc], ": ", ipc, " ", mu, "A"), list(ipc = noquote(formatC(ipc, format = "f", digits = 2)))),
         adj = c(0, NA), cex = 0.80)
  } else {
    if(filename$stir_rate == "off"){
    text(x = max(filename$potential),
         y = max(filename$current) - 0.05 * delta.y,
         substitute(paste(E[pa], ": ", epa, " V"), list(epa = noquote(formatC(epa, format = "f", digits = 3)))),
         adj = c(0, NA), cex = 0.80)}
    text(x = max(filename$potential),
         y = max(filename$current) - 0.1 * delta.y,
         substitute(paste(i[pa], ": ", ipa, " ", mu, "A"), list(ipa = noquote(formatC(ipa, format = "f", digits = 2)))),
         adj = c(0, NA), cex = 0.80)
  }
}