#' Annotate Cyclic Voltammogram
#'
#' Plots a cyclic voltammogram and annotates it with values for the cathodic peak potential (Epc), the anodic peak potential (Epa), the difference in peak potentials (Delta E), the cathodic peak current (ip,c), the anodic peak current (ip,a), and the peak current ratio (either ip,c/ip,a or ip,a/ip,c). The baseline for deteriming peak currents is set using a defined percentage of points at the beginning of the forward and the reverse scans. Values are reported as unmeasurable when they fall below a threshold value.
#'
#' @param filename Name of the file that contains the results of a simulated cyclic voltammetry experiment.
#' @param forward.per The percentage of points from the beginning of the forward scan used to set the baseline for measuring the peak current.
#' @param reverse.per The percentage of points from the beginning of the reverse scan used to set the baseline for measuring the peak current.
#' @param main_title An optional main title.
#' @param threshold Sets the smallest measurable current.
#'
#' @return Returns a plot of the cyclic voltammogram with annotations.
#'
#' @importFrom graphics plot grid abline arrows lines text
#'
#' @importFrom stats lm
#'
#' @export
#'
#' @examples
#'
#' cv_ex2 = simulateCV(ko = 0.01, e.switch = -0.8, e.form = -0.4,
#'   x.units = 100, t.units = 1000)
#' annotateCV(cv_ex2)

annotateCV = function(filename, main_title = NULL,
                      forward.per = 5, reverse.per = 5,
                      threshold = 0.05) {

# verify that the file was created using cvSim

  if (filename$expt != "CV") {
    stop("This file is not from a cyclic voltammetry simulation.")
  }

# plot the cyclic voltammogram

  plot(x = filename$potential,
       y = filename$current, type = "l", main = main_title,
       xlab = "potential (V)",
       ylab = expression(paste("current (", mu, "A)")),
       xlim = c(max(filename$potential), min(filename$potential)))
  grid()

# identify the cathodic (Epc) and the anodic (Epa) peak potentials

  epc.id = which.max(filename$current)
  epa.id = which.min(filename$current)
  epc = filename$potential[epc.id]
  epa = filename$potential[epa.id]

# create linear models to predict baseline currents using the
# parameters forward.per and reverse.per; lm1 models the baseline
# for the foward reaction and uses the first forward.per% of the
# data points to model the baseline and a first-order
# relationship between current and potential; lm2 models the
# baseline for the reverse reaction and uses the first
# reverse.per% of the data points beginning with the switching
# potential to predict the current decay in the absence of a
# change in potential using a model in which the current decays
# as function of t^(-0.5)

  n.points1 = (forward.per/100) * filename$tunits
  n.points2 = (reverse.per/100) * filename$tunits
  lm1 = lm(filename$current[1:n.points1] ~ filename$potential[1:n.points1])
  lm2 = lm(filename$current[(filename$tunits/2+1):(filename$tunits/2+n.points2 + 1)] ~ I(filename$time[(filename$tunits/2 + 1):(filename$tunits/2+n.points2 + 1)]^(-0.5)))

# add a dashed line to show the baseline for the forward reaction
# and add an arrow to show the peak current and peak potential

  abline(lm1, lwd = 1, lty = 2, col = "black")
  if (filename$direction == -1) {
    arrows(x0 = epc, y0 = lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epc.id],
           x1 = epc, y1 = filename$current[epc.id],
           code = 3, length = 0.1, angle = 15)
  } else {
    arrows(x0 = epa, y0 = lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epc.id],
           x1 = epa, y1 = filename$current[epa.id],
           code = 3, length = 0.1, angle = 15)
  }

# add a dashed line to show the baseline for the reverse
# reaction; an arrow to identify the peak potential and the peak
# current is not drawn at this time, but is added later if it is
# applicable

  y = lm2$coefficients[1] + lm2$coefficients[2] * filename$time[(filename$tunits/2 + 1):(filename$tunits+1)]^(-0.5)
  lines(filename$potential[(filename$tunits/2+1):(filename$tunits+1)], y,
        lwd = 1, lty = 2, col = "black")

# set flags to control the plotting of annotations; flags for the
# cathodic and the anodic potentials determine whether these
# potentials are included; flags for the cathodic and the anodic
# current determine whether these currents are included; both
# sets of flags determine whether an arrow is drawn for the
# reverse scan

  flag.cpot = FALSE
  flag.ccur = FALSE
  flag.apot = FALSE
  flag.acur = FALSE

  if (abs(filename$current[epc.id]) < threshold) {
    flag.cpot = TRUE
    flag.ccur = TRUE
  }
  if (abs(filename$current[epa.id]) < threshold) {
    flag.apot = TRUE
    flag.acur = TRUE
  }

  if (filename$direction == -1 & flag.apot == FALSE) {
    if (filename$current[epa.id] - (y[epa.id - filename$tunits/2 + 1]) > 0) {
      flag.acur = TRUE
    }
  }
  if (filename$direction == 1 & flag.cpot == FALSE) {
    if (filename$current[epc.id] - (y[epc.id - filename$tunits/2 + 1]) < 0) {
      flag.ccur = TRUE
    }
  }

  if (filename$direction == -1) {
    if (flag.acur == FALSE & flag.apot == FALSE) {
      arrows(x0 = epa, y0 = y[epa.id - filename$tunits/2 + 1],
             x1 = epa, y1 = filename$current[epa.id],
             code = 3, length = 0.1, angle = 15)
    }
  } else {
    if (flag.ccur == FALSE & flag.cpot == FALSE) {
      arrows(x0 = epc, y0 = y[epc.id - filename$tunits/2 + 1],
             x1 = epc, y1 = filename$current[epc.id],
             code = 3, length = 0.1, angle = 15)
    }
  }

# calculate the cathodic peak current, ipc, and the anodic peak
# current, ipa

  if (filename$direction == -1) {
    ipc = filename$current[epc.id] - (lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epc.id])
    ipa = filename$current[epa.id] - y[epa.id - filename$tunits/2+1]
  } else {
    ipa = (filename$current[epa.id] - (lm1$coefficients[1] + lm1$coefficients[2] * filename$potential[epa.id]))
    ipc = (filename$current[epc.id] - y[epc.id - filename$tunits/2+1])
  }

# add annotations to plot the peak potentials, the peak currents,
# delta E and the peak current ratio; values are not shown for
# the reverse reaction if threshold values are not met

  delta.y = max(filename$current) - min(filename$current)

#  add annotations for the cathodic peak potential and the
#  cathodic peak current

  if (flag.cpot == FALSE) {
    text(x = max(filename$potential),
         y = max(filename$current),
         substitute(paste(E[pc], ": ", epc, " V"), list(epc = noquote(formatC(epc, format = "f", digits = 3)))),
         adj = c(0, NA), cex = 0.80)
  } else {
    text(x = max(filename$potential),
         y = max(filename$current),
         substitute(paste(E[pc], ": not measurable")),
         adj = c(0, NA), cex = 0.80)
  }

  if (flag.ccur == FALSE) {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.80 * delta.y,
         substitute(paste(i[pc], ": ", ipc, " ", mu, "A"), list(ipc = noquote(formatC(ipc, format = "f", digits = 2)))),
         adj = c(1, NA), cex = 0.80)
  } else {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.80 * delta.y,
         substitute(paste(i[pc], ": not measurable")),
         adj = c(1, NA), cex = 0.80)
  }

# add annotations for the anodic peak potential and the anodic
# peak current

  if (flag.apot == FALSE) {
    text(x = max(filename$potential),
         y = max(filename$current) - 0.05 * delta.y,
         substitute(paste(E[pa], ": ", epa, " V"), list(epa = noquote(formatC(epa, format = "f", digits = 3)))),
         adj = c(0, NA), cex = 0.80)
  } else {
    text(x = max(filename$potential),
         y = max(filename$current) - 0.05 * delta.y,
         substitute(paste(E[pa], ": not measurable")),
         adj = c(0, NA), cex = 0.80)
  }

  if(flag.acur == FALSE) {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.85 * delta.y,
         substitute(paste(i[pa], ": ", ipa, " ", mu, "A"), list(ipa = noquote(formatC(ipa, format = "f", digits = 2)))),
         adj = c(1, NA), cex = 0.80)
  } else {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.85 * delta.y,
         substitute(paste(i[pa], ": not measurable")),
         adj = c(1, NA), cex = 0.80)
  }

# add annotations for delta E, Eavg, and the peak current ratio

  if (flag.cpot == FALSE & flag.apot == FALSE) {
    text(x = max(filename$potential),
         y = max(filename$current) - 0.1 * delta.y,
         substitute(paste(Delta, "E: ", deltae, " V"),
                    list(deltae = noquote(formatC(epa - epc, format = "f", digits = 3)))),
         adj = c(0, NA), cex = 0.80)
    text(x = max(filename$potential),
         y = max(filename$current) - 0.15 * delta.y,
         substitute(paste(E[avg], ": ", eavg, " V"),
                    list(eavg = noquote(formatC(0.5 * (epa + epc), format = "f", digits = 3)))),
         adj = c(0, NA), cex = 0.80)
  } else {
    text(x = max(filename$potential),
         y = max(filename$current) - 0.1 * delta.y,
         substitute(paste(Delta, "E: not measurable")),
         adj = c(0, NA), cex = 0.80)
    text(x = max(filename$potential),
         y = max(filename$current) - 0.15 * delta.y,
         substitute(paste(E[avg], ": not measurable")),
         adj = c(0, NA), cex = 0.80)
  }

  if (flag.ccur == FALSE & flag.acur == FALSE) {
    if (filename$direction == 1) {
      text(x = min(filename$potential),
           y = max(filename$current) - 0.90 * delta.y,
           substitute(paste("|", i[pc]/i[pa], "|: ", ratio), list(ratio = noquote(formatC(abs(ipc/ipa), format = "f", digits = 2)))),
           adj = c(1, NA), cex = 0.80)
    } else {
      text(x = min(filename$potential),
           y = max(filename$current) - 0.90 * delta.y,
           substitute(paste("|", i[pa]/i[pc], "|: ", ratio), list(ratio = noquote(formatC(abs(ipa/ipc), format = "f", digits = 2)))),
           adj = c(1, NA), cex = 0.80)
    }
  } else {
    text(x = min(filename$potential),
         y = max(filename$current) - 0.90 * delta.y,
         substitute(paste("|", i[pc]/i[pa], "|: not measurable")),
         adj = c(1, NA), cex = 0.80)
  }

}