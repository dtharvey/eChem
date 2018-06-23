#' Simulate a Chronocoulometry Experiment
#'
#' Simulates either a single pulse or a double pulse
#' chroncoulometry experiment as either an E, EC, or CE
#' mechanism, where E is a redox reaction and where C is a
#' chemical reaction that either precedes or follows the redox
#' reaction. The function operates on an object created using
#' \code{caSim}, which simulates the corresponding
#' chronoamperometry experiment, integrating current over time
#' using the trapezoidal integration rule.
#'
#' @param filename The filename that contains the results of a chronampeometry simulation created using the \code{caSim} function.
#'
#' @return Returns a list with the following components \item{expt}{type of experiment; defaults to CC for a chronocoulometry simulation} \item{mechanism}{type of mechanism used for the simulation} \item{file_type}{value that indicates whether the output includes all data (full) or a subset of data (reduced); defaults to full for \code{ccSim}} \item{charge}{vector giving the charge as a function of time} \item{potential}{vector giving the potential as a function of time} \item{time}{vector giving the times used for the diffusion grids} \item{distance}{vector giving the distances from electrode surface used for the diffusion grids} \item{oxdata}{diffusion grid, as a matrix, giving the concentration of Ox} \item{reddata}{diffusion grid, as a matrix, giving the concentrations of Red} \item{chemdata}{diffusion grid, as a matrix, giving the concentrations of Z} \item{formalE}{formal potential for the redox reaction} \item{initialE}{initial potential} \item{pulseE}{potential after apply the initial pulse} \item{electrons}{number of electrons, n, in the redox reaction} \item{ko}{standard heterogeneous electron transfer rate constant} \item{kcf}{homogeneous first-order rate constant for forward chemical reaction} \item{kcr}{homogeneous first-order rate constant for reverse chemical reaction} \item{alpha}{transfer coefficient} \item{diffcoef}{diffusion coefficient for Ox and Red} \item{area}{surface area for electrode} \item{temperature}{temperature} \item{conc.bulk}{initial concentration of Ox or Red for an E or EC mechanism, or the combined initial concentrations of Ox and Z, or of Red and Z for a CE mechanism} \item{tunits}{the number of increments in time for the diffusion grids} \item{xunits}{the number of increments in distance for the diffusion grids} \item{sdnoise}{standard deviation, as percent of maximum current, used to add noise to simulated data} \item{direction}{-1 for an initial reduction reaction of Ox to Red; +1 for an initial oxidation reaction of Red to Ox} \item{pulses}{number of pulses: either single or double} \item{time_pulse1}{time when first pulse is applied} \item{time_pulse2}{time when second pulse is applied} \item{time_end}{time when experiment ends} \item{k_f}{vector of forward electron transfer rate constant as a function of potential} \item{k_b}{vector of reverse electron transfer rate constant as a function of potential} \item{jox}{vector giving the flux of Ox to the electrode surface as a function of potential} \item{jred}{vector giving the flux of Red to the electrode surface as a function of potential}
#'
#' @export
#'
#' @examples
#' ex_ca = simulateCA(e.start = 0.25, e.pulse = -0.25, e.form = 0,
#'   pulses = "double", t.2 = 20, x.units = 100, t.units = 1000)
#' ex_cc = simulateCC(ex_ca)
#' str(ex_cc)

simulateCC = function(filename){

# check that file being used is for a chronoamperometry experiment

  if (filename$expt != "CA") {
    stop("This file is not from a chronoamperometry simulation.")
  }

# create vector to hold charge

  charge = rep(0, length(filename$current))

# calculate charge

  for (i in 2:length(charge)) {
    x = 2:i
    charge[i] = as.double((filename$time[x] - filename$time[x-1]) %*% (filename$current[x] + filename$current[x - 1])/2)
  }

  output = list("expt" = "CC",
                "mechanism" = filename$mechanism,
                "file_type" = filename$file_type,
                "charge" = charge,
                "potential" = filename$potential,
                "time" = filename$time,
                "distance" = filename$distance,
                "oxdata" = filename$oxdata,
                "reddata" = filename$reddata,
                "chemdata" = filename$chemdata,
                "formalE" = filename$formalE,
                "initialE" = filename$initialE,
                "pulseE" = filename$pulseE,
                "electrons" = filename$electrons,
                "ko" = filename$ko,
                "kcf" = filename$kcf,
                "kcr" = filename$kcr,
                "alpha" = filename$alpha,
                "diffcoef" = filename$diffcoef,
                "area" = filename$area,
                "temperature" = filename$temperature,
                "conc.bulk" = filename$conc.bulk,
                "tunits" = filename$tunits,
                "xunits" = filename$xunits,
                "sdnoise" = filename$sdnoise,
                "direction" = filename$direction,
                "pulses" = filename$pulses,
                "time_pulse1" = filename$time_pulse1,
                "time_pulse2" = filename$time_pulse2,
                "time_end" = filename$time_end,
                "k_f" = filename$kf,
                "k_b" = filename$kb,
                "jox" = filename$jox,
                "jred" = filename$jred
  )

  invisible(output)

}