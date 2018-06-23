#' Simulate a Linear Sweep Voltammetry Experiment
#'
#' Simulates a linear sweep voltammetry experiment as either an
#' E, EC, or CE mechanism, where E is a redox reaction and where
#' C is a chemical reaction that either precedes or follows the
#' redox reaction.
#'
#' @param e.start Initial potential (in volts).
#' @param e.end Final potential (in volts).
#' @param e.form Formal potential for the redox reaction (in volts).
#' @param mechanism Mechanism for the electrochemical system; one of \code{E} for redox reaction only, \code{EC} for redox reaction with a following chemical reaction, or \code{CE} for redox reaction with a preceeding chemical reaction. Default is \code{E}.
#' @param ko Standard heterogeneous electron transfer rate constant for the redox reaction (in cm/s).
#' @param kcf Homogeneous first-order rate constant for the forward chemical reaction (in s^-1).
#' @param kcr Homogeneous first-order rate constant for the reverse chemical reaction (in s^-1).
#' @param n Number of electrons in the redox reaction.
#' @param alpha Transfer coefficient.
#' @param d Diffusion coefficient for Ox and Red (in cm^2 s^-1).
#' @param area Surface area of the electrode (in cm^2).
#' @param temp Temperature (in K).
#' @param scan.rate Rate at which the potential is changed (in V/s).
#' @param conc.bulk Initial bulk concentration of Ox or Red for an E or an EC mechanism, or the combined initial concentrations of Ox and Z, or of Red and Z for a CE mechanism (in mol/L).
#' @param t.units The number of increments in time for the diffusion grids.
#' @param x.units The number of increments in distance for the diffusion grids.
#' @param sd.noise The standard deviation for noise as a percent of maximum current (in \eqn{\mu}A).
#' @param stir.rate The rate at which the solution is stirred with options for \code{off}, or no stirring, and for \code{slow}, \code{medium}, and \code{fast} stirring; defaults to \code{off}.
#'
#' @return Returns a list with the following components \item{expt}{type of experiment; defaults to LSV for a linear sweep voltammetry simulation} \item{mechanism}{type of mechanism used for the simulation} \item{file_type}{value that indicates whether the output includes all data (full) or a subset of data (reduced); defaults to full for \code{lsvSim}} \item{current}{vector giving the current as a function of time} \item{potential}{vector giving the potential as a function of time} \item{time}{vector giving the times used for the diffusion grids} \item{distance}{vector giving the distances from electrode surface used for the diffusion grids} \item{oxdata}{diffusion grid, as a matrix, giving the concentration of Ox} \item{reddata}{diffusion grid, as a matrix, giving the concentrations of Red} \item{chemdata}{diffusion grid, as a matrix, giving the concentrations of Z} \item{formalE}{formal potential for the redox reaction} \item{initialE}{initial potential} \item{endE}{end potential} \item{electrons}{number of electrons, n, in the redox reaction} \item{ko}{standard heterogeneous electron transfer rate constant} \item{kcf}{homogeneous first-order rate constant for forward chemical reaction} \item{kcr}{homogeneous first-order rate constant for reverse chemical reaction} \item{alpha}{transfer coefficient} \item{diffcoef}{diffusion coefficient for Ox and Red} \item{area}{surface area for electrode} \item{temperature}{temperature} \item{scanrate}{scan rate} \item{conc.bulk}{initial concentration of Ox or Red for an E or EC mechanism, or the combined initial concentrations of Ox and Z, or of Red and Z for a CE mechanism} \item{tunits}{the number of increments in time for the diffusion grids} \item{xunits}{the number of increments in distance for the diffusion grids} \item{sdnoise}{standard deviation, as percent of maximum current, used to add noise to simulated data} \item{direction}{-1 for an initial reduction reaction of Ox to Red; +1 for an initial oxidation reaction of Red to Ox} \item{stir_rate}{rate at whcih solution is stirred} \item{k_f}{vector of forward electron transfer rate constant as a function of potential} \item{k_b}{vector of reverse electron transfer rate constant as a function of potential} \item{jox}{vector giving the flux of Ox to the electrode surface as a function of potential} \item{jred}{vector giving the flux of Red to the electrode surface as a function of potential}
#'
#' @importFrom stats rnorm
#'
#' @export
#'
#' @examples
#' ex_lsv = simulateLSV(e.start = 0.25, e.end = -0.25, e.form = 0,
#'   stir.rate = "fast", x.units = 100, t.units = 1000)
#' str(ex_lsv)

simulateLSV = function(e.start = 0.0, e.end = -1, e.form = -0.25,
                  mechanism = c("E", "EC", "CE"),
                  ko = 1, kcf = 0, kcr = 0,
                  n = 1, alpha = 0.50, d = 1e-5, area = 0.01,
                  temp = 298.15, scan.rate = 1.0, conc.bulk = 1e-3,
                  t.units = 2000, x.units = 180, sd.noise = 0,
                  stir.rate = c("off", "slow", "medium", "fast")) {

# Test to ensure that t.units and x.units satisfy constraint that
# the number of distance units is less than (18 * number of time
# unit)^(0.5).

  if (x.units >= sqrt(18 * t.units)) {
    stop("x.units must be less than sqrt(18 * t.units)")
  }

# verify that e.form falls between e.start and e.end

  if(e.end > e.start){
    if(e.form < e.start | e.form > e.end){
      stop("e.form must be between e.start and e.end")
    }
  } else {if(e.end < e.start) {
    if(e.form > e.start | e.form < e.end){
      stop("e.form is not between e.start and e.end")
    }
  }
  }

# Determine the mechansim and test to ensure that homogeneous
# rate constants have acceptable values

  mechanism = match.arg(mechanism)

  if (mechanism == "E") {
    if (kcf != 0 | kcr != 0) {
      stop("For the E mechanism, kcf and kcr must have values of 0.")
    }
  }

  if (mechanism == "CE") {
    if (kcf <= 0) {
      stop("For the CE mechanism, kcf must have a value greater than 0.")
    }
  }

# translate stir.rate to a factor that limits the thickness of
# the diffusion layer when stirring is turned on; this is
# accomplished in the loops for calculating the diffusion grid by
# dividing x.units by the scaling.factor defined here

  stir.rate = match.arg(stir.rate)

  if (stir.rate == "off") {
    scaling.factor = 1
  } else if (stir.rate == "slow") {
    scaling.factor = 25
  } else if (stir.rate == "medium") {
    scaling.factor = 30
  } else if (stir.rate == "fast") {
    scaling.factor = 35
  } else {
    stop("Options for stir rate are off, slow, medium, or fast.")
  }

# physical constants used in simulations: Faraday's contant (F)
# in C/mol and the gas constant (R) in J/K•mol

  f = 96485
  r = 8.31451

# define the limits for the diffusion grid with respect to time
# and to distance, and calculate additional simulation
# parameters, including bulk concentrations of all species t.tot:
# time to complete one full sweep from e.start to e.start (s)
# delta.t: increment in time (s) time: vector of discrete times
# for diffusion grid (s) x.tot: max distance chosen to exceed
# difusion limit (cm) delta.x: increment in distance (cm)
# distance: vector of discrete distances for diffusion grid (cm)
# lambda: a gathering of constants (unitless) direction: -1 for
# initial reduction and +1 for initial oxidation cox.bulk: bulk
# concentration of Ox (mol/cm^3) cred.bulk: bulk concentration of
# Red (mol/cm^3)

  t.tot = abs((e.start - e.end))/scan.rate
  delta.t = t.tot/t.units
  time = seq(0, t.tot, delta.t)
  x.tot = 6 * sqrt(d * t.tot)
  delta.x = x.tot/x.units
  distance = seq(0, x.tot, delta.x)
  lambda = d * delta.t/(delta.x)^2
  if (e.start > e.end) {
    direction = -1
    if (mechanism == "CE") {
      cchem.bulk = conc.bulk/(1 + kcf/kcr)
      cox.bulk = conc.bulk - cchem.bulk
      cred.bulk = 0
    } else {
      cox.bulk = conc.bulk
      cred.bulk = 0
      cchem.bulk = 0
    }
  } else {
    direction = +1
    if (mechanism == "CE") {
      cchem.bulk = conc.bulk/(1 + kcf/kcr)
      cox.bulk = 0
      cred.bulk = conc.bulk - cchem.bulk
    } else {
      cox.bulk = 0
      cred.bulk = conc.bulk
      cchem.bulk = 0
    }
  }

# check to ensure that the number of time units satisfies
# Gosser's requirement for accuracy when using an EC or CE
# mechanism

  if (mechanism != "E") {
    min_tunits = 4 * t.tot * kcf
    if (t.units < min_tunits) {
      stop(paste("Minimum time units is", min_tunits, "if kcf =", kcf, "and with a total scan time of", t.tot, "s."))
    }
  }

# create vector of discrete applied potentials

  potential = seq(e.start, e.end, direction * scan.rate * delta.t)

# calculate the potential-dependent forward (kf) and reverse (kb)
# heterogeneous electron-transfer rate constants

  kf = ko * exp(-alpha * n * f * (potential - e.form)/(r*temp))
  kb = ko * exp((1 - alpha) * n * f * (potential - e.form)/(r*temp))

# initialize diffusion grids (rows = time; cols = distance) using
# bulk concentrations for Ox, Red, and Chem and adjusting
# concentrations to mol/cm^3; the actual concentrations are
# calculated later

  dif.ox = matrix(cox.bulk/1000, nrow = t.units + 1, ncol = x.units + 1)
  dif.red = matrix(cred.bulk/1000, nrow = t.units + 1, ncol = x.units + 1)
  dif.chem = matrix(cchem.bulk/1000, nrow = t.units + 1, ncol = x.units + 1)

# create vectors for fluxes and current, which are calculated
# later; the initial values here are not important as actual
# values are calculated later

  jox = rep(0, t.units + 1)
  jred = rep(0, t.units + 1)
  current.total = rep(0, t.units + 1)

# calculate diffusion grids over time and, at each time, over
# distance; for each time the diffusion grids is calculated at
# all distances except for at the electrode surface; next, for
# each time, the flux of each species to the electrode surface is
# used to calculate their concentrations at the electrode
# surface; and, finally, for each time, the current is calculated

  if (mechanism == "CE") {
    for (i in 2:(t.units + 1)){
      for (j in 2:(x.units/scaling.factor)) {
        if (direction == -1) {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1]) + kcf * delta.t * dif.chem[i-1, j] - kcr * delta.t * dif.ox[i-1, j]
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1])
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) - kcf * delta.t * dif.chem[i-1, j] + kcr * delta.t * dif.ox[i-1, j]
        } else {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1])
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1]) + kcf * delta.t*dif.chem[i-1, j] - kcr * delta.t * dif.red[i-1, j]
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) - kcf * delta.t * dif.chem[i-1, j] + kcr * delta.t * dif.red[i-1, j]
        }
      }
      jox[i] = -(kf[i] * dif.ox[i,2] - kb[i] * dif.red[i,2])/(1 + (kf[i] * delta.x)/d + (kb[i] * delta.x)/d)
      jred[i] = -jox[i]
      dif.ox[i, 1] = dif.ox[i, 2] + jox[i] * delta.x/d
      dif.red[i, 1] = dif.red[i, 2] + jred[i] * delta.x/d
      dif.chem[i,1] = dif.chem[i, 2]
      current.total[i] = -n * f * area * jox[i]
    }
  } else {
    for (i in 2:(t.units + 1)){
      for (j in 2:(x.units/scaling.factor)) {
        if (direction == -1) {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1])
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1]) - kcf * delta.t * dif.red[i-1, j] + kcr * delta.t * dif.chem[i - 1, j]
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) + kcf * delta.t * dif.red[i-1, j] - kcr * delta.t * dif.chem[i - 1, j]
        } else {
          dif.ox[i, j] = dif.ox[i-1, j] + lambda * (dif.ox[i-1, j-1] - 2 * dif.ox[i-1, j] + dif.ox[i-1, j+1]) - kcf * delta.t * dif.ox[i-1, j] + kcr * delta.t * dif.chem[i - 1, j]
          dif.red[i, j] = dif.red[i-1, j] + lambda * (dif.red[i-1, j-1] - 2 * dif.red[i-1, j] + dif.red[i-1, j+1])
          dif.chem[i, j] = dif.chem[i-1, j] + lambda * (dif.chem[i-1, j-1] - 2 * dif.chem[i-1, j] + dif.chem[i-1, j+1]) + kcf * delta.t * dif.ox[i-1, j] - kcr * delta.t * dif.chem[i - 1, j]
        }
      }
      jox[i] = -(kf[i] * dif.ox[i,2] - kb[i] * dif.red[i,2])/(1 + (kf[i] * delta.x)/d + (kb[i] * delta.x)/d)
      jred[i] = -jox[i]
      dif.ox[i, 1] = dif.ox[i, 2] + jox[i] * delta.x/d
      dif.red[i, 1] = dif.red[i, 2] + jred[i] * delta.x/d
      dif.chem[i,1] = dif.chem[i, 2]
      current.total[i] = -n * f * area * jox[i]
    }
  }

# if desired, add noise to the current; note default is sd.noise
# = 0, which returns the pure, noise-free simulated cyclic
# voltammogram

  noise = rnorm(t.units + 1, mean = 0,
                sd = sd.noise * max(abs(current.total))/100)
  current.total = current.total + noise

# return original inputs and calculated results as a list for use
# with other functions

  output = list("expt" = "LSV",
                "mechanism" = mechanism,
                "file_type" = "full",
                "current" = current.total*10^6,
                "potential" = potential,
                "time" = time,
                "distance" = distance,
                "oxdata" = dif.ox * 10^6,
                "reddata" = dif.red * 10^6,
                "chemdata" = dif.chem * 10^6,
                "formalE" = e.form,
                "initialE" = e.start,
                "endE" = e.end,
                "electrons" = n,
                "ko" = ko,
                "kcf" = kcf,
                "kcr" = kcr,
                "alpha" = alpha,
                "diffcoef" = d,
                "area" = area,
                "temperature" = temp,
                "scanrate" = scan.rate,
                "conc.bulk" = conc.bulk,
                "tunits" = t.units,
                "xunits" = x.units,
                "sdnoise" = sd.noise,
                "direction" = direction,
                "stir_rate" = stir.rate,
                "k_f" = kf,
                "k_b" = kb,
                "jox" = jox,
                "jred" = jred
  )
  invisible(output)
}