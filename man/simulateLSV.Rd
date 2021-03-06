% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulateLSV.R
\name{simulateLSV}
\alias{simulateLSV}
\title{Simulate a Linear Sweep Voltammetry Experiment}
\usage{
simulateLSV(e.start = 0, e.end = -1, e.form = -0.25, mechanism = c("E",
  "EC", "CE"), ko = 1, kcf = 0, kcr = 0, n = 1, alpha = 0.5,
  d = 1e-05, area = 0.01, temp = 298.15, scan.rate = 1,
  conc.bulk = 0.001, t.units = 2000, x.units = 180, sd.noise = 0,
  stir.rate = c("off", "slow", "medium", "fast"))
}
\arguments{
\item{e.start}{Initial potential (in volts).}

\item{e.end}{Final potential (in volts).}

\item{e.form}{Formal potential for the redox reaction (in volts).}

\item{mechanism}{Mechanism for the electrochemical system; one of \code{E} for redox reaction only, \code{EC} for redox reaction with a following chemical reaction, or \code{CE} for redox reaction with a preceding chemical reaction. Default is \code{E}.}

\item{ko}{Standard heterogeneous electron transfer rate constant for the redox reaction (in cm/s).}

\item{kcf}{Homogeneous first-order rate constant for the forward chemical reaction (in s^-1).}

\item{kcr}{Homogeneous first-order rate constant for the reverse chemical reaction (in s^-1).}

\item{n}{Number of electrons in the redox reaction.}

\item{alpha}{Transfer coefficient.}

\item{d}{Diffusion coefficient for Ox and Red (in cm^2 s^-1).}

\item{area}{Surface area of the electrode (in cm^2).}

\item{temp}{Temperature (in K).}

\item{scan.rate}{Rate at which the potential is changed (in V/s).}

\item{conc.bulk}{Initial bulk concentration of Ox or Red for an E or an EC mechanism, or the combined initial concentrations of Ox and Z, or of Red and Z for a CE mechanism (in mol/L).}

\item{t.units}{The number of increments in time for the diffusion grids.}

\item{x.units}{The number of increments in distance for the diffusion grids.}

\item{sd.noise}{The standard deviation for noise as a percent of maximum current (in \eqn{\mu}A).}

\item{stir.rate}{The rate at which the solution is stirred with options for \code{off}, or no stirring, and for \code{slow}, \code{medium}, and \code{fast} stirring; defaults to \code{off}.}
}
\value{
Returns a list with the following components \item{expt}{type of experiment; defaults to LSV for a linear sweep voltammetry simulation} \item{mechanism}{type of mechanism used for the simulation} \item{file_type}{value that indicates whether the output includes all data (full) or a subset of data (reduced); defaults to full for \code{lsvSim}} \item{current}{vector giving the current as a function of time} \item{potential}{vector giving the potential as a function of time} \item{time}{vector giving the times used for the diffusion grids} \item{distance}{vector giving the distances from electrode surface used for the diffusion grids} \item{oxdata}{diffusion grid, as a matrix, giving the concentration of Ox} \item{reddata}{diffusion grid, as a matrix, giving the concentrations of Red} \item{chemdata}{diffusion grid, as a matrix, giving the concentrations of Z} \item{formalE}{formal potential for the redox reaction} \item{initialE}{initial potential} \item{endE}{end potential} \item{electrons}{number of electrons, n, in the redox reaction} \item{ko}{standard heterogeneous electron transfer rate constant} \item{kcf}{homogeneous first-order rate constant for forward chemical reaction} \item{kcr}{homogeneous first-order rate constant for reverse chemical reaction} \item{alpha}{transfer coefficient} \item{diffcoef}{diffusion coefficient for Ox and Red} \item{area}{surface area for electrode} \item{temperature}{temperature} \item{scanrate}{scan rate} \item{conc.bulk}{initial concentration of Ox or Red for an E or EC mechanism, or the combined initial concentrations of Ox and Z, or of Red and Z for a CE mechanism} \item{tunits}{the number of increments in time for the diffusion grids} \item{xunits}{the number of increments in distance for the diffusion grids} \item{sdnoise}{standard deviation, as percent of maximum current, used to add noise to simulated data} \item{direction}{-1 for an initial reduction reaction of Ox to Red; +1 for an initial oxidation reaction of Red to Ox} \item{stir_rate}{rate at which solution is stirred} \item{k_f}{vector of forward electron transfer rate constant as a function of potential} \item{k_b}{vector of reverse electron transfer rate constant as a function of potential} \item{jox}{vector giving the flux of Ox to the electrode surface as a function of potential} \item{jred}{vector giving the flux of Red to the electrode surface as a function of potential}
}
\description{
Simulates a linear sweep voltammetry experiment as either an
E, EC, or CE mechanism, where E is a redox reaction and where
C is a chemical reaction that either precedes or follows the
redox reaction.
}
\examples{
ex_lsv = simulateLSV(e.start = 0.25, e.end = -0.25, e.form = 0,
  stir.rate = "fast", x.units = 100, t.units = 1000)
str(ex_lsv)
}
