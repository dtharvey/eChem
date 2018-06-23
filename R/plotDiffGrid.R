#' Plot Diffusion Grids
#'
#' Plots the diffusion grids used in cyclic voltammetry, linear sweep voltammetry, chronoamperometry, and chronocoulometry simulation. The diffusion grids are presented as heat maps giving the concentration of Ox, Red, or Z as a function of distance from the electrode surface on the \emph{x}-axis and time on the \emph{y}- axis. The scale on the \emph{x}-axis may be adjusted to highlight better the diffusion grid near the electrode's surface. Note: the \code{plotDiffGrid} function requires that the \code{plot3D} package is installed.
#'
#' @param filename Name of the file that contains the results of a cyclic voltammetry, linear sweep voltammetry, chronoamperometry, or chronocoulometry simulation created using \code{cvSim}, \code{lsvSim}, \code{caSim}, or \code{ccSim}.
#' @param species A vector of three logical values (\code{T} or \code{F}) indicating the species for which diffusion grids are displayed. The order in which species are identified is Ox, Red, and Z.
#' @param scale.factor A factor for adjusting the scale on the \emph{x}-axis. Setting the scale.factor to a value of less than 1 decreases the range of distances displayed in the diffusion grids.
#'
#'
#' @export
#'
#' @return Returns a single plot showing 1--3 diffusion grids.
#'
#' @examples
#' ex_cv = simulateCV(e.start = 0.25, e.switch = -0.25, e.form = 0,
#'   x.units = 100, t.units = 1000)
#' plotDiffGrid(ex_cv, species = c(TRUE, TRUE, FALSE),
#'   scale.factor = 0.5)

plotDiffGrid = function(filename, species = c(TRUE, TRUE, FALSE),
                         scale.factor = 1){

# verify that the plot3D package is installed

  if (!requireNamespace("plot3D", quietly = TRUE)) {
    stop("You need to install the plot3D package to use diffusionGrid.")
  }

# check that logical values provided for all three species;
# determine number of species to plot; adjust plotting parameters
# so diffusion grids are displayed in a single row

  if(length(species) != 3){
    stop("Be sure to enter logical values for Ox, Red, and Z.")
  }

  no.species = sum(species)

  par(mfrow = c(1, no.species))

# set color scales

  colorgradient_ox = plot3D::ramp.col(c("white", "blue"))
  colorgradient_red = plot3D::ramp.col(c("white", "red"))
  colorgradient_chem = plot3D::ramp.col(c("white", "green"))

# create initial matrix for Z in case it is needed

  chem.conc = matrix(0, nrow = filename$tunits, ncol = filename$xunits)

# in order to orient the diffusion grid so that rows correspond
# to times, with values on the y-axis and so that columns
# correspond to distances, with values on the x-axis, the
# transpose of the diffusion grid matrix is used for the
# concentrations plotted on the z-axis

  if (species[1] == TRUE){
    plot3D::image2D(z = t(filename$oxdat),
                    y = filename$time,
                    x = filename$distance,
                    contour = FALSE,
                    ylab = "time (s)", xlab = "distance (cm)",
                    xlim = c(min(filename$distance * scale.factor),
                             max(filename$distance * scale.factor)),
                    clab = "[Ox] (mM)", col = colorgradient_ox)
  }
  if (species[2] == TRUE) {
    plot3D::image2D(z = t(filename$reddata),
                    y = filename$time,
                    x = filename$distance,
                    contour = FALSE,
                    ylab = "time (s)", xlab = "distance (cm)",
                    xlim = c(min(filename$distance * scale.factor),
                             max(filename$distance * scale.factor)),
                    clab = "[Red] (mM)", col = colorgradient_red)
  }
  if (species[3] == TRUE) {
    if (filename$mechanism != "E") {
      chem.conc = filename$chemdata
    }
    plot3D::image2D(z = t(chem.conc),
                   y = filename$time,
                   x = filename$distance,
                   contour = FALSE,
                   ylab = "time (s)", xlab = "distance (cm)",
                   xlim = c(min(filename$distance * scale.factor),
                            max(filename$distance * scale.factor)),
                   clab = "[Z] (mM)", col = colorgradient_chem)
  }

# restore original plotting parameters

  par(mfrow = c(1,1))

}