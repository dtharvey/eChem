% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animateLSV.R
\name{animateLSV}
\alias{animateLSV}
\title{Animate Linear Sweep Voltammetry Simulation}
\usage{
animateLSV(filename, out_type = c("html", "gif"), out_name = "aniLSV")
}
\arguments{
\item{filename}{Name of the file that contains the results of a linear sweep voltammetry simulation.}

\item{out_type}{identifies the type of file generated, either an HTML file or a GIF file.}

\item{out_name}{Name used for the file(s) created by this function.}
}
\value{
For an HTML animation, the function saves four items in the working directory: a folder with CSS files, a folder with javascript files, a folder with the image files for the animation, and a .html file; the latter two files are named using the function's \code{out_name} argument. For a GIF animation, the function saves a single .gif file using the function's \code{out_name} argument. See the vignettes for examples.
}
\description{
Creates either an HTML or a GIF animation of a linear sweep
voltammetry simulation created using \code{lsvSim}. The
resulting animation displays the diffusion profiles for Ox,
Red, and, where appropriate, Z situated above the linear sweep
voltammogram. Note: the \code{animateLSV} function requires
that the \code{animation} package is installed.
}
