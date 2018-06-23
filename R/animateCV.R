#' Animate Cyclic Voltammetry Simulation
#'
#' Creates either an HTML or a GIF animation of a cyclic
#' voltammetry simulation created using \code{cvSim}. The
#' resulting animation displays the diffusion profiles for Ox,
#' Red, and, where appropriate, Z situated above the cyclic
#' voltammogram. Note: the \code{animateCV} function requires
#' that the \code{animation} package is installed.
#'
#' @param filename Name of the file that contains the results of a cyclic voltammetry simulation.
#' @param out_type Identifies the type of file generated, either an HTML file or a GIF file.
#' @param out_name Name used for the file(s) created by this function.
#'
#' @return For an HTML animation, the function saves four items in the working directory: a folder with CSS files, a folder with javascript files, a folder with the image files for the animation, and a .html file; the latter two files are named using the function's \code{out_name} arguement. For a GIF animation, the function saves a single .gif file using the function's \code{out_name} arguement.
#'
#' @importFrom graphics par plot grid lines legend
#'
#' @export
#'
#' @examples
#'\dontrun{
#'ex_cv = simulateCV(e.start = 0.25, e.switch = -0.25, e.form = 0)
#'animateCV(ex_cv, out_type = "gif", out_name = "ex_cv")
#'}

animateCV = function(filename, out_type = c("html", "gif"), out_name = "aniCV"){

# verify that animation package is installed

  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("You need to install the aniimation package.")
  }

# verify that the file was created using simulateCV

  if (filename$expt != "CV" & filename$file_type != "full") {
    stop("This file is not from a cyclic voltammetry simulation
         created using simulateCV")
  }

# determine whether output is HTML or GIF

  out_type = match.arg(out_type)

# adjust ani.options

  if (out_type == "html"){
    old.ani = animation::ani.options(interval = 0.2, verbose = FALSE)
  } else {
    old.ani = animation::ani.options(interval = 0.2, loop = 1)
  }

# set increment between the animation's frames

  pot_increment = round(length(filename$potential)/40, digits = 0)

# create and save files

  if (out_type == "html"){
  animation::saveHTML({
    old.par = par(mfrow = c(2, 1))
    for (i in seq(1, length(filename$time), pot_increment)) {
      plot(x = filename$distance, y = filename$oxdata[i,  ],
           type = "l", lwd = 3, col = "blue",
           ylim = c(0, 1050 * filename$conc.bulk),
           xlab = "distance from electrode (cm)",
           ylab = "concentration (mM)")
      grid()
      lines(x = filename$distance, y = filename$reddata[i, ],
            lwd = 3, col = "red")
      if (filename$mechanism != "E") {
        lines(x = filename$distance,
              y = filename$chemdata[i, ],
              lwd = 3, col = "green")
        legend(x = "right", legend = c("Ox", "Red", "Chem"),
               fill = c("blue", "red", "green"),
               bty = "n", inset = 0.05)
      } else {
        legend(x = "right", legend = c("Ox", "Red"),
               fill = c("blue", "red"),
               bty = "n", inset = 0.05)
      }
      plot(x = filename$potential[1:i], y = filename$current[1:i],
           col = "blue", type = "l", lwd = 3,
           xlim = c(max(filename$potential), min(filename$potential)),
           ylim = c(min(filename$current), max(filename$current)),
           xlab = "potential (V)", ylab = expression(paste("current (", mu, "A)")))
      grid()
    }
    par(old.par)
  },
  img.name = paste0(out_name,"_plot"),
  imgdir = paste0(out_name,"_dir"),
  htmlfile = paste0(out_name,".html"),
  navigator = FALSE
  )
  } else {
    animation::saveGIF({
      old.par = par(mfrow = c(2, 1))
      for (i in seq(1, length(filename$time), pot_increment)) {
        plot(x = filename$distance, y = filename$oxdata[i,  ],
             type = "l", lwd = 3, col = "blue",
             ylim = c(0, 1050 * filename$conc.bulk),
             xlab = "distance from electrode (cm)",
             ylab = "concentration (mM)")
        grid()
        lines(x = filename$distance, y = filename$reddata[i, ],
              lwd = 3, col = "red")
        if (filename$mechanism != "E") {
          lines(x = filename$distance,
                y = filename$chemdata[i, ],
                lwd = 3, col = "green")
          legend(x = "right", legend = c("Ox", "Red", "Chem"),
                 fill = c("blue", "red", "green"),
                 bty = "n", inset = 0.05)
        } else {
          legend(x = "right", legend = c("Ox", "Red"), fill = c("blue", "red"),
                 bty = "n", inset = 0.05)
        }
        plot(x = filename$potential[1:i], y = filename$current[1:i],
             col = "blue", type = "l", lwd = 3,
             xlim = c(max(filename$potential), min(filename$potential)),
             ylim = c(min(filename$current), max(filename$current)),
             xlab = "potential (V)", ylab = expression(paste("current (", mu, "A)")))
        grid()
      }
      par(old.par)}, movie.name = paste0(out_name,".gif")
      )
  }

# reset the original values for ani.options

  animation::ani.options(old.ani)

}