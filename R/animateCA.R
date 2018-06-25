#' Animate Chronoamperometry Simulation
#'
#' Creates either an HTML or a GIF animation of a
#' chronoamperometry simulation created using \code{caSim}. The
#' resulting animation displays the diffusion profiles for Ox,
#' Red, and, where appropriate, Z situated above the
#' chronoamperogram. Note: the \code{animateCA} function requires
#' that the \code{animation} package is installed.
#'
#' @param filename Name of the file that contains the results of a chronoamperometry simulation.
#' @param out_type Identifies the type of file generated, either an HTML file or a GIF file.
#' @param out_name Name used for the file(s) created by this function.
#'
#' @return For an HTML animation, the function saves four items in the working directory: a folder with CSS files, a folder with javascript files, a folder with the image files for the animation, and a .html file; the latter two files are named using the function's \code{out_name} argument. For a GIF animation, the function saves a single .gif file using the function's \code{out_name} argument. See the vignettes for examples.
#'
#' @importFrom graphics par plot grid lines legend
#'
#' @export
#'


animateCA = function(filename, out_type = c("html", "gif"), out_name = "aniCA"){

# verify that animation package is installed

  if (!requireNamespace("animation", quietly = TRUE)) {
    stop("You need to install the aniimation package.")
  }

# verify that the file was created using caSim

  if (filename$expt != "CA" & filename$file_type != "full") {
    stop("This file is not from a chronoamperometry
         simulation created using caSim.")
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

  time_increment = round(length(filename$time)/40, digits = 0)

# create and save files

  if (out_type == "html"){
    animation::saveHTML({
      old.par = par(mfrow = c(2, 1))
      for (i in seq(1, length(filename$time), time_increment)) {
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
        plot(x = filename$time[1:i], y = filename$current[1:i],
             col = "blue", type = "l", lwd = 3,
             xlim = c(min(filename$time), max(filename$time)),
             ylim = c(min(filename$current), max(filename$current)),
             xlab = "time (s)",ylab = expression(paste("current (", mu, "A)")))
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
      for (i in seq(1, length(filename$time), time_increment)) {
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
        plot(x = filename$time[1:i], y = filename$current[1:i],
             col = "blue", type = "l", lwd = 3,
             xlim = c(min(filename$time), max(filename$time)),
             ylim = c(min(filename$current), max(filename$current)),
             xlab = "time (s)", ylab = expression(paste("current (", mu, "A)")))
        grid()
      }
      par(old.par)}, movie.name = paste0(out_name,".gif")
    )
  }

# reset the original values for ani.options

  animation::ani.options(old.ani)

}