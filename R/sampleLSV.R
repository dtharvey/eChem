#' Create Subsample of a Simulated Linear Sweep Voltammogram
#'
#' Used to create a reduced data file of potentials and currents for a linear sweep voltammogram. When passed to plotLSV, the resulting plot shows the data as discrete points instead of as a line.
#'
#' @param filename The filename that contains the result of a simulated linear sweep voltammetry experiment (created using the \code{lsvSim} function).
#' @param data.reduction A value that gives the percentage of the original data to keep, which then is spaced evenly across the full data set.
#'
#' @return Returns a list with the following components \item{expt}{type of experiment; LSV for a linear sweep voltammetry experiment} \item{file_type}{value that indicates whether the output includes all data (full) or a subset of data (reduced); defaults to reduced} \item{current}{vector giving the current as a function of time} \item{potential}{vector giving the potential as a function of time}
#'
#' @export
#'
#' @examples
#' ex_lsv = simulateLSV(e.start = 0.25, e.end = -0.25, e.form = 0,
#'   stir.rate = "fast", x.units = 100, t.units = 1000)
#' ex_lsvsample = sampleLSV(ex_lsv, data.reduction = 5)
#' str(ex_lsvsample)

sampleLSV = function(filename, data.reduction = 1) {

# check to verify that file is for an LSV simulation

  if (filename$expt != "LSV"){
    stop("The file must be for a linear sweep voltammetry simluation")
  }

# read in potentials and currents, determine increment for
# keeping data, and extract the potentials and currents to keep

  potential = filename$potential
  current = filename$current
  len = length(filename$potential)
  delta.data = len * data.reduction/100
  pot.new = potential[seq(1, len, len/delta.data)]
  cur.new = current[seq(1, len, len/delta.data)]

# return values for use with plotLSV

  output = list("expt" = filename$expt,
                "file_type" = "reduced",
                "current" = cur.new,
                "potential" = pot.new
                 )
    invisible(output)
}