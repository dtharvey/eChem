#' Create Subsample of a Simulated Chronocoulogram
#'
#' Used to create a reduced data file of times and currents for a chronocoulogram. When passed to plotCC, the resulting plot shows the data as discrete points instead of as a line.
#'
#' @param filename The filename that contains the result of a simulated chronocoulometry experiment (created using the \code{ccSim} function).
#' @param data.reduction A value that gives the percentage of the original data to keep, which then is spaced evenly across the full data set.
#'
#' @return Returns a list with the following components \item{expt}{type of experiment; CC for a chronocoulometry experiment} \item{file_type}{value that indicates whether the output includes all data (full) or a subset of data (reduced); defaults to reduced} \item{charge}{vector giving the charge as a function of time} \item{time}{vector giving the time}
#'
#' @export
#'
#' @examples
#' ex_ca = simulateCA(e.start = 0.25, e.pulse = -0.25, e.form = 0,
#'   pulses = "double", t.2 = 20, x.units = 100, t.units = 1000)
#' ex_cc = simulateCC(ex_ca)
#' ex_ccsample = sampleCC(ex_cc, data.reduction = 5)
#' str(ex_ccsample)

sampleCC = function(filename, data.reduction = 1) {

  # check to verify that file is for a CC simulation

  if (filename$expt != "CC"){
    stop("The file must be for a chronocoulometry simluation")
  }

  # read in times and charge, determine increment for
  # keeping data, and extract the times and charges to keep

  time = filename$time
  charge = filename$charge
  len = length(filename$time)
  delta.data = len * data.reduction/100
  time.new = time[seq(1, len, len/delta.data)]
  charge.new = charge[seq(1, len, len/delta.data)]

# return values for use with plotCC

  output = list("expt" = filename$expt,
                "file_type" = "reduced",
                "charge" = charge.new,
                "time" = time.new
  )
  invisible(output)
}
