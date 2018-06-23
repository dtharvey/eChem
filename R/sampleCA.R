#' Create Subsample of a Simulated Chronoamperogram
#'
#' Used to create a reduced data file of times and currents for a chronoamperogram. When passed to plotCA, the resulting plot shows the data as discrete points instead of as a line.
#'
#' @param filename The filename that contains the result of a simulated chronoamperometry experiment (created using the \code{caSim} function).
#' @param data.reduction A value that gives the percentage of the original data to keep, which then is spaced evenly across the full data set
#'
#' @return Returns a list with the following components \item{expt}{type of experiment; CA for a chronoamperometry experiment} \item{file_type}{value that indicates whether the output includes all data (full) or a subset of data (reduced); defaults to reduced} \item{current}{vector giving the current as a function of time} \item{potential}{vector giving the potential as a function of time}
#'
#' @export
#'
#' @examples
#' ex_ca = simulateCA(e.start = 0.25, e.pulse = -0.25, e.form = 0,
#'   pulses = "double", t.2 = 20, x.units = 100, t.units = 1000)
#' ex_casample = sampleCA(ex_ca, data.reduction = 5)
#' str(ex_casample)

sampleCA = function(filename, data.reduction = 1) {

# check to verify that file is for a CA simulation

  if (filename$expt != "CA"){
    stop("The file must be for a chronoamperometry simluation")
  }

# read in times and currents, determine increment for
# keeping data, and extract the times and currents to keep

  time = filename$time
  current = filename$current
  len = length(filename$time)
  delta.data = len * data.reduction/100
  time.new = time[seq(1, len, len/delta.data)]
  cur.new = current[seq(1, len, len/delta.data)]

# return values for use with plotCA

  output = list("expt" = filename$expt,
                "file_type" = "reduced",
                "current" = cur.new,
                "time" = time.new
  )
  invisible(output)
}