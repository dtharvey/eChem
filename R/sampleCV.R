#' Create Subsample of a Simulated Cyclic Voltammogram
#'
#' Used to create a reduced data file of potentials and currents for a cyclic voltammogram or a linear sweep voltammogram. When passed to plotCV, the resulting plot shows the data as discrete points instead of as a line.
#'
#' @param filename The filename that contains the result of a simulated cyclic voltammetry experiment (created using the \code{cvSim} function).
#'
#' @param data.reduction A value that gives the percentage of the original data to keep, which then is spaced evenly across the full data set.
#'
#' @return Returns a list with the following components \item{expt}{type of experiment; CV for a cyclic voltammetry simulation} \item{file_type}{value that indicates whether the output includes all data (full) or a subset of data (reduced); defaults to reduced} \item{current}{vector giving the current as a function of time} \item{potential}{vector giving the potential as a function of time}
#'
#' @export
#'
#' @examples
#' ex_cv = simulateCV(e.start = 0.25, e.switch = -0.25, e.form = 0,
#'   x.units = 100, t.units = 1000)
#' ex_cvsample = sampleCV(ex_cv, data.reduction = 5)
#' str(ex_cvsample)

sampleCV = function(filename, data.reduction = 1) {

# check to verify that file is for a CV simulation

  if (filename$expt != "CV"){
    stop("The file must be for a cyclic voltammetry simluation")
  }

# check to verify that data.reduction is between 0 and 100

  if(data.reduction <= 0){
    stop("You must retain some data; thus, data.reduction must
         have a value greater than 0")
  }

  if(data.reduction > 100){
    stop("You cannot retain more data than is present in the original
          data file; thus, data.reduction cannot excede 100")
  }

# read in potentials and currents, determine increment for
# keeping data, and extract the potentials and currents to keep

  potential = filename$potential
  current = filename$current
  len = length(filename$potential)
  delta.data = len * data.reduction/100
  pot.new = potential[seq(1, len, len/delta.data)]
  cur.new = current[seq(1, len, len/delta.data)]

# return values for use with plotCV

  output = list("expt" = filename$expt,
                "file_type" = "reduced",
                "current" = cur.new,
                "potential" = pot.new
                 )
    invisible(output)
}