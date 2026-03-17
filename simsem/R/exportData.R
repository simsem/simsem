### Sunthud Pornprasertmanit and Alex Schoemann
### Last updated: 6 March 2026
### Export simulated datasets to external files

#' Export simulated datasets to external files
#'
#' Generates simulated datasets using \code{\link{sim}} and exports them to
#' data files that can be used by external SEM software such as Mplus or
#' LISREL.
#'
#' The function first generates data using \code{\link{sim}} and then writes
#' the datasets to disk using filenames based on \code{fileStem}. Missing
#' values are replaced with a user-specified missing-data code before
#' exporting.
#'
#' For Mplus, each replication is written as a separate \code{.dat} file,
#' and an additional file containing the list of dataset names is also
#' created. For LISREL, all datasets are stacked together and written to a
#' single file.
#'
#' @param nRep Number of simulation replications.
#' @param model A \code{\linkS4class{SimSem}} object specifying the simulation model.
#' @param n Sample size for each replication.
#' @param program Character string specifying the target software.
#' Supported options include \code{"Mplus"} and \code{"LISREL"}.
#' @param fileStem Character string used as the prefix for exported filenames.
#' @param miss Optional missing-data mechanism specification passed to
#' \code{\link{sim}}.
#' @param missCode Numeric value used to represent missing values in the
#' exported datasets.
#' @param datafun Optional user-specified data generation function.
#' @param pmMCAR Proportion of missing completely at random.
#' @param pmMAR Proportion of missing at random.
#' @param facDist Distribution specification for latent variables.
#' @param indDist Distribution specification for indicators.
#' @param errorDist Distribution specification for residual errors.
#' @param sequential Logical indicating whether simulation should run
#' sequentially.
#' @param modelBoot Logical indicating whether model bootstrapping is used.
#' @param realData Optional dataset used for resampling.
#' @param maxDraw Maximum number of attempts for drawing valid parameter sets.
#' @param misfitType Type of population misfit measure used when drawing
#' parameters.
#' @param misfitBounds Optional bounds for acceptable population misfit.
#' @param averageNumMisspec Logical indicating whether misfit should be
#' averaged across misspecified parameters.
#' @param optMisfit Character specifying optimization of misspecification
#' draws (e.g., \code{"min"} or \code{"max"}).
#' @param optDraws Number of candidate draws used when optimizing
#' misspecification.
#' @param seed Random seed used for simulation.
#' @param silent Logical indicating whether progress messages are suppressed.
#' @param multicore Logical indicating whether parallel processing is used.
#' @param numProc Number of processors used when \code{multicore = TRUE}.
#' @param params Logical. If \code{TRUE}, the function also returns the
#' parameter values used in each simulation replication.
#'
#' @details
#' The function is intended for situations where simulated datasets must be
#' exported to external SEM programs rather than analyzed directly within R.
#'
#' Missing values in generated datasets are replaced with the value specified
#' in \code{missCode} before exporting.
#'
#' When \code{program = "Mplus"}, the function creates:
#' \itemize{
#' \item One data file per replication (e.g., \code{sim1.dat}, \code{sim2.dat})
#' \item A file containing the list of dataset names
#' }
#'
#' When \code{program = "LISREL"}, all datasets are stacked and written to a
#' single data file.
#'
#' @return
#' If \code{params = TRUE}, returns the parameter values used in the
#' simulation. Otherwise, the function writes datasets to disk and returns
#' \code{NULL} invisibly.
#'
#' @seealso
#' \code{\link{sim}}, \code{\link{draw}}
#'
#' @examples
#' \dontrun{
#' exportData(
#'   nRep = 10,
#'   model = myModel,
#'   n = 500,
#'   program = "Mplus",
#'   fileStem = "simdata"
#' )
#' }
#'
#' @export
exportData <- function(
  nRep, model, n, program = "Mplus", fileStem = "sim", miss = NULL, missCode = -999, datafun = NULL,
  pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, sequential = FALSE,
  modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0",
  misfitBounds = NULL, averageNumMisspec = NULL, optMisfit = NULL, optDraws = 50,
  seed = 123321, silent = FALSE, multicore = FALSE, numProc = NULL, params = FALSE
) {
  ## Steps
  # 1. Generate data
  # 2. Write data (make files names needed)
  # 3. Write any other info

  # SP: Increase efficiency when params is TRUE (avoid running sim twice)
  if (!params) {
    dat <- sim(
      nRep = nRep, model = model, n = n, program = program, fileStem = fileStem, miss = miss, datafun = datafun,
      pmMCAR = pmMCAR, pmMAR = pmMAR, facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential,
      modelBoot = modelBoot, realData = realData, maxDraw = maxDraw, misfitType = misfitType,
      misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws,
      seed = seed, silent = silent, multicore = multicore, numProc = numProc, dataOnly = TRUE
    )

    # Replace missing data with NA with -999
    # Function to find missing in each dataset and replace with missing code
    replaceMissing <- function(dataF, misscode) {
      dataF[is.na(dataF)] <- misscode
      dataF
    }

    dat <- lapply(dat, replaceMissing, missCode)

    if (tolower(program) == "mplus") {
      fileN <- NULL
      for (i in 1:length(dat)) {
        fileN <- c(fileN, paste(fileStem, i, ".dat", sep = ""))
        write.table(dat[[i]], fileN[i], sep = " ", row.names = FALSE, col.names = FALSE)
      }
      # write list of file names
      write.table(fileN, paste(fileStem, ".dat", sep = ""), row.names = FALSE, col.names = FALSE, quote = FALSE)
    }

    if (tolower(program) == "lisrel") {
      datStack <- do.call(rbind, dat)
      write.table(datStack, paste(fileStem, ".dat", sep = ""), row.names = FALSE, col.names = TRUE)
    }

    ## If random paramaters are used, return a list of random paramaters
  } else {
    datP <- sim(
      nRep = nRep, model = model, n = n, program = program, fileStem = fileStem, miss = miss, datafun = datafun,
      pmMCAR = pmMCAR, pmMAR = pmMAR, facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential,
      modelBoot = modelBoot, realData = realData, maxDraw = maxDraw, misfitType = misfitType,
      misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws,
      seed = seed, silent = silent, multicore = multicore, numProc = numProc, paramOnly = TRUE
    )

    datP
  }
}
