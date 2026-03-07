.onAttach <- function(libname, pkgname) {
  version <- utils::packageVersion(pkgname)
  packageStartupMessage(" ")
  packageStartupMessage("#################################################################")
  packageStartupMessage("This is ", paste(pkgname, version))
  packageStartupMessage(pkgname, " is BETA software! Please report any bugs.")
  packageStartupMessage(pkgname, " was first developed at the University of Kansas Center for")
  packageStartupMessage("Research Methods and Data Analysis, under NSF Grant 1053160.")
  packageStartupMessage("#################################################################")
	options('simsem.multicore' = FALSE)
}

#' @keywords internal
#' @import methods
#' @importFrom graphics abline barplot contour hist legend lines par persp points
#' @importFrom stats binomial coef cor cov cov2cor density glm loess.smooth nlminb pchisq predict qchisq qnorm quantile runif sd uniroot var vcov
#' @importFrom utils capture.output combn data write.table
#' @importFrom lavaan lav_cor2cov lavInspect parTable
"_PACKAGE"
