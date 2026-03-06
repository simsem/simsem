### Sunthud Pornprasertmanit & Terry D. Jorgensen
### Last updated: 6 March 2026
### Get a log likelihood ratio based on the fit indices

#' Compute a Likelihood Ratio Based on Fit Indices
#'
#' Computes a likelihood ratio comparing two competing models using simulated
#' distributions of fit indices. The likelihood is estimated using 2D kernel
#' density estimates constructed from simulation results.
#'
#' @param outMod1 Fitted \code{lavaan} object for Model 1.
#' @param outMod2 Fitted \code{lavaan} object for Model 2.
#' @param dat1Mod1 Simulation results for Model 1 under data-generating Model 1.
#' @param dat1Mod2 Simulation results for Model 2 under data-generating Model 1.
#' @param dat2Mod1 Simulation results for Model 1 under data-generating Model 2.
#' @param dat2Mod2 Simulation results for Model 2 under data-generating Model 2.
#' @param usedFit Character vector of fit indices to be used. If \code{NULL},
#'   the default fit indices are used.
#' @param prior Prior odds for Model 1 relative to Model 2.
#'
#' @return A vector of likelihood ratios for the selected fit indices.
#'
#' @export
likRatioFit <- function(outMod1, outMod2, dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2,
    usedFit = NULL, prior = 1) {
	usedFit <- cleanUsedFit(usedFit, colnames(dat1Mod1@fit), colnames(dat1Mod2@fit), colnames(dat2Mod1@fit), colnames(dat2Mod2@fit))

  observedFit <- as.data.frame(rbind(lavInspect(outMod1, "fit"),
                                     lavInspect(outMod2, "fit"))[,usedFit])

  mod1 <- clean(dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2)
  dat1Mod1 <- mod1[[1]]
  dat1Mod2 <- mod1[[2]]
  mod2 <- clean(dat2Mod1, dat2Mod2)
  dat2Mod1 <- mod2[[1]]
  dat2Mod2 <- mod2[[2]]
  dat1Mod1Fit <- as.data.frame(dat1Mod1@fit[, usedFit])
  dat1Mod2Fit <- as.data.frame(dat1Mod2@fit[, usedFit])
  dat2Mod1Fit <- as.data.frame(dat2Mod1@fit[, usedFit])
  dat2Mod2Fit <- as.data.frame(dat2Mod2@fit[, usedFit])

  histDat1 <- mapply(find2Dhist, vec1 = dat1Mod1Fit, vec2 = dat1Mod2Fit, SIMPLIFY = FALSE)
  histDat2 <- mapply(find2Dhist, vec1 = dat2Mod1Fit, vec2 = dat2Mod2Fit, SIMPLIFY = FALSE)

  likDat1 <- mapply(findphist, observedFit, histDat1)
  likDat2 <- mapply(findphist, observedFit, histDat2)
  likDat1[likDat1 == 0] <- 1e-07
  likDat2[likDat2 == 0] <- 1e-07
  (likDat1/likDat2) * prior
}

#' Find the Density of a Pair of Values from a 2D Kernel Density Estimate
#'
#' Returns the estimated density corresponding to a pair of values from a
#' two-dimensional kernel density estimate.
#'
#' @param value Numeric vector of length two representing the target pair of values.
#' @param hist A 2D binned kernel density estimate produced by
#'   \code{\link[KernSmooth]{bkde2D}}.
#'
#' @return The estimated density corresponding to the pair of values.
#'
#' @keywords internal
findphist <- function(value, hist) {
    if (is.na(hist)) {
        return(NA)
    } else {
        x <- hist$x1
        y <- hist$x2
        posx <- posy <- NA
        x <- (x[2:length(x)] + x[1:(length(x) - 1)])/2
        y <- (y[2:length(y)] + y[1:(length(y) - 1)])/2
        testx <- value[1] < x
        testy <- value[2] < y
        if (sum(testx) == 0) {
            posx <- length(x)
        } else {
            posx <- which(testx)[1]
        }
        if (sum(testy) == 0) {
            posy <- length(y)
        } else {
            posy <- which(testy)[1]
        }
        return(hist$fhat[posx, posy])
    }
}

#' Fit a 2D Kernel Density Estimate
#'
#' Fits a two-dimensional kernel density estimate for a pair of variables.
#'
#' @param vec1 Numeric vector representing the first variable.
#' @param vec2 Numeric vector representing the second variable.
#' @param gridsize Integer vector specifying the grid size used for the
#'   kernel density estimate.
#'
#' @return A 2D kernel density estimate object produced by
#'   \code{\link[KernSmooth]{bkde2D}}.
#'
#' @keywords internal
find2Dhist <- function(vec1, vec2, gridsize = c(51L, 51L)) {
    result <- NA
    try(result <- suppressWarnings(KernSmooth::bkde2D(cbind(vec1, vec2), c(KernSmooth::dpik(vec1), KernSmooth::dpik(vec2)), gridsize = gridsize)),
        silent = TRUE)
    return(result)
}
