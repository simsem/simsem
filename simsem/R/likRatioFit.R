# logLikFit: Get a log likelihood ratio based on the fit indices

likRatioFit <- function(outMod1, outMod2, dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2,
    usedFit = NULL, prior = 1) {
	usedFit <- cleanUsedFit(usedFit, colnames(dat1Mod1@fit), colnames(dat1Mod2@fit), colnames(dat2Mod1@fit), colnames(dat2Mod2@fit))

  observedFit <- as.data.frame(rbind(lavaan::lavInspect(outMod1, "fit"),
                                     lavaan::lavInspect(outMod2, "fit"))[,usedFit])

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

# \title{
	# Find the density (likelihood) of a pair value in 2D Kernel Density Estimate
# }
# \description{
	# Find the density (likelihood) of a pair value in 2D Kernel Density Estimate
# }
# \usage{
# findphist(value, hist)
# }
# \arguments{
  # \item{value}{
	# A target pair of values
# }
  # \item{hist}{
	# A 2D Binned Kernel Density Estimate
# }
# }
# \value{
	# The probability (density) of the target pair of value
# }

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

# \title{
	# Fit the 2D Kernel Density Estimate
# }
# \description{
	# Fit the 2D Kernel Density Estimate to a pair of variables
# }
# \usage{
# find2Dhist(vec1, vec2)
# }
# \arguments{
  # \item{vec1}{
	# Variable 1
# }
  # \item{vec2}{
	# Variable 2
# }
# }
# \value{
	# The 2D Kernel Density Estimate based on each pair of values in \code{vec1} and \code{vec2}
# }

find2Dhist <- function(vec1, vec2, gridsize = c(51L, 51L)) {
    result <- NA
    try(result <- suppressWarnings(KernSmooth::bkde2D(cbind(vec1, vec2), c(KernSmooth::dpik(vec1), KernSmooth::dpik(vec2)), gridsize = gridsize)),
        silent = TRUE)
    return(result)
}
