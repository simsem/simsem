# logLikFit: Get a log likelihood ratio based on the fit indices

likRatioFit <- function(outMod1, outMod2, dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2, usedFit=NULL, prior=1) {
	if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit

	observedFit <- as.data.frame(rbind(outMod1@fit, outMod2@fit)[,usedFit])
	
	mod1 <- clean(dat1Mod1, dat1Mod2, dat2Mod1, dat2Mod2)
	dat1Mod1 <- mod1[[1]]
	dat1Mod2 <- mod1[[2]]
	mod2 <- clean(dat2Mod1, dat2Mod2)
	dat2Mod1 <- mod2[[1]]
	dat2Mod2 <- mod2[[2]]
	dat1Mod1Fit <- as.data.frame(dat1Mod1@fit[,usedFit])
	dat1Mod2Fit <- as.data.frame(dat1Mod2@fit[,usedFit])
	dat2Mod1Fit <- as.data.frame(dat2Mod1@fit[,usedFit])
	dat2Mod2Fit <- as.data.frame(dat2Mod2@fit[,usedFit])
	
	histDat1 <- mapply(find2Dhist, vec1 = dat1Mod1Fit, vec2 = dat1Mod2Fit, SIMPLIFY=FALSE)
	histDat2 <- mapply(find2Dhist, vec1 = dat2Mod1Fit, vec2 = dat2Mod2Fit, SIMPLIFY=FALSE)
	
	likDat1 <- mapply(findphist, observedFit, histDat1)
	likDat2 <- mapply(findphist, observedFit, histDat2)
	likDat1[likDat1 == 0] <- 0.0000001
	likDat2[likDat2 == 0] <- 0.0000001
	(likDat1/likDat2) * prior
}

findphist <- function(value, hist) {
	x <- hist$x1
	y <- hist$x2
	posx <- posy <- NA
	x <- (x[2:length(x)] + x[1:(length(x)-1)])/2
	y <- (y[2:length(y)] + y[1:(length(y)-1)])/2
	testx <- value[1] < x
	testy <- value[2] < y
	if(sum(testx) == 0) {
		posx <- length(x)
	} else {
		posx <- which(testx)[1]
	}
	if(sum(testy) == 0) {
		posy <- length(y)
	} else {
		posy <- which(testy)[1]
	}
	return(hist$fhat[posx, posy])
}

find2Dhist <- function(vec1, vec2) {
	library(KernSmooth)
	suppressWarnings(bkde2D(cbind(vec1, vec2), c(dpik(vec1), dpik(vec2))))
}
