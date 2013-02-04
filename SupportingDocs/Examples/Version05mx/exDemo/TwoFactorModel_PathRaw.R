library(simsem)
library(semTools)
library(OpenMx)

############################ Fitting twoFactorFit

data(myFADataRaw)

twoFactorRaw <- myFADataRaw[,c("x1","x2","x3","y1","y2","y3")]

twoFactorModel <- mxModel("Two Factor Model Path Specification", 
	type="RAM",
    mxData(
    	observed=twoFactorRaw, 
    	type="raw"
    ),
    manifestVars=c("x1", "x2", "x3", "y1", "y2", "y3"),
    latentVars=c("F1","F2"),
    mxPath(
    	from=c("x1", "x2", "x3", "y1", "y2", "y3"),
        arrows=2,
        free=TRUE,
        values=c(1,1,1,1,1,1),
        labels=c("e1","e2","e3","e4","e5","e6")
    ),
    # residual variances
	# -------------------------------------
    mxPath(
    	from=c("F1","F2"),
        arrows=2,
		connect="unique.pairs",
        free=TRUE,
		values=c(1, .5, 1),
		labels=c("varF1", "cov", "varF2")
    ), 
    # latent variances and covaraince
	# -------------------------------------
    mxPath(
    	from="F1",
        to=c("x1","x2","x3"),
        arrows=1,
        free=c(FALSE,TRUE,TRUE),
        values=c(1,1,1),
        labels=c("l1","l2","l3")
    ),
    # factor loadings for x variables
	# -------------------------------------   
    mxPath(
    	from="F2",
        to=c("y1","y2","y3"),
        arrows=1,
        free=c(FALSE,TRUE,TRUE),
        values=c(1,1,1),
        labels=c("l4","l5","l6")
    ),
	# factor loadings for y variables
	# ------------------------------------- 
    mxPath(
    	from="one",
        to=c("x1","x2","x3","y1","y2","y3","F1","F2"),
        arrows=1,
        free=c(T ,T, T, T, T, T, F, F),
        values=c(1,1,1,1,1,1,0,0),
        labels=c("meanx1","meanx2","meanx3",
                 "meany1","meany2","meany3",
                  NA,NA)
    )
	# means
	# -------------------------------------
) 
    
twoFactorFit <- mxRun(twoFactorModel)
fitMeasuresMx(twoFactorFit)
twoFactorFitSim <- sim(10, twoFactorFit, n = 200)
summary(twoFactorFit)
