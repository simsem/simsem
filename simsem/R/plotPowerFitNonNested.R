# plotPowerFitNested: This function will plot sampling distributions of
# difference in fit indices that visualize power

plotPowerFitNonNested <- function(dat2Mod1, dat2Mod2, dat1Mod1 = NULL, dat1Mod2 = NULL, 
    cutoff = NULL, usedFit = NULL, alpha = 0.05, contN = TRUE, contMCAR = TRUE, contMAR = TRUE, 
    useContour = TRUE, logistic = TRUE, onetailed = FALSE) {
    if (is.null(cutoff) & is.null(dat1Mod1) & is.null(dat1Mod2)) 
        stop("Please specify result objects representing the simulation results for datasets from Model 1 ('dat1Mod1' and 'dat1Mod2') or cutoff")
	usedFit <- cleanUsedFit(usedFit)
    mod2 <- clean(dat2Mod1, dat2Mod2)
    dat2Mod1 <- mod2[[1]]
    dat2Mod2 <- mod2[[2]]
    
    if (!isTRUE(all.equal(unique(dat2Mod1@paramValue), unique(dat2Mod2@paramValue)))) 
        stop("'dat2Mod1' and 'dat2Mod2' are based on different data and cannot be compared, check your random seed")
    if (!is.null(dat1Mod1) & !is.null(dat1Mod2)) {
        mod1 <- clean(dat1Mod1, dat1Mod2)
        dat1Mod1 <- mod1[[1]]
        dat1Mod2 <- mod1[[2]]
        if (!isTRUE(all.equal(unique(dat1Mod1@paramValue), unique(dat1Mod2@paramValue)))) 
            stop("'dat1Mod1' and 'dat1Mod2' are based on different data and cannot be compared, check your random seed")
        if (!multipleAllEqual(unique(dat1Mod1@n), unique(dat1Mod2@n), unique(dat2Mod1@n), 
            unique(dat2Mod2@n))) 
            stop("Models are based on different values of sample sizes")
        if (!multipleAllEqual(unique(dat1Mod1@pmMCAR), unique(dat1Mod2@pmMCAR), unique(dat2Mod1@pmMCAR), 
            unique(dat2Mod2@pmMCAR))) 
            stop("Models are based on different values of the percent completely missing at random")
        if (!multipleAllEqual(unique(dat1Mod1@pmMAR), unique(dat1Mod2@pmMAR), unique(dat2Mod1@pmMAR), 
            unique(dat2Mod2@pmMAR))) 
            stop("Models are based on different values of the percent missing at random")
    } else {
        if (!isTRUE(all.equal(unique(dat2Mod1@n), unique(dat2Mod2@n)))) 
            stop("Models are based on different values of sample sizes")
        if (!isTRUE(all.equal(unique(dat2Mod1@pmMCAR), unique(dat2Mod2@pmMCAR)))) 
            stop("Models are based on different values of the percent completely missing at random")
        if (!isTRUE(all.equal(unique(dat2Mod1@pmMAR), unique(dat2Mod2@pmMAR)))) 
            stop("Models are based on different values of the percent missing at random")
    }
    
    
    nrep <- dim(dat2Mod1@fit)[[1]]
    if (is.null(usedFit)) 
        usedFit <- getKeywords()$usedFit
    if (!is.null(cutoff)) {
		names(cutoff) <- cleanUsedFit(names(cutoff))
        usedFit <- intersect(usedFit, names(cutoff))
        cutoff <- cutoff[usedFit]
    }
    # Create matrix of predictors (randomly varying params)
    x <- NULL
    pred <- NULL
    
    if ((length(unique(dat2Mod1@n)) > 1) && contN) {
        if (!length(dat2Mod1@n) == nrep) {
            stop("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
        }
        x <- cbind(x, N = dat2Mod1@n)
        pred$N <- min(dat2Mod1@n):max(dat2Mod1@n)
    }
    if ((length(unique(dat2Mod1@pmMCAR)) > 1) && contMCAR) {
        if (!length(dat2Mod1@pmMCAR) == nrep) {
            stop("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
        }
        x <- cbind(x, pmMCAR = dat2Mod1@pmMCAR)
        pred$MCAR <- seq(min(dat2Mod1@pmMCAR), max(dat2Mod1@pmMCAR), by = 0.01)
        
    }
    if ((length(unique(dat2Mod1@pmMAR)) > 1) && contMAR) {
        if (!length(dat2Mod1@pmMAR) == nrep) {
            stop("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
        }
        x <- cbind(x, pmMAR = dat2Mod1@pmMAR)
        pred$MAR <- seq(min(dat2Mod1@pmMAR), max(dat2Mod1@pmMAR), by = 0.01)
        
    }
    
    Data1 <- NULL
    Data2 <- as.data.frame((dat2Mod1@fit - dat2Mod2@fit))
    if (!is.null(dat1Mod1) & !is.null(dat1Mod2)) 
        Data1 <- as.data.frame((dat1Mod1@fit - dat1Mod2@fit))
    
    condition <- c(length(dat2Mod1@pmMCAR) > 1, length(dat2Mod1@pmMAR) > 1, length(dat2Mod1@n) > 
        1)
    condValue <- cbind(dat2Mod1@pmMCAR, dat2Mod1@pmMAR, dat2Mod1@n)
    colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
    
    cutoff1 <- cutoff
    cutoff2 <- NULL
    cutoff3 <- NULL
    if (!is.null(cutoff)) 
        cutoff3 <- -cutoff
    cutoff4 <- NULL
    if (!is.null(alpha)) {
        if (is.null(x)) {
            if (is.null(cutoff)) {
                cutoff <- getCutoffNonNested(dat1Mod1, dat1Mod2, alpha = alpha, onetailed = onetailed)[[1]]
                cutoff1 <- cutoff[1, ]
                cutoff2 <- cutoff[2, ]
                if (!is.null(dat2Mod1) && !is.null(dat2Mod2)) {
                  cutoff <- getCutoffNonNested(dat2Mod2, dat2Mod1, alpha = alpha, 
                    onetailed = onetailed)[[1]]
                  cutoff3 <- -cutoff[1, ]
                  cutoff4 <- -cutoff[2, ]
                }
            }
        }
    }
    
    if (is.null(x)) {
        if (!is.null(Data1)) {
            plotOverHist(Data2, Data1, cutoff = cutoff1, usedFit = usedFit, cutoff2 = cutoff2, 
                cutoff3 = cutoff3, cutoff4 = cutoff4)
        } else {
            plotCutoff(Data2, cutoff1, usedFit = usedFit, cutoff2 = cutoff2)
        }
    } else if (ncol(x) == 1) {
        if (logistic & (!is.null(Data1) | !is.null(cutoff))) {
            plotLogisticFit(Data2, nullObject = Data1, cutoff = cutoff, usedFit = usedFit, 
                x = x, xval = pred, alpha = alpha, useContour = useContour)
        } else {
            plotScatter(Data2, nullObject = Data1, cutoff = cutoff, usedFit = usedFit, 
                x = x, alpha = alpha)
            # Plot scatterplot if only one continuous; Optional for putting horizontal
            # cutoff If the cutoff exists, the power plot can be used.
        }
    } else if (ncol(x) == 2) {
        if (logistic & (!is.null(Data1) | !is.null(cutoff))) {
            plotLogisticFit(Data2, nullObject = Data1, cutoff = cutoff, usedFit = usedFit, 
                x = x, xval = pred, alpha = alpha, useContour = useContour)
        } else {
            stop("Cannot make scatter plot with two or more varying variables")
        }
        # If the cutoff exists, the power 2/3D plot can be used.
    } else {
        stop("The varying parameter used cannot be over two dimensions.")
    }
} 
