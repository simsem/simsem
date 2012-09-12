## Functon to impute missing data, run Lavaan on each one


## Currently outputs a list of parameter estimates, standard errors, fit indices and fraction missing information

## TO DO: Get names for each element from the lavaan object

runMI <- function(data.mat, data.model, m, miPackage = "amelia", silent = FALSE, opts = list()) {
    ################### I put the silent argument here as the 'runRep' and 'simResult' have one.
    data.model@auxiliary <- new("NullVector")
    # Currently only supports imputation by Amelia. We want to add mice, and maybe EM imputatin too...
    if (!miPackage == "amelia") 
        stop("Currently runMI only supports imputation by amelia")
    # Impute missing data no longer creates two copies of imputed data
	
	miFun <- list()
	if(miPackage == "amelia") {
		require(Amelia)
		miFun[[1]] <- amelia
	} else if (miPackage == "mice") {
		stop("This package has not implemented the mice package yet.")
		#miFun[[1]] <- mice
	} else {
		stop("Cannot recognize the package name used for multiple imputation")
	}
    miFun[[2]] <- data.mat
	miFun[[3]] <- m
    optsLength <- length(opts)
	if(optsLength > 0) {
		for (i in 1:optsLength) {
			miFun[[i + 3]] <- opts[[i]]
		}
		names(miFun) <- c("", "", "", names(opts))
	}
    temp.am <- eval(as.call(miFun))
	
	
	
    # nRep <- m
    args <- opts
    ## Return list of simModelOut objects, to be combined.
    runSimMI <- function(MIdata, simModel) {
        model <- run(object = simModel, data = MIdata)
        return(model)
    }
    imputed.l <- lapply(temp.am$imputations, function(data, var) {
        return(data[, var])
    }, var = data.model@indLab)
    # Run models on each imputed data set using simModel
    if (class(data.model) == "SimModel") {
        imputed.results.l <- lapply(imputed.l, runSimMI, data.model)
    }
    # Run models on each imputed data set using lavaan syntax Can we switch to simSEM framework?
    if (is.character(data.model)) {
        # Function to run lavaan using lapply; inputs: raw data, syntax; Output: list of parameter estimates, se and fit from each model
        
        runlavaanMI <- function(MIdata, syntax) {
            fit <- cfa(syntax, data = MIdata)
            FitIndices <- extractLavaanFit(fit)
            coef <- inspect(fit, "coef")
            se <- inspect(fit, "se")
            Converged = TRUE
            if (sum(unlist(lapply(inspect(fit, "se"), sum))) == 0) 
                Converged = FALSE
            return(new("SimModelOut", package = "lavaan", coef = coef, fit = FitIndices, se = se, converged = Converged))
        }
        
        imputed.results.l <- lapply(imputed.l, runlavaanMI, data.model)
    }
    
    ## New miPool should return simResult object. Can be used with runRep runSIM or can be summarized.
    comb.results <- miPool(imputed.results.l)
    
    return(comb.results)
    
}


testMI <- function() {
    ## Shamelessly using the example in lavaan
    test <- HolzingerSwineford1939[, -5]
    HS.model <- " visual  =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed   =~ x7 + x8 + x9 "
    cfa(HS.model, data = test)
    ## Impose missing data to test
    log.mat1 <- makeMCAR(dim(test), 0.3, )
    test[log.mat1] <- NA
    runMI(test, HS.model, 3)
}
 
