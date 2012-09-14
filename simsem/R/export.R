##Functions to export raw data for analysis with other programs
#Currently only will be set to export to LISREL and Mplus (how to use others?)

export <- function(nRep, model, n, program = "Mplus", fileStem = "simData", miss = NULL, datafun = NULL, 
    pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, 
    sequential = FALSE, modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0", 
    misfitBounds = NULL, averageNumMisspec = NULL, optMisfit = NULL, optDraws = 50, 
    seed = 123321, silent = FALSE, multicore = FALSE, numProc = NULL, missValue = -999) {
	
	#Generate data into a list - Does sim do this? If not we need to use generate...
	dat <- sim(nRep = nRep, model = model, n = n, miss = miss, datafun = datafun, 
    pmMCAR = pmMCAR, pmMAR = pmMAR, facDist = facDist, indDist = indDist, errorDist = errorDist, 
    sequential = sequential, modelBoot = modelBoot, realData = realData, maxDraw = maxDraw, misfitType = misfitType, 
    misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, optMisfit = optMisfit, optDraws = optDraws, 
    seed = seed, silent = silent, multicore = multicore, numProc = numProc, dataOnly=TRUE)
	
	
	#Turn NA into -999
	if (!is.null(miss)){
	#Function to be applied across the list
	#input datF: data frame
	#output data frame with NAs as -999 
	missCode <- function(datF){
	logF <- is.na(datF)
	datF[logF] <- missValue
	return(datF)	
	}
	dat <- lapply(dat, missCode)
	}
	
	if(program == "Mplus") {
	
	#Function to write each dataset to a .dat file
	#input: number for file, file name stem (character object)
	writeMplus <- function (num, filename){
	
	write.table(dat[[num]], paste(filename, num, ".dat", sep = ""), row.names=FALSE, col.names=FALSE)
	}
	reps <- 1:length(dat)
	#write data files
	lapply(reps, writeMplus, fileStem)
	#write toplevel file with names in it
	dats <- paste(fileStem, reps, ".dat", sep = "")
	write.table(dats, paste(fileStem, ".dat", sep = ""), row.names=FALSE, col.names=FALSE, quote=FALSE)
	
	}
	if(program == "LISREL" | program == "lisrel") {
	
	datL <- do.call(rbind, dat)
	write.table(datL, paste(fileStem, ".dat", sep = ""), row.names=FALSE, col.names=TRUE)
	}
	
	
	
	
	
	
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	}