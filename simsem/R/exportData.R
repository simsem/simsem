##Export Data function for simsem
##Inputs are inputs for sim function
##Output varies by program, if params =TRUE, returns a list of parameters for each rep too.
##Currently only works for Mplus and lavaan

exportData <- function(nRep, model, n, program = "Mplus", fileStem = "sim", miss = NULL, missCode = -999, datafun=NULL, 
pmMCAR = NULL, pmMAR = NULL, facDist = NULL, indDist = NULL, errorDist = NULL, sequential = FALSE, 
modelBoot = FALSE, realData = NULL, maxDraw = 50, misfitType = "f0", 
misfitBounds = NULL, averageNumMisspec = NULL, optMisfit=NULL, optDraws = 50, 
seed = 123321, silent = FALSE, multicore = FALSE, numProc = NULL,  params = FALSE){

##Steps
#1. Generate data
#2. Write data (make files names needed)
#3. Write any other info


dat <- sim(nRep=nRep, model=model, n=n, program = program, fileStem = fileStem, miss = miss, datafun = datafun, 
pmMCAR = pmMCAR, pmMAR = pmMAR, facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, 
modelBoot = modelBoot, realData = realData, maxDraw = maxDraw, misfitType = misfitType, 
misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, optMisfit=optMisfit, optDraws = optDraws, 
seed = seed, silent = silent, multicore = multicore, numProc = numProc, dataOnly=TRUE)

#Replace missing data with NA with -999
#Function to find missing in each dataset and replace with missing code
   replaceMissing <- function(dataF, misscode) {
        dataF[is.na(dataF)] <- misscode
		dataF
    }
#browser()
    dat <- lapply(dat, replaceMissing, missCode)

if(program == "Mplus" | program == "mplus") {

fileN <- NULL
for(i in 1:length(dat)){
    fileN <- c(fileN, paste(fileStem, i, ".dat", sep=""))
	write.table(dat[[i]], fileN[i] ,sep=" ", row.names=FALSE, col.names=FALSE)
}
#write list of file names
write.table(fileN, paste(fileStem, ".dat", sep=""), row.names=FALSE, col.names=FALSE, quote=FALSE)
}

if(program == "LISREL" | program == "lisrel") {

datStack <- do.call(rbind, dat)
write.table(datStack, paste(fileStem, ".dat", sep=""), row.names=FALSE, col.names=TRUE)
}

##If random paramaters are used, return a list of random paramaters
if(params) {

datP <- sim(nRep=nRep, model=model, n=n, program = program, fileStem = fileStem, miss = miss, datafun = datafun, 
pmMCAR = pmMCAR, pmMAR = pmMAR, facDist = facDist, indDist = indDist, errorDist = errorDist, sequential = sequential, 
modelBoot = modelBoot, realData = realData, maxDraw = maxDraw, misfitType = misfitType, 
misfitBounds = misfitBounds, averageNumMisspec = averageNumMisspec, optMisfit=optMisfit, optDraws = optDraws, 
seed = seed, silent = silent, multicore = multicore, numProc = numProc, paramOnly=TRUE)

datP
}
}