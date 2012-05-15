#library(simsem)

sourceDir <- function(path, trace = TRUE, ...) {
     for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
		if(nm != "AllClass.R" & nm != "AllGenerics.R") {
        if(trace) cat(nm,":") 
        source(file.path(path, nm), ...)
        if(trace) cat("\n")
		}
     }
}

#dir <- "C:/Users/Sunthud/Desktop/My Dropbox/simsem/simsem/R/"
#dir <- "C:/Users/Sunthud/simsem_backup/simsem/R/"
dir <- "C:/Users/student/Dropbox/simsem/simsem/R/"
 source(paste(dir, "AllClass.R", sep=""))
 source(paste(dir, "AllGenerics.R", sep=""))
 sourceDir(dir)

################################ Population Model ##########################

# Writing the Documentation
# Change the simResult to provide only one parameter set if param is not random!


runSim <- function(n, mis, nRep, seed) {
#seed <- 123321
#n <- 125
#mis <- 0.7
#nRep <- 50
trivialMis <- 0.3
set.seed(seed)
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
LY <- simMatrix(loading, 0.7)

facCor <- matrix(NA, 3, 3)
diag(facCor) <- 1
RPS <- symMatrix(facCor, 0.3)

RTE <- symMatrix(diag(9))

VTE <- simVector(rep(NA, 9), 0.51)

modelPop <- simSetCFA(LY=LY, RPS=RPS, RTE=RTE, VTE=VTE)

loadingMisPop <- matrix(0, 9, 3)
loadingMisPop[1, 2] <- NA
loadingMisPop[4, 3] <- NA
LYMisPop <- simMatrix(loadingMisPop, mis)
misPop <- simMisspecCFA(LY=LYMisPop)

modelData <- simData(modelPop, n, misspec=misPop)
dat <- run(modelData)

############################## Analysis Part ###########################################

# Analysis model
loadingParam <- matrix(0, 9, 3)
loadingParam[1:3, 1] <- NA
loadingParam[4:6, 2] <- NA
loadingParam[7:9, 3] <- NA
paramModel <- simParamCFA(LY=loadingParam)
analyzeModel <- simModel(paramModel)
out <- run(analyzeModel, dat)

# Misspecification
# 1) no misspecification
# 2) fixed method with accurate one
# 3) fixed method with inaccurate one
# 4) random method with uniform dist
# 5) random method with normal dist
# 6) maximal method

misspec1 <- new("NullSimMisspec")

loadingMis2 <- matrix(0, 9, 3)
loadingMis2[1,2] <- NA
loadingMis2[4,3] <- NA
LYMis2 <- simMatrix(loadingMis2, trivialMis)
misspec2 <- simMisspecCFA(LY=LYMis2, misBeforeFill=FALSE)

loadingMis3 <- matrix(0, 9, 3)
loadingMis3[6,1] <- NA
loadingMis3[9,2] <- NA
LYMis3 <- simMatrix(loadingMis3, trivialMis)
misspec3 <- simMisspecCFA(LY=LYMis3, misBeforeFill=FALSE)

u3 <- simUnif(-trivialMis, trivialMis)
loadingMis4 <- matrix(0, 9, 3)
loadingMis4[4:9, 1] <- NA
loadingMis4[c(1:3, 7:9),2] <- NA
loadingMis4[1:6,3] <- NA
LYMis4 <- simMatrix(loadingMis4, "u3")
misspec4 <- simMisspecCFA(LY=LYMis4, misBeforeFill=FALSE)

n3 <- simNorm(0, trivialMis/2)
loadingMis5 <- matrix(0, 9, 3)
loadingMis5[4:9, 1] <- NA
loadingMis5[c(1:3, 7:9),2] <- NA
loadingMis5[1:6,3] <- NA
LYMis5 <- simMatrix(loadingMis5, "n3")
misspec5 <- simMisspecCFA(LY=LYMis5, misBeforeFill=FALSE)

u3 <- simUnif(-trivialMis, trivialMis)
loadingMis6 <- matrix(0, 9, 3)
loadingMis6[4:9, 1] <- NA
loadingMis6[c(1:3, 7:9),2] <- NA
loadingMis6[1:6,3] <- NA
LYMis6 <- simMatrix(loadingMis6, "u3")
misspec6 <- simMisspecCFA(LY=LYMis6, optMisfit="max", numIter=100, misBeforeFill=FALSE)

misList <- list(misspec1, misspec2, misspec3, misspec4, misspec5, misspec6)

simOut <- lapply(misList, runFit, model=analyzeModel, realdata=dat, nRep=nRep, seed=seed)
outP <- sapply(simOut, pValue, target=out)
name <- expand.grid(rownames(outP), 1:6)
result <- as.vector(outP)
names(result) <- paste(name[,1], name[,2], sep="")
return(result)
}

