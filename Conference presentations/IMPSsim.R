
allFun <- '

runSim <- function(cond) {

N <- unlist(cond[1])
nRep <- unlist(cond[2])
mis <- unlist(cond[4])
seed <- unlist(cond[3])
set.seed(seed)

trivialMis <- -0.1
# N <- 200
# nRep <- 100
# mis <- -0.7

loading <- matrix(0, 9, 3)
loading[1, 1] <- 1
loading[2:3, 1] <- NA
loading[4, 2] <- 1
loading[5:6, 2] <- NA
loading[7, 3] <- 1
loading[8:9, 3] <- NA
LY <- simMatrix(loading, 1)

facCor <- matrix(NA, 3, 3)
diag(facCor) <- 1
facCorVal <- diag(3)
facCorVal[1, 2] <- facCorVal[2, 1] <- 0.7
facCorVal[2, 3] <- facCorVal[3, 2] <- 0.7
facCorVal[1, 3] <- facCorVal[3, 1] <- 0.49
RPS <- symMatrix(facCor, facCorVal)

VE <- simVector(rep(NA, 3), c(1, 1.2, 1.4))

error <- diag(9)
error[1, 4] <- error[4, 7] <- error[4, 1] <- error[7, 4] <- NA
error[2, 5] <- error[5, 8] <- error[5, 2] <- error[8, 5] <- NA
error[3, 6] <- error[6, 9] <- error[6, 3] <- error[9, 6] <- NA
error[1, 7] <- error[7, 1] <- NA
error[2, 8] <- error[8, 2] <- NA
error[3, 9] <- error[9, 3] <- NA
errorVal <- diag(9)
errorVal[1, 4] <- errorVal[4, 7] <- errorVal[4, 1] <- errorVal[7, 4] <- 0.2
errorVal[2, 5] <- errorVal[5, 8] <- errorVal[5, 2] <- errorVal[8, 5] <- 0.2
errorVal[3, 6] <- errorVal[6, 9] <- errorVal[6, 3] <- errorVal[9, 6] <- 0.2
errorVal[1, 7] <- errorVal[7, 1] <- 0.04
errorVal[2, 8] <- errorVal[8, 2] <- 0.04
errorVal[3, 9] <- errorVal[9, 3] <- 0.04
RTE <- symMatrix(error, errorVal)

VTE <- simVector(rep(NA, 9), 0.4)

longCFA <- simSetCFA(LY=LY, RPS=RPS, VE=VE, RTE=RTE, VTE=VTE)

con1 <- matrix(0, 3, 2)
con1[1,] <- c(2, 1)
con1[2,] <- c(5, 2)
con1[3,] <- c(8, 3)
rownames(con1) <- rep("LY", 3)
con2 <- matrix(0, 3, 2)
con2[1,] <- c(3, 1)
con2[2,] <- c(6, 2)
con2[3,] <- c(9, 3)
rownames(con2) <- rep("LY", 3)
equalCon <- simEqualCon(con1, con2, modelType="CFA")

loadingMis <- matrix(0, 9, 3)
loadingMis[8:9, 3] <- NA
LYMis <- simMatrix(loadingMis, mis) 

longCFAMis <- simMisspecCFA(LY=LYMis)

dataTemplate <- simData(longCFA, N, misspec=longCFAMis, equalCon=equalCon)

analyzeNested <- simModel(longCFA, equalCon=equalCon)
analyzeParent <- simModel(longCFA)

dat <- run(dataTemplate)

outNested <- run(analyzeNested, dat)
outParent <- run(analyzeParent, dat)

converged <- c(
outNested@converged,
outParent@converged)

################################################## Misspecification #####################

# Trivial misspecification

misspec1 <- new("NullSimMisspec")

loadingMis2 <- matrix(0, 9, 3)
loadingMis2[8:9, 3] <- NA
LYMis2 <- simMatrix(loadingMis2, trivialMis)
misspec2 <- simMisspecCFA(LY=LYMis2)

unifMis <- simUnif(-abs(trivialMis), abs(trivialMis))
loadingMis3 <- matrix(0, 9, 3)
loadingMis3[2:3, 1] <- NA
loadingMis3[5:6, 2] <- NA
loadingMis3[8:9, 3] <- NA
LYMis3 <- simMatrix(loadingMis3, toFunction(unifMis))
misspec3 <- simMisspecCFA(LY=LYMis3)

loadingMis4 <- matrix(0, 9, 3)
loadingMis4[2:3, 1] <- NA
loadingMis4[5:6, 2] <- NA
loadingMis4[8:9, 3] <- NA
LYMis4 <- simMatrix(loadingMis4, toFunction(unifMis))
misspec4 <- simMisspecCFA(LY=LYMis4, optMisfit="max", numIter=100, misBeforeFill=FALSE)

misList <- list(misspec1, misspec2, misspec3, misspec4)

simOutNested <- NULL
simOutParent <- NULL

try(simOutNested <- lapply(misList, runFit, model=analyzeNested, data=dat, nRep=nRep, seed=seed, analyzeModel=analyzeNested))
try(simOutParent <- lapply(misList, runFit, model=analyzeNested, data=dat, nRep=nRep, seed=seed, analyzeModel=analyzeParent))

chiDiff <- anova(outNested, outParent)$diff[2, 1:3]
CFIDiff <- anova(outNested, outParent)$diff[2, 4]
pMis1 <- pValueNested(outNested, outParent, simOutNested[[1]], simOutParent[[1]])
pMis2 <- pValueNested(outNested, outParent, simOutNested[[2]], simOutParent[[2]])
pMis3 <- pValueNested(outNested, outParent, simOutNested[[3]], simOutParent[[3]])
pMis4 <- pValueNested(outNested, outParent, simOutNested[[4]], simOutParent[[4]])

names(chiDiff) <- c("chi", "df", "p")
names(CFIDiff) <- c("CFI.diff")
names(pMis1) <- paste("mis.1.", c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR", "andRule", "orRule"), sep="")
names(pMis2) <- paste("mis.2.", c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR", "andRule", "orRule"), sep="")
names(pMis3) <- paste("mis.3.", c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR", "andRule", "orRule"), sep="")
names(pMis4) <- paste("mis.4.", c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR", "andRule", "orRule"), sep="")

result <- unlist(c(chiDiff, CFIDiff, pMis1, pMis2, pMis3, pMis4))

return(result)
} else {
return(NA)
}
}

m <- subSeed
set.seed(m)
seedList <- sample(1:999999, 1)
n <- c(125, 250, 500, 1000) #125, 250, 500, and 1000
mis <- c(1, 0.9, 0.4) #0, 0.3, 0.7
nRep <- 1000

conds <- expand.grid(n, mis, nRep, seedList)

output <- matrix(NA, nrow(conds), 40)

for(i in 1:nrow(conds)) {
	output[i,] <- runSim(conds[i,])
}

dput(output, paste("output", subNum, ".Rdata", sep=""))
'

sh <- '

# PBS -N subSim
# PBS -l nodes=1:ppn=1
# PBS -l walltime=480:00:00

cd $PBS_O_WORKDIR
orterun --hostfile $PBS_NODEFILE -n 1 R --no-save --vanilla -f subSim.R
'

nRep <- 1000
set.seed(123321)
seedList <- sample(1:999999, nRep)

for(i in 1:nRep) {
temp <- gsub("subNum", i, allFUN)
temp2 <- gsub("subSeed", seedList[i], temp)
write(temp2, paste("sim", i, ".R", sep=""))
temp3 <- gsub("subSim", paste("sim", i, sep=""), sh)
write(temp3, paste("sim", i, ".sh", sep=""))
}
