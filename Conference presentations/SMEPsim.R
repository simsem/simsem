# model1
# model2
# model3
# model4
allFun <- '

runSim <- function(cond) {
library(simsem)

seed <- unlist(cond[3])
N <- unlist(cond[1])
nRep <- unlist(cond[2])
set.seed(seed)

# Model A
pathA <- matrix(0, 9, 9)
pathA[3, 1:2] <- NA
pathA[4:9, 3] <- NA
BEA <- simMatrix(pathA, 0.2)
VPSA <- simVector(rep(NA, 9), c(1, 1, rep(0.8, 7)))
facCorA <- diag(1, 9)
facCorA[1, 2] <- facCorA[2, 1] <- NA
RPSA <- symMatrix(facCorA, 0.2)
AL <- simVector(rep(0, 9))
LY <- simMatrix(diag(9))
TE <- symMatrix(matrix(0, 9, 9))
TY <- simVector(rep(NA, 9), 0)
modelA <- simSetSEM(BE=BEA, VPS=VPSA, RPS=RPSA, LY=LY, TE=TE, AL=AL, TY=TY)

pathB <- matrix(0, 9, 9)
pathB[2, 1] <- NA
pathB[3, 2] <- NA
pathB[4, 2:3] <- NA
pathB[5, 3] <- NA
pathB[6, 2:3] <- NA
pathB[7, c(3, 4)] <- NA
pathB[8, 5] <- NA
pathB[9, c(3, 6)] <- NA
BEB <- simMatrix(pathB, 0.2)
VPSB <- simVector(rep(NA, 9), c(1, rep(0.8, 8)))
facCorB <- diag(9)
facCorB[7, 8] <- facCorB[8, 7] <- NA
facCorB[7, 9] <- facCorB[9, 7] <- NA
facCorB[8, 9] <- facCorB[9, 8] <- NA
RPSB <- symMatrix(facCorB, 0.2)
modelB <- simSetSEM(BE=BEB, VPS=VPSB, RPS=RPSB, LY=LY, TE=TE, AL=AL, TY=TY)

pathC <- matrix(0, 9, 9)
pathC[6, 3] <- NA
pathC[7, 4] <- NA
pathC[8, 5] <- NA
pathC[9, 6:8] <- NA
BEC <- simMatrix(pathC, 0.2)
VPSC <- simVector(rep(NA, 9), c(rep(1, 5), rep(0.8, 4)))
subFacCorC <- matrix(NA, 5, 5)
diag(subFacCorC) <- 1
facCorC <- diag(1, 9)
facCorC[1:5, 1:5] <- subFacCorC
RPSC <- symMatrix(facCorC, 0.2)
modelC <- simSetSEM(BE=BEC, VPS=VPSC, RPS=RPSC, LY=LY, TE=TE, AL=AL, TY=TY)

pathD <- matrix(0, 9, 9)
pathD[2:4, 1] <- NA
pathD[5:7, 2] <- NA
pathD[7:8, 3] <- NA
pathD[8:9, 4] <- NA
BED <- simMatrix(pathD, 0.2)
VPSD <- simVector(rep(NA, 9), c(1, rep(0.8, 8)))
RPSD <- symMatrix(diag(9))
modelD <- simSetSEM(BE=BED, VPS=VPSD, RPS=RPSD, LY=LY, TE=TE, AL=AL, TY=TY)

dataA <- simData(modelA, N)
dataB <- simData(modelB, N)
dataC <- simData(modelC, N)
dataD <- simData(modelD, N)

analyzeA <- simModel(modelA)
analyzeB <- simModel(modelB)
analyzeC <- simModel(modelC)
analyzeD <- simModel(modelD)

datA <- run(dataA)
datB <- run(dataB)
datC <- run(dataC)
datD <- run(dataD)

outAA <- run(analyzeA, datA)
outAB <- run(analyzeB, datA)
outAC <- run(analyzeC, datA)
outAD <- run(analyzeD, datA)
outBA <- run(analyzeA, datB)
outBB <- run(analyzeB, datB)
outBC <- run(analyzeC, datB)
outBD <- run(analyzeD, datB)
outCA <- run(analyzeA, datC)
outCB <- run(analyzeB, datC)
outCC <- run(analyzeC, datC)
outCD <- run(analyzeD, datC)
outDA <- run(analyzeA, datD)
outDB <- run(analyzeB, datD)
outDC <- run(analyzeC, datD)
outDD <- run(analyzeD, datD)

converged <- c(
outAA@converged,
outAB@converged, 
outAC@converged, 
outAD@converged, 
outBA@converged, 
outBB@converged, 
outBC@converged, 
outBD@converged, 
outCA@converged, 
outCB@converged, 
outCC@converged, 
outCD@converged, 
outDA@converged, 
outDB@converged, 
outDC@converged, 
outDD@converged)

if(all(converged)) {
simAA <- runFit(analyzeA, nRep=nRep, data=datA, analyzeModel=analyzeA)
simAB <- runFit(analyzeA, nRep=nRep, data=datA, analyzeModel=analyzeB)
simAC <- runFit(analyzeA, nRep=nRep, data=datA, analyzeModel=analyzeC)
simAD <- runFit(analyzeA, nRep=nRep, data=datA, analyzeModel=analyzeD)
simBA <- runFit(analyzeB, nRep=nRep, data=datB, analyzeModel=analyzeA)
simBB <- runFit(analyzeB, nRep=nRep, data=datB, analyzeModel=analyzeB)
simBC <- runFit(analyzeB, nRep=nRep, data=datB, analyzeModel=analyzeC)
simBD <- runFit(analyzeB, nRep=nRep, data=datB, analyzeModel=analyzeD)
simCA <- runFit(analyzeC, nRep=nRep, data=datC, analyzeModel=analyzeA)
simCB <- runFit(analyzeC, nRep=nRep, data=datC, analyzeModel=analyzeB)
simCC <- runFit(analyzeC, nRep=nRep, data=datC, analyzeModel=analyzeC)
simCD <- runFit(analyzeC, nRep=nRep, data=datC, analyzeModel=analyzeD)
simDA <- runFit(analyzeD, nRep=nRep, data=datD, analyzeModel=analyzeA)
simDB <- runFit(analyzeD, nRep=nRep, data=datD, analyzeModel=analyzeB)
simDC <- runFit(analyzeD, nRep=nRep, data=datD, analyzeModel=analyzeC)
simDD <- runFit(analyzeD, nRep=nRep, data=datD, analyzeModel=analyzeD)



########## ANOVA

anova.a.ab <- anova(outAA, outAB)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.a.ac <- anova(outAA, outAC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.a.ad <- anova(outAA, outAD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.a.bc <- anova(outAB, outAC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.a.bd <- anova(outAB, outAD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.a.cd <- anova(outAC, outAD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.a <- c(anova.a.ab, anova.a.ac, anova.a.ad, anova.a.bc, anova.a.bd, anova.a.cd)
anova.a <- unlist(anova.a[c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)])
name <- expand.grid(c("ab", "ac", "ad", "bc", "bd", "cd"), c("AIC.a.", "BIC.a."))
names(anova.a) <- paste(name[,2], name[,1], sep="")

anova.b.ab <- anova(outBA, outBB)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.b.ac <- anova(outBA, outBC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.b.ad <- anova(outBA, outBD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.b.bc <- anova(outBB, outBC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.b.bd <- anova(outBB, outBD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.b.cd <- anova(outBC, outBD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.b <- c(anova.b.ab, anova.b.ac, anova.b.ad, anova.b.bc, anova.b.bd, anova.b.cd)
anova.b <- unlist(anova.b[c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)])
name <- expand.grid(c("ab", "ac", "ad", "bc", "bd", "cd"), c("AIC.b.", "BIC.b."))
names(anova.b) <- paste(name[,2], name[,1], sep="")

anova.c.ab <- anova(outCA, outCB)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.c.ac <- anova(outCA, outCC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.c.ad <- anova(outCA, outCD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.c.bc <- anova(outCB, outCC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.c.bd <- anova(outCB, outCD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.c.cd <- anova(outCC, outCD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.c <- c(anova.c.ab, anova.c.ac, anova.c.ad, anova.c.bc, anova.c.bd, anova.c.cd)
anova.c <- unlist(anova.c[c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)])
name <- expand.grid(c("ab", "ac", "ad", "bc", "bd", "cd"), c("AIC.c.", "BIC.c."))
names(anova.c) <- paste(name[,2], name[,1], sep="")

anova.d.ab <- anova(outDA, outDB)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.ac <- anova(outDA, outDC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.ad <- anova(outDA, outDD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.bc <- anova(outDB, outDC)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.bd <- anova(outDB, outDD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d.cd <- anova(outDC, outDD)[[2]][2, c("AIC.diff", "BIC.diff")]
anova.d <- c(anova.d.ab, anova.d.ac, anova.d.ad, anova.d.bc, anova.d.bd, anova.d.cd)
anova.d <- unlist(anova.d[c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)])
name <- expand.grid(c("ab", "ac", "ad", "bc", "bd", "cd"), c("AIC.d.", "BIC.d."))
names(anova.d) <- paste(name[,2], name[,1], sep="")

anova.vec <- c(anova.a, anova.b, anova.c, anova.d)

########## Monte Carlo Difference

mc.a.ab <- sapply(pValueNonNested(outAA, outAB, simAA, simAB, simBA, simBB), function(x) x[c("AIC")])
mc.a.ac <- sapply(pValueNonNested(outAA, outAC, simAA, simAC, simCA, simCC), function(x) x[c("AIC")])
mc.a.ad <- sapply(pValueNonNested(outAA, outAD, simAA, simAD, simDA, simDD), function(x) x[c("AIC")])
mc.a.bc <- sapply(pValueNonNested(outAB, outAC, simBB, simBC, simCB, simCC), function(x) x[c("AIC")])
mc.a.bd <- sapply(pValueNonNested(outAB, outAD, simBB, simBD, simDB, simDD), function(x) x[c("AIC")])
mc.a.cd <- sapply(pValueNonNested(outAC, outAD, simCC, simCD, simDC, simDD), function(x) x[c("AIC")])
mc.a <- c(mc.a.ab, mc.a.ac, mc.a.ad, mc.a.bc, mc.a.bd, mc.a.cd)
name <- expand.grid(c("mc1.a.", "mc2.a."), c("ab", "ac", "ad", "bc", "bd", "cd"))
names(mc.a) <- paste(name[,1], name[,2], sep="")

mc.b.ab <- sapply(pValueNonNested(outBA, outBB, simAA, simAB, simBA, simBB), function(x) x[c("AIC")])
mc.b.ac <- sapply(pValueNonNested(outBA, outBC, simAA, simAC, simCA, simCC), function(x) x[c("AIC")])
mc.b.ad <- sapply(pValueNonNested(outBA, outBD, simAA, simAD, simDA, simDD), function(x) x[c("AIC")])
mc.b.bc <- sapply(pValueNonNested(outBB, outBC, simBB, simBC, simCB, simCC), function(x) x[c("AIC")])
mc.b.bd <- sapply(pValueNonNested(outBB, outBD, simBB, simBD, simDB, simDD), function(x) x[c("AIC")])
mc.b.cd <- sapply(pValueNonNested(outBC, outBD, simCC, simCD, simDC, simDD), function(x) x[c("AIC")])
mc.b <- c(mc.b.ab, mc.b.ac, mc.b.ad, mc.b.bc, mc.b.bd, mc.b.cd)
name <- expand.grid(c("mc1.b.", "mc2.b."), c("ab", "ac", "ad", "bc", "bd", "cd"))
names(mc.b) <- paste(name[,1], name[,2], sep="")

mc.c.ab <- sapply(pValueNonNested(outCA, outCB, simAA, simAB, simBA, simBB), function(x) x[c("AIC")])
mc.c.ac <- sapply(pValueNonNested(outCA, outCC, simAA, simAC, simCA, simCC), function(x) x[c("AIC")])
mc.c.ad <- sapply(pValueNonNested(outCA, outCD, simAA, simAD, simDA, simDD), function(x) x[c("AIC")])
mc.c.bc <- sapply(pValueNonNested(outCB, outCC, simBB, simBC, simCB, simCC), function(x) x[c("AIC")])
mc.c.bd <- sapply(pValueNonNested(outCB, outCD, simBB, simBD, simDB, simDD), function(x) x[c("AIC")])
mc.c.cd <- sapply(pValueNonNested(outCC, outCD, simCC, simCD, simDC, simDD), function(x) x[c("AIC")])
mc.c <- c(mc.c.ab, mc.c.ac, mc.c.ad, mc.c.bc, mc.c.bd, mc.c.cd)
name <- expand.grid(c("mc1.c.", "mc2.c."), c("ab", "ac", "ad", "bc", "bd", "cd"))
names(mc.c) <- paste(name[,1], name[,2], sep="")

mc.d.ab <- sapply(pValueNonNested(outDA, outDB, simAA, simAB, simBA, simBB), function(x) x[c("AIC")])
mc.d.ac <- sapply(pValueNonNested(outDA, outDC, simAA, simAC, simCA, simCC), function(x) x[c("AIC")])
mc.d.ad <- sapply(pValueNonNested(outDA, outDD, simAA, simAD, simDA, simDD), function(x) x[c("AIC")])
mc.d.bc <- sapply(pValueNonNested(outDB, outDC, simBB, simBC, simCB, simCC), function(x) x[c("AIC")])
mc.d.bd <- sapply(pValueNonNested(outDB, outDD, simBB, simBD, simDB, simDD), function(x) x[c("AIC")])
mc.d.cd <- sapply(pValueNonNested(outDC, outDD, simCC, simCD, simDC, simDD), function(x) x[c("AIC")])
mc.d <- c(mc.d.ab, mc.d.ac, mc.d.ad, mc.d.bc, mc.d.bd, mc.d.cd)
name <- expand.grid(c("mc1.d.", "mc2.d."), c("ab", "ac", "ad", "bc", "bd", "cd"))
names(mc.d) <- paste(name[,1], name[,2], sep="")

mc.vec <- c(mc.a, mc.b, mc.c, mc.d)

########## Monte Carlo Difference

lik.a.ab <- likRatioFit(outAA, outAB, simAA, simAB, simBA, simBB)[c("AIC")]
lik.a.ac <- likRatioFit(outAA, outAC, simAA, simAC, simCA, simCC)[c("AIC")]
lik.a.ad <- likRatioFit(outAA, outAD, simAA, simAD, simDA, simDD)[c("AIC")]
lik.a.bc <- likRatioFit(outAB, outAC, simBB, simBC, simCB, simCC)[c("AIC")]
lik.a.bd <- likRatioFit(outAB, outAD, simBB, simBD, simDB, simDD)[c("AIC")]
lik.a.cd <- likRatioFit(outAC, outAD, simCC, simCD, simDC, simDD)[c("AIC")]
lik.a <- c(lik.a.ab, lik.a.ac, lik.a.ad, lik.a.bc, lik.a.bd, lik.a.cd)
names(lik.a) <- paste("lik.a.", c("ab", "ac", "ad", "bc", "bd", "cd"), sep="")

lik.b.ab <- likRatioFit(outBA, outBB, simAA, simAB, simBA, simBB)[c("AIC")]
lik.b.ac <- likRatioFit(outBA, outBC, simAA, simAC, simCA, simCC)[c("AIC")]
lik.b.ad <- likRatioFit(outBA, outBD, simAA, simAD, simDA, simDD)[c("AIC")]
lik.b.bc <- likRatioFit(outBB, outBC, simBB, simBC, simCB, simCC)[c("AIC")]
lik.b.bd <- likRatioFit(outBB, outBD, simBB, simBD, simDB, simDD)[c("AIC")]
lik.b.cd <- likRatioFit(outBC, outBD, simCC, simCD, simDC, simDD)[c("AIC")]
lik.b <- c(lik.b.ab, lik.b.ac, lik.b.ad, lik.b.bc, lik.b.bd, lik.b.cd)
names(lik.b) <- paste("lik.b.", c("ab", "ac", "ad", "bc", "bd", "cd"), sep="")

lik.c.ab <- likRatioFit(outCA, outCB, simAA, simAB, simBA, simBB)[c("AIC")]
lik.c.ac <- likRatioFit(outCA, outCC, simAA, simAC, simCA, simCC)[c("AIC")]
lik.c.ad <- likRatioFit(outCA, outCD, simAA, simAD, simDA, simDD)[c("AIC")]
lik.c.bc <- likRatioFit(outCB, outCC, simBB, simBC, simCB, simCC)[c("AIC")]
lik.c.bd <- likRatioFit(outCB, outCD, simBB, simBD, simDB, simDD)[c("AIC")]
lik.c.cd <- likRatioFit(outCC, outCD, simCC, simCD, simDC, simDD)[c("AIC")]
lik.c <- c(lik.c.ab, lik.c.ac, lik.c.ad, lik.c.bc, lik.c.bd, lik.c.cd)
names(lik.c) <- paste("lik.c.", c("ab", "ac", "ad", "bc", "bd", "cd"), sep="")

lik.d.ab <- likRatioFit(outDA, outDB, simAA, simAB, simBA, simBB)[c("AIC")]
lik.d.ac <- likRatioFit(outDA, outDC, simAA, simAC, simCA, simCC)[c("AIC")]
lik.d.ad <- likRatioFit(outDA, outDD, simAA, simAD, simDA, simDD)[c("AIC")]
lik.d.bc <- likRatioFit(outDB, outDC, simBB, simBC, simCB, simCC)[c("AIC")]
lik.d.bd <- likRatioFit(outDB, outDD, simBB, simBD, simDB, simDD)[c("AIC")]
lik.d.cd <- likRatioFit(outDC, outDD, simCC, simCD, simDC, simDD)[c("AIC")]
lik.d <- c(lik.d.ab, lik.d.ac, lik.d.ad, lik.d.bc, lik.d.bd, lik.d.cd)
names(lik.d) <- paste("lik.d.", c("ab", "ac", "ad", "bc", "bd", "cd"), sep="")

lik.vec <- c(lik.a, lik.b, lik.c, lik.d)

fit <- NULL
target <- "AIC"
fit <- c(outAA@fit[target], 
outAB@fit[target], 
outAC@fit[target], 
outAD@fit[target], 
outBA@fit[target], 
outBB@fit[target], 
outBC@fit[target], 
outBD@fit[target], 
outCA@fit[target], 
outCB@fit[target], 
outCC@fit[target], 
outCD@fit[target], 
outDA@fit[target], 
outDB@fit[target], 
outDC@fit[target], 
outDD@fit[target])
target <- "BIC"
fit <- c(fit, outAA@fit[target], 
outAB@fit[target], 
outAC@fit[target], 
outAD@fit[target], 
outBA@fit[target], 
outBB@fit[target], 
outBC@fit[target], 
outBD@fit[target], 
outCA@fit[target], 
outCB@fit[target], 
outCC@fit[target], 
outCD@fit[target], 
outDA@fit[target], 
outDB@fit[target], 
outDC@fit[target], 
outDD@fit[target])
name <- expand.grid(c("A", "B", "C", "D"), c("A", "B", "C", "D"), c("AIC", "BIC"))
names(fit) <- paste(name[,3], name[,2], name[,1], sep=".")

result <- c(N=N, fit, anova.vec, mc.vec, lik.vec)
return(result)
} else {
return(NA)
}
}

m <- subSeed
set.seed(m)
seedList <- sample(1:999999, 1)
n <- c(80, 120, 200, 500, 1500, 5000) 
nRep <- 1000

conds <- expand.grid(n, nRep, seedList)

output <- matrix(NA, nrow(conds), 153)

 for(i in 1:nrow(conds)) {
	 output[i,] <- runSim(conds[i,])
 }

dput(output, paste("output", subNum, ".Rdata", sep=""))
'

sh <- '

#PBS -N subSim
#PBS -l nodes=1:ppn=1
#PBS -l walltime=480:00:00

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