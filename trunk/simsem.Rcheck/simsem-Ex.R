pkgname <- "simsem"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('simsem')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("SimData-class")
### * SimData-class

flush(stderr()); flush(stdout())

### Name: SimData-class
### Title: Class '"SimData"'
### Aliases: SimData-class run,SimData-method summary,SimData-method
### Keywords: classes

### ** Examples

showClass("SimData")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(200, CFA.Model)
summary(SimData)
run(SimData)



cleanEx()
nameEx("SimDataOut-class")
### * SimDataOut-class

flush(stderr()); flush(stdout())

### Name: SimDataOut-class
### Title: Class '"SimDataOut"'
### Aliases: SimDataOut-class summary,SimDataOut-method
### Keywords: classes

### ** Examples

showClass("SimDataOut")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
Data <- run(SimData, dataOnly=FALSE)
Result <- run(SimModel, Data)
summary(Data)



cleanEx()
nameEx("SimEqualCon-class")
### * SimEqualCon-class

flush(stderr()); flush(stdout())

### Name: SimEqualCon-class
### Title: Class '"SimEqualCon"'
### Aliases: SimEqualCon-class summary,SimEqualCon-method
### Keywords: classes

### ** Examples

showClass("SimEqualCon")
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM")
summary(equal.loading)



cleanEx()
nameEx("SimMatrix-class")
### * SimMatrix-class

flush(stderr()); flush(stdout())

### Name: SimMatrix-class
### Title: Class '"SimMatrix"' (Random parameters matrix)
### Aliases: SimMatrix-class run,SimMatrix-method
###   summaryShort,SimMatrix-method summary,SimMatrix-method
### Keywords: classes

### ** Examples

showClass("SimMatrix")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)
run(LX)

n65 <- simNorm(0.6, 0.05)
LY <- simMatrix(loading, "n65")
summary(LY)
run(LY)

u34 <- simUnif(0.3, 0.4)
LY <- adjust(LY, "u34", c(2, 1))
summary(LY)
run(LY)
summaryShort(LY)



cleanEx()
nameEx("SimMissing-class")
### * SimMissing-class

flush(stderr()); flush(stdout())

### Name: SimMissing-class
### Title: Class '"SimMissing"'
### Aliases: SimMissing-class
### Keywords: classes

### ** Examples

# No Example



cleanEx()
nameEx("SimMisspec-class")
### * SimMisspec-class

flush(stderr()); flush(stdout())

### Name: SimMisspec-class
### Title: Class '"SimMisspec"'
### Aliases: SimMisspec-class run,SimMisspec-method
### Keywords: classes

### ** Examples

showClass("SimMisspec")
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(TD=TD.Mis)



cleanEx()
nameEx("SimModel-class")
### * SimModel-class

flush(stderr()); flush(stdout())

### Name: SimModel-class
### Title: Class '"SimModel"'
### Aliases: SimModel-class run,SimModel-method summary,SimModel-method
### Keywords: classes

### ** Examples

showClass("SimModel")
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
SimModel <- simModel(CFA.Model)
summary(SimModel)



cleanEx()
nameEx("SimModelMIOut-class")
### * SimModelMIOut-class

flush(stderr()); flush(stdout())

### Name: SimModelMIOut-class
### Title: Class '"SimModelMIOut"'
### Aliases: SimModelMIOut-class
### Keywords: classes

### ** Examples

showClass("SimModelMIOut")



cleanEx()
nameEx("SimModelOut-class")
### * SimModelOut-class

flush(stderr()); flush(stdout())

### Name: SimModelOut-class
### Title: Class '"SimModelOut"'
### Aliases: SimModelOut-class summary,SimModelOut-method
### Keywords: classes

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
Data <- run(SimData)
Result <- run(SimModel, Data)
summary(Result)



cleanEx()
nameEx("SimNorm-class")
### * SimNorm-class

flush(stderr()); flush(stdout())

### Name: SimNorm-class
### Title: Class "SimNorm"
### Aliases: SimNorm-class run,SimNorm-method summary,SimNorm-method
### Keywords: classes

### ** Examples

showClass("SimNorm")
n2 <- simNorm(0, 0.2)
run(n2)
summary(n2)



cleanEx()
nameEx("SimResult-class")
### * SimResult-class

flush(stderr()); flush(stdout())

### Name: SimResult-class
### Title: Class '"SimResult"'
### Aliases: SimResult-class summary,SimResult-method
### Keywords: classes

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(SimData, SimModel, 50)
summary(Output)
getCutoff(Output, 0.05)



cleanEx()
nameEx("SimSet-class")
### * SimSet-class

flush(stderr()); flush(stdout())

### Name: SimSet-class
### Title: Class '"SimSet"'
### Aliases: SimSet-class run,SimSet-method summary,SimSet-method

### ** Examples

showClass("SimSet")

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)

# Error Correlation Object
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
summary(CFA.Model)
#run(CFA.Model)



cleanEx()
nameEx("SimUnif-class")
### * SimUnif-class

flush(stderr()); flush(stdout())

### Name: SimUnif-class
### Title: Class "SimUnif"
### Aliases: SimUnif-class run,SimUnif-method summary,SimUnif-method
### Keywords: classes

### ** Examples

showClass("SimUnif")
u1 <- simUnif(-0.1, 0.1)
run(u1)
summary(u1)



cleanEx()
nameEx("SimVector-class")
### * SimVector-class

flush(stderr()); flush(stdout())

### Name: SimVector-class
### Title: Class '"SimVector"' (Random parameters vector)
### Aliases: SimVector-class run,SimVector-method
###   summaryShort,SimVector-method summary,SimVector-method
### Keywords: classes

### ** Examples

showClass("SimVector")

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- simVector(factor.mean, factor.mean.starting)
run(AL)
summary(AL)
summaryShort(AL)

n01 <- simNorm(0, 1)
AL <- adjust(AL, "n01", 2)
run(AL)
summary(AL)



cleanEx()
nameEx("SymMatrix-class")
### * SymMatrix-class

flush(stderr()); flush(stdout())

### Name: SymMatrix-class
### Title: Class '"SymMatrix"' (Random parameters symmetric matrix)
### Aliases: SymMatrix-class run,SymMatrix-method summary,SymMatrix-method
### Keywords: classes

### ** Examples

showClass("SymMatrix")

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)

u46 <- simUnif(0.4, 0.6)
PH <- adjust(PH, "u46", c(3,2))
summary(PH)
summaryShort(PH)
run(PH)



cleanEx()
nameEx("VirtualDist-class")
### * VirtualDist-class

flush(stderr()); flush(stdout())

### Name: VirtualDist-class
### Title: Class "VirtualDist"
### Aliases: VirtualDist-class
### Keywords: classes

### ** Examples

showClass("VirtualDist")



cleanEx()
nameEx("adjust")
### * adjust

flush(stderr()); flush(stdout())

### Name: adjust
### Title: Change an element in 'SimMatrix', 'SymMatrix', or 'SimVector'.
### Aliases: adjust adjust-methods adjust,ANY-method
###   adjust,SimMatrix-method adjust,SymMatrix-method
###   adjust,SimVector-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)
summary(LX)
run(LX)

u34 <- simUnif(0.3, 0.4)
LX <- adjust(LX, "u34", c(2, 1))
summary(LX)
run(LX)

LX <- adjust(LX, 0, c(2,1))
LX <- adjust(LX, 0.5, c(2,2), FALSE)
summary(LX)
run(LX)

factor.mean <- rep(NA, 2)
factor.mean.starting <- c(5, 2)
AL <- simVector(factor.mean, factor.mean.starting)
run(AL)
summary(AL)

n01 <- simNorm(0, 1)
AL <- adjust(AL, "n01", 2)
run(AL)
summary(AL)



cleanEx()
nameEx("getCutoff")
### * getCutoff

flush(stderr()); flush(stdout())

### Name: getCutoff
### Title: Find cutoff given a priori alpha level
### Aliases: getCutoff getCutoff-methods getCutoff,data.frame-method
###   getCutoff,matrix-method getCutoff,SimResult-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(200, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(SimData, SimModel, 50)
getCutoff(Output, 0.05)



cleanEx()
nameEx("getPower")
### * getPower

flush(stderr()); flush(stdout())

### Name: getPower
### Title: Find power in rejecting alternative models based on fit indices
###   criteria
### Aliases: getPower getPower-methods getPower,data.frame-method
###   getPower,matrix-method getPower,SimResult-method

### ** Examples

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
PH.NULL <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, PS = PH.NULL, TE = TD)
SimData.NULL <- simData(500, CFA.Model.NULL)
SimModel <- simModel(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- simResult(SimData.NULL, SimModel, 50)
Cut.NULL <- getCutoff(Output.NULL, 0.95)

u79 <- simUnif(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
PH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, PS = PH.ALT, TE = TD)
SimData.ALT <- simData(500, CFA.Model.ALT)
Output.ALT <- simResult(SimData.ALT, SimModel, 50)
getPower(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
getPower(Output.ALT, Rule.of.thumb, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))



cleanEx()
nameEx("imposeMissing")
### * imposeMissing

flush(stderr()); flush(stdout())

### Name: imposeMissing
### Title: Impose MAR, MCAR, and planned missingness on a data set
### Aliases: imposeMissing

### ** Examples

  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)
  datac <- cbind(data,rnorm(10,0,1),rnorm(10,5,5))

  # Imposing Missing with the following arguments produces no missing values
  imposeMissing(data)
  imposeMissing(data,covs=c(1,2))
  imposeMissing(data,pmMCAR=0)
  imposeMissing(data,pmMAR=0)
  imposeMissing(data,nforms=0)

  #Some more usage examples
  imposeMissing(data,covs=c(1,2),pmMCAR=.1)
  imposeMissing(datac,covs=c(20,21),pmMAR=.2)
  imposeMissing(data,nforms=3)
  imposeMissing(data,nforms=3,itemGroups=list(c(1,2,3,4,5),c(6,7,8,9,10),c(11,12,13,14,15),c(16,17,18,19)))
  imposeMissing(datac,covs=c(20,21),nforms=3)
  imposeMissing(data,twoMethod=c(19,.8))
  imposeMissing(datac,covs=c(20,21),pmMCAR=.1,pmMAR=.1,nforms=3)



cleanEx()
nameEx("loadingFromAlpha")
### * loadingFromAlpha

flush(stderr()); flush(stdout())

### Name: loadingFromAlpha
### Title: Find standardized factor loading from coefficient alpha
### Aliases: loadingFromAlpha

### ** Examples

    loadingFromAlpha(0.8, 4)



cleanEx()
nameEx("miPool")
### * miPool

flush(stderr()); flush(stdout())

### Name: miPool
### Title: Function to pool imputed results
### Aliases: miPool

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(imputed.results,imps){

MI.param<-matrix(NA,nrow=length(imputed.results),ncol=length(imputed.results[[1]]@Estimates))
MI.se<-matrix(NA,nrow=length(imputed.results),ncol=length(imputed.results[[1]]@SE))
MI.fit<-matrix(NA,nrow=length(imputed.results),ncol=length(imputed.results[[1]]@Fit))

for(i in 1:length(imputed.results)){
MI.param[i,]<-unlist(imputed.results[[i]]@Estimates)
MI.se[i,]<-unlist(imputed.results[[i]]@SE)
MI.fit[i,]<-unlist(imputed.results[[i]]@Fit)
  }

#Need to remove columns representing fixed parameters
MI.param <- MI.param[ , colMeans( MI.param==0 ) == 0, drop=FALSE ]
MI.param <- MI.param[ , colMeans( MI.param==1 ) == 0, drop=FALSE ]
MI.se <- MI.se[ , colSums( MI.se==0 ) == 0, drop=FALSE ]

#compute parameter estimates
Estimates <- colMeans(MI.param)

#compute between-imputation variance: variance of parameter estimates
Bm <- apply(MI.param,2,var)



#compute within-imputation variance: average of squared estimated SEs 
#Um <- colSums(MI.se^2/m)
Um <- apply(MI.se^2,2,mean)

#Total variance
#Tm <- Um + (Bm)*((1+m)/m+1)

#compute total variance: sum of between- and within- variance with correction
SE <- Um + ((imps+1)/imps)*Bm

#compute correction factor for fraction of missing info
nu <- (imps-1)*((((1+1/imps)*Bm)/SE)^-2)

#compute 2 estimates of fraction of missing information
FMI.1 <- 1-(Um/SE)
FMI.2 <- 1- ((nu+1)*Um)/((nu+3)*SE)
FMI<-rbind(FMI.1,FMI.2)

#compute average fit index estimates (only some of these will be interpretable!)
Fit.indices <- colMeans(MI.fit)

MI.res<-list(Estimates,SE,Fit.indices,FMI.1,FMI.2)
names(MI.res)<-c('Estimates','SE','Fit.indices','FMI.1','FMI.2')
#compute chi-square proportion (is this useful?)
#(MI.fit.mat$chisq.p is a placeholder for however we'll index the p-value of chi square)
#chisq <- sum(MI.fit.mat$chisq.pval<.05)/m
return(MI.res)
  }



cleanEx()
nameEx("plotCutoff")
### * plotCutoff

flush(stderr()); flush(stdout())

### Name: plotCutoff
### Title: Plot sampling distributions of fit indices
### Aliases: plotCutoff plotCutoff-methods plotCutoff,data.frame-method
###   plotCutoff,SimResult-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(200, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(SimData, SimModel, 50)
plotCutoff(Output, 0.05, usedFit=c("RMSEA", "SRMR", "CFI", "TLI"))



cleanEx()
nameEx("plotPower")
### * plotPower

flush(stderr()); flush(stdout())

### Name: plotPower
### Title: Plot sampling distributions of fit indices that visualize power
### Aliases: plotPower plotPower-methods
###   plotPower,data.frame,data.frame-method
###   plotPower,data.frame,vector-method
###   plotPower,SimResult,SimResult-method
###   plotPower,SimResult,vector-method

### ** Examples

loading.null <- matrix(0, 6, 1)
loading.null[1:6, 1] <- NA
LX.NULL <- simMatrix(loading.null, 0.7)
PH.NULL <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model.NULL <- simSetCFA(LY = LX.NULL, PS = PH.NULL, TE = TD)
SimData.NULL <- simData(500, CFA.Model.NULL)
SimModel <- simModel(CFA.Model.NULL)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output.NULL <- simResult(SimData.NULL, SimModel, 50)
Cut.NULL <- getCutoff(Output.NULL, 0.95)

u79 <- simUnif(0.7, 0.9)
loading.alt <- matrix(0, 6, 2)
loading.alt[1:3, 1] <- NA
loading.alt[4:6, 2] <- NA
LX.ALT <- simMatrix(loading.alt, 0.7)
latent.cor.alt <- matrix(NA, 2, 2)
diag(latent.cor.alt) <- 1
PH.ALT <- symMatrix(latent.cor.alt, "u79")
CFA.Model.ALT <- simSetCFA(LY = LX.ALT, PS = PH.ALT, TE = TD)
SimData.ALT <- simData(500, CFA.Model.ALT)
Output.ALT <- simResult(SimData.ALT, SimModel, 50)
getPower(Output.ALT, Cut.NULL)
Rule.of.thumb <- c(RMSEA=0.05, CFI=0.95, TLI=0.95, SRMR=0.06)
plotPower(Output.ALT, Output.NULL, alpha=0.05, usedFit=c("RMSEA", "CFI", "TLI", "SRMR"))



cleanEx()
nameEx("run")
### * run

flush(stderr()); flush(stdout())

### Name: run
### Title: Run a particular object in 'simsem' package.
### Aliases: run run-methods run,ANY-method run,NullSimMatrix-method
###   run,NullSymMatrix-method run,NullSimVector-method
### Keywords: run

### ** Examples

n02 <- simNorm(0, 0.2)
run(n02)



cleanEx()
nameEx("runMI")
### * runMI

flush(stderr()); flush(stdout())

### Name: runMI
### Title: Multiply impute and analyze data using lavaan
### Aliases: runMI

### ** Examples

##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data.mat,data.model,imps) {
  #Impute missing data
  imputed.l<-imputeMissing(data.mat,imps)
  
  #Run models on each imputed data set
  #Does this give results from each dataset in the list?
  
  imputed.results<-result.object(imputed.l[[1]],sim.data.model,10)

  imputed.results <- lapply(imputed.l,result.object,data.model,1)
  comb.results<-MIpool(imputed.results,imps)
  
  return(comb.results)

  }



cleanEx()
nameEx("simData")
### * simData

flush(stderr()); flush(stdout())

### Name: simData
### Title: Data object
### Aliases: simData

### ** Examples


loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(200, CFA.Model)
summary(SimData)
run(SimData)

# With Misspecification Model
n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(TD=TD.Mis)
SimData <- simData(200, CFA.Model, misspec=CFA.Model.Mis)
summary(SimData)
run(SimData)



cleanEx()
nameEx("simEqualCon")
### * simEqualCon

flush(stderr()); flush(stdout())

### Name: simEqualCon
### Title: Equality Constraint Object
### Aliases: simEqualCon

### ** Examples

# Example 1: Single-group, one constraint
constraint <- matrix(0, 3, 2)
constraint[1,] <- c(1, 1)
constraint[2,] <- c(2, 1)
constraint[3,] <- c(3, 1)
rownames(constraint) <- rep("LY", 3)
equal.loading <- simEqualCon(constraint, modelType="SEM.exo")

# Example 2: Multiple-group, one constraint
group.con <- matrix(0, 2, 3)
group.con[1,] <- c(1, 2, 1)
group.con[2,] <- c(2, 2, 1)
rownames(group.con) <- rep("BE", 2)
equal.path <- simEqualCon(group.con, modelType="Path")

# Example 3: Single-group, multiple constraints
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading2 <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM")
summary(equal.loading2)



cleanEx()
nameEx("simMatrix")
### * simMatrix

flush(stderr()); flush(stdout())

### Name: simMatrix
### Title: Create simMatrix that save free parameters and starting values,
###   as well as fixed values
### Aliases: simMatrix

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)
run(LX)

n65 <- simNorm(0.6, 0.05)
LY <- simMatrix(loading, "n65")
summary(LY)
run(LY)



cleanEx()
nameEx("simMisspecCFA")
### * simMisspecCFA

flush(stderr()); flush(stdout())

### Name: simMisspecCFA
### Title: Set of model misspecification for CFA model.
### Aliases: simMisspecCFA

### ** Examples

n01 <- simNorm(0, 0.1)
error.cor.Mis <- matrix(NA, 6, 6)
diag(error.cor.Mis) <- 1
TD.Mis <- symMatrix(error.cor.Mis, "n01")
CFA.Model.Mis <- simMisspecCFA(TD=TD.Mis)



cleanEx()
nameEx("simMisspecPath")
### * simMisspecPath

flush(stderr()); flush(stdout())

### Name: simMisspecPath
### Title: Set of model misspecification for Path analysis model.
### Aliases: simMisspecPath

### ** Examples

u1 <- simUnif(-0.1, 0.1)
mis.path.GA <- matrix(0, 2, 2)
mis.path.GA[2, 1:2] <- NA
mis.GA <- simMatrix(mis.path.GA, "u1")
Path.Mis.Model <- simMisspecPath(GA = mis.GA, exo=TRUE)



cleanEx()
nameEx("simMisspecSEM")
### * simMisspecSEM

flush(stderr()); flush(stdout())

### Name: simMisspecSEM
### Title: Set of model misspecification for SEM model.
### Aliases: simMisspecSEM

### ** Examples

u2 <- simUnif(-0.2, 0.2)
n1 <- simNorm(0, 0.1)
loading.X.trivial <- matrix(NA, 6, 2)
loading.X.trivial[is.na(loading.X.trivial)] <- 0
LX.trivial <- simMatrix(loading.X.trivial, "u2")
error.cor.X.trivial <- matrix(NA, 6, 6)
diag(error.cor.X.trivial) <- 0
TD.trivial <- symMatrix(error.cor.X.trivial, "n1")
error.cor.Y.trivial <- matrix(NA, 2, 2)
diag(error.cor.Y.trivial) <- 0
TE.trivial <- symMatrix(error.cor.Y.trivial, "n1")
TH.trivial <- simMatrix(matrix(NA, 6, 2), "n1")
SEM.Mis.Model <- simMisspecSEM(LX = LX.trivial, TE = TE.trivial, TD = TD.trivial, TH = TH.trivial, exo=TRUE)



cleanEx()
nameEx("simModel")
### * simModel

flush(stderr()); flush(stdout())

### Name: simModel
### Title: Create simModel from model specification and be ready for data
###   analysis.
### Aliases: simModel simModel-methods simModel,ANY-method
###   simModel,SimSet-method simModel,SimFreeParam-method

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)
error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)
CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)
SimModel <- simModel(CFA.Model)



cleanEx()
nameEx("simNorm")
### * simNorm

flush(stderr()); flush(stdout())

### Name: simNorm
### Title: Create random normal distribution object
### Aliases: simNorm

### ** Examples

    n02 <- simNorm(0, 0.2)
    run(n02)



cleanEx()
nameEx("simResult")
### * simResult

flush(stderr()); flush(stdout())

### Name: simResult
### Title: Create simResult.
### Aliases: simResult

### ** Examples

loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(SimData, SimModel, 50)
#summary(Output)



cleanEx()
nameEx("simSetCFA")
### * simSetCFA

flush(stderr()); flush(stdout())

### Name: simSetCFA
### Title: Create a set of matrix that belongs to CFA model.
### Aliases: simSetCFA

### ** Examples

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
loadingValues[1:3, 1] <- 0.7
loadingValues[4:6, 2] <- 0.7
LX <- simMatrix(loading, loadingValues)
summary(LX)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
TD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, PH = PH, TD = TD)



cleanEx()
nameEx("simSetPath")
### * simSetPath

flush(stderr()); flush(stdout())

### Name: simSetPath
### Title: Create a set of matrix belongs to Path analysis model
### Aliases: simSetPath

### ** Examples
 
u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.BE <- matrix(0, 4, 4)
path.BE[3, 1:2] <- NA
path.BE[4, 3] <- NA
starting.BE <- matrix("", 4, 4)
starting.BE[3, 1:2] <- "u35"
starting.BE[4, 3] <- "u57"
BE <- simMatrix(path.BE, starting.BE)

residual.error <- diag(4)
residual.error[1,2] <- residual.error[2,1] <- NA
PS <- symMatrix(residual.error, "n31")

Path.Model <- simSetPath(PS = PS, BE = BE)

u35 <- simUnif(0.3, 0.5)
u57 <- simUnif(0.5, 0.7)
u1 <- simUnif(-0.1, 0.1)
n31 <- simNorm(0.3, 0.1)

path.GA <- matrix(0, 2, 2)
path.GA[1, 1:2] <- NA
GA <- simMatrix(path.GA, "u35")

path.BE <- matrix(0, 2, 2)
path.BE[2, 1] <- NA
BE <- simMatrix(path.BE, "u57")

exo.cor <- matrix(NA, 2, 2)
diag(exo.cor) <- 1
PH <- symMatrix(exo.cor, "n31")

PS <- symMatrix(diag(2))

Path.Exo.Model <- simSetPath(PS = PS, BE = BE, PH = PH, GA = GA, exo=TRUE)



cleanEx()
nameEx("simSetSEM")
### * simSetSEM

flush(stderr()); flush(stdout())

### Name: simSetSEM
### Title: Create a set of matrix belongs to SEM model
### Aliases: simSetSEM

### ** Examples

u68 <- simUnif(0.6, 0.8)
loading <- matrix(0, 8, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:8, 3] <- NA
loading.start <- matrix("", 8, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:8, 3] <- "u68"
LY <- simMatrix(loading, loading.start)

TE <- symMatrix(diag(8))

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
PS <- symMatrix(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "n65"
path.start[3, 2] <- "u35"
BE <- simMatrix(path, path.start)

SEM.model <- simSetSEM(BE=BE, LY=LY, PS=PS, TE=TE)

loading.X <- matrix(0, 6, 2)
loading.X[1:3, 1] <- NA
loading.X[4:6, 2] <- NA
LX <- simMatrix(loading.X, 0.7)

loading.Y <- matrix(NA, 2, 1)
LY <- simMatrix(loading.Y, "u68")

TD <- symMatrix(diag(6))

TE <- symMatrix(diag(2))

factor.K.cor <- matrix(NA, 2, 2)
diag(factor.K.cor) <- 1
PH <- symMatrix(factor.K.cor, 0.5)

PS <- symMatrix(as.matrix(1))

path.GA <- matrix(NA, 1, 2)
path.GA.start <- matrix(c("n65", "u35"), ncol=2)
GA <- simMatrix(path.GA, path.GA.start)

BE <- simMatrix(as.matrix(0))

SEM.Exo.model <- simSetSEM(GA=GA, BE=BE, LX=LX, LY=LY, PH=PH, PS=PS, TD=TD, TE=TE, exo=TRUE)



cleanEx()
nameEx("simUnif")
### * simUnif

flush(stderr()); flush(stdout())

### Name: simUnif
### Title: Create random uniform distribution object
### Aliases: simUnif

### ** Examples

u1 <- simUnif(-0.1, 0.1)
run(u1)



cleanEx()
nameEx("simVector")
### * simVector

flush(stderr()); flush(stdout())

### Name: simVector
### Title: Create simVector that save free parameters and starting values,
###   as well as fixed values
### Aliases: simVector

### ** Examples

factor.mean <- rep(NA, 4)
AL <- simVector(factor.mean, 0)

n02 <- simNorm(0, 0.2)
factor.start <- rep("n02", 4)
KA <- simVector(factor.mean, factor.start)



cleanEx()
nameEx("summaryParam")
### * summaryParam

flush(stderr()); flush(stdout())

### Name: summaryParam
### Title: Provide summary of parameter estimates and standard error across
###   replications
### Aliases: summaryParam summaryParam-methods summaryParam,ANY-method
###   summaryParam,SimResult-method summaryParam,SimModelOut-method
###   summaryParam,SimModelMIOut-method

### ** Examples

showClass("SimResult")
loading <- matrix(0, 6, 1)
loading[1:6, 1] <- NA
LX <- simMatrix(loading, 0.7)
PH <- symMatrix(diag(1))
TD <- symMatrix(diag(6))
CFA.Model <- simSetCFA(LY = LX, PS = PH, TE = TD)
SimData <- simData(500, CFA.Model)
SimModel <- simModel(CFA.Model)
# We make the examples running only 50 replications to save time.
# In reality, more replications are needed.
Output <- simResult(SimData, SimModel, 50)
summaryParam(Output)
summaryParam(Output, detail=TRUE)



cleanEx()
nameEx("summaryShort")
### * summaryShort

flush(stderr()); flush(stdout())

### Name: summaryShort
### Title: Provide short summary of an object.
### Aliases: summaryShort summaryShort-methods summaryShort,ANY-method
###   summaryShort,vector-method summaryShort,matrix-method
###   summary,SimFreeParam-method summary,SimLabels-method
###   summary,MatrixSet-method summary,SimRSet-method

### ** Examples

u89 <- simUnif(0.8, 0.9)
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loadingValues <- matrix(0, 6, 2)
LX <- simMatrix(loading, "u89")
summaryShort(LX)



cleanEx()
nameEx("symMatrix")
### * symMatrix

flush(stderr()); flush(stdout())

### Name: symMatrix
### Title: Create symmetric simMatrix that save free parameters and
###   starting values, as well as fixed values
### Aliases: symMatrix

### ** Examples

latent.cor <- matrix(NA, 3, 3)
diag(latent.cor) <- 1
PH <- symMatrix(latent.cor, 0.5)

u46 <- simUnif(0.4, 0.6)
factor.cor <- matrix(NA, 4, 4)
diag(factor.cor) <- 1
factor.cor.start <- matrix("u46", 4, 4)
factor.cor.start[1, 2] <- factor.cor.start[2, 1] <- "0.5"
PS <- symMatrix(factor.cor, factor.cor.start)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
