#Monte Carlo Approach to Model Fit Evaluation in Structural Equation Modeling: How to Specify Trivial Misspecification
#Sunthud Pornprasertmanit, Wei Wu, Todd D. Little
#Department of Psychology and the Center for Research Methods and Data Analysis, The University of Kansas
#Presented at the 24th American Psychological Society Annual Convention, May 25th, 2012, Chicago, IL

#We use the simsem package for data generation and evaluating model fit using the Monte Carlo approach. Here is the example code for data generation when the cross loadings in the trivial misspecification is 0.3 and the sample size is 500:
n <- 500
mis <- 0.3
nRep <- 1000
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
modelData <- simData(param=modelPop, n=n, misspec=misPop)
dat <- run(modelData)

#Here is the example code for running the model specified in Figure 1a:
loadingParam <- matrix(0, 9, 3)
loadingParam[1:3, 1] <- NA
loadingParam[4:6, 2] <- NA
loadingParam[7:9, 3] <- NA
paramModel <- simParamCFA(LY=loadingParam)
analyzeModel <- simModel(paramModel)
out <- run(analyzeModel, dat)

#Here is the code for specifying the fixed method with the correct cross-loadings:
loadingMis2 <- matrix(0, 9, 3)
loadingMis2[1,2] <- NA
loadingMis2[4,3] <- NA
LYMis2 <- simMatrix(loadingMis2, trivialMis)
misspec2 <- simMisspecCFA(LY=LYMis2, misBeforeFill=FALSE)

#Here is the code for specifying the fixed method with the incorrect cross-loadings:
loadingMis3 <- matrix(0, 9, 3)
loadingMis3[6,1] <- NA
loadingMis3[9,2] <- NA
LYMis3 <- simMatrix(loadingMis3, trivialMis)
misspec3 <- simMisspecCFA(LY=LYMis3, misBeforeFill=FALSE)

#Here is the code for specifying the random method with uniform distributions:
u3 <- simUnif(-trivialMis, trivialMis)
loadingMis4 <- matrix(0, 9, 3)
loadingMis4[4:9, 1] <- NA
loadingMis4[c(1:3, 7:9),2] <- NA
loadingMis4[1:6,3] <- NA
LYMis4 <- simMatrix(loadingMis4, toFunction(u3))
misspec4 <- simMisspecCFA(LY=LYMis4, misBeforeFill=FALSE)

#Here is the code for specifying the random method with normal distributions:
n3 <- simNorm(0, trivialMis/2)
loadingMis5 <- matrix(0, 9, 3)
loadingMis5[4:9, 1] <- NA
loadingMis5[c(1:3, 7:9),2] <- NA
loadingMis5[1:6,3] <- NA
LYMis5 <- simMatrix(loadingMis5, toFunction(n3))
misspec5 <- simMisspecCFA(LY=LYMis5, misBeforeFill=FALSE)

#Here is the code for specifying the maximal method:
u3 <- simUnif(-trivialMis, trivialMis)
loadingMis6 <- matrix(0, 9, 3)
loadingMis6[4:9, 1] <- NA
loadingMis6[c(1:3, 7:9),2] <- NA
loadingMis6[1:6,3] <- NA
LYMis6 <- simMatrix(loadingMis6, toFunction(u3))
misspec6 <- simMisspecCFA(LY=LYMis6, optMisfit="max", numIter=100, misBeforeFill=FALSE)

#The Monte Carlo approach can be done by the runFit function by
simOut <- runFit(model=analyzeModel, realdata=dat, nRep=nRep, seed=seed, misspec=misspec2) 

#The misspecification method can be changed to different types of misspecification from misspec2 to misspec3, … , or misspec6.
pValue(out, simOut)
