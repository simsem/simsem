##M3 Simulation Examples

library(simsem)

##Example methodological simulation. Vary percent missing data (MCAR)
##Percent missing, traditional method
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
SimMissing <- simMissing(pmMCAR=0.05, numImps=0)
Output.05 <- simResult(2000, SimData, SimModel, objMissing=SimMissing, multicore=F)
summary(Output.05, digits=5)
round(colMeans(Output.05@fit),3)

SimMissing <- simMissing(pmMCAR=0.40, numImps=0)
Output.40 <- simResult(2000, SimData, SimModel, objMissing=SimMissing, multicore=F)
summary(Output.40, digits=5)
round(colMeans(Output.40@fit),3)


SimMissing <- simMissing(pmMCAR=0.8, numImps=0)
Output.8 <- simResult(2000, SimData, SimModel, objMissing=SimMissing, multicore=F)
summary(Output.8, digits=5)
round(colMeans(Output.8@fit),3)

#Pull elements for F tests
PS.05<-cbind(Output.05@coef$PS2_1, rep(1,length(Output.05@coef$PS2_1)))
PS.40<-cbind(Output.40@coef$PS2_1, rep(2,length(Output.40@coef$PS2_1)))
PS.8<-cbind(Output.8@coef$PS2_1, rep(3,length(Output.8@coef$PS2_1)))
PS <- data.frame(rbind(PS.05,PS.40,PS.8))
PS$X2<-as.factor(PS$X2)
m1<-lm(X1~X2, data=PS)

CFI <- c(Output.05@fit$CFI,Output.40@fit$CFI,Output.8@fit$CFI)
chi <- c(Output.05@fit$Chi,Output.40@fit$Chi,Output.8@fit$Chi)
RMSEA <- c(Output.05@fit$RMSEA,Output.40@fit$RMSEA,Output.8@fit$RMSEA)
SRMR <- c(Output.05@fit$SRMR,Output.40@fit$SRMR,Output.8@fit$SRMR)
PS <- cbind(PS, CFI, chi, RMSEA, SRMR)
PS$X2 <- as.factor(PS$X2)
m1<-lm(CFI~X2, data=PS)
anova(m1)
m1<-lm(chi~X2, data=PS)
anova(m1)
m1<-lm(RMSEA~X2, data=PS)
anova(m1)
m1<-lm(SRMR~X2, data=PS)
anova(m1)


#Continously varying parameters
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.5)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
mis <- simUnif(.01,.9)
Output <- simResult(2000, SimData, SimModel, pmMCAR=mis, multicore=T)

summary(Output, digits=5)
round(colMeans(Output@fit),3)

#Create data frame for analysis
PS <- data.frame(cbind(Output@paramValue$PS2_1,Output@coef$PS2_1, Output@pmMCAR))
names(PS) <- c('pop', 'samp', 'pmMCAR')
PS$bias <- (PS$pop-PS$samp)/PS$pop
m1 <- lm(bias ~ pmMCAR, data=PS)

PS$chis <- Output@fit$Chi
PS$RMSEA <- Output@fit$RMSEA
PS$CFI <- Output@fit$CFI
PS$SRMR <- Output@fit$SRMR

#Analyze results with regression
m1 <- lm(chis ~ pmMCAR, data=PS)
summary(m1)
m1 <- lm(RMSEA ~ pmMCAR, data=PS)
summary(m1)
m1 <- lm(CFI ~ pmMCAR, data=PS)
summary(m1)
m1 <- lm(SRMR ~ pmMCAR, data=PS)
summary(m1)

#plot results for continously varying percent missing
plot(PS$pmMCAR, PS$bias, ylab="Parameter Bias", xlab="% Missing", ylim=c(-1,1), mar=c(0,0,0,0), cex.lab=1.5, cex.axis=1.25)
m1 <- lm(bias ~ pmMCAR, data=PS)
abline(reg = m1, col='blue', lwd=2.5)
points(.05, -.00004, pch=16, col='red', lwd=2.5, bg='red')
points(.40,  .00021, pch=16, col='red', lwd=2.5, bg='red')
points(.80, -.00882, pch=16, col='red', lwd=2.5, bg='red')

plot(PS$pmMCAR, PS$CFI, ylab="CFI", xlab="% Missing", ylim=c(.7,1), mar=c(0,0,0,0), cex.lab=1.5, cex.axis=1.25)
m1 <- lm(CFI ~ pmMCAR, data=PS)
abline(reg = m1, col='blue', lwd=2.5)
points(.05, .998, pch=16, col='red', lwd=2.5, bg='red')
points(.40, .994, pch=16, col='red', lwd=2.5, bg='red')
points(.80, .956, pch=16, col='red', lwd=2.5, bg='red')

plot(PS$pmMCAR, PS$SRMR, ylab="SRMR", xlab="% Missing", ylim=c(0,.2), mar=c(0,0,0,0), cex.lab=1.5, cex.axis=1.25)
m1 <- lm(SRMR ~ pmMCAR, data=PS)
abline(reg = m1, col='blue', lwd=2.5)
points(.05, .017, pch=16, col='red', lwd=2.5, bg='red')
points(.40, .029, pch=16, col='red', lwd=2.5, bg='red')
points(.80, .107, pch=16, col='red', lwd=2.5, bg='red')

plot(PS$pmMCAR, PS$chis, ylab="Chi square", xlab="% Missing", ylim=c(0,40), mar=c(0,0,0,0), cex.lab=1.5, cex.axis=1.25)
m1 <- lm(chis ~ pmMCAR, data=PS)
abline(reg = m1, col='blue', lwd=2.5)
points(.05, 8.13, pch=16, col='red', lwd=2.5, bg='red')
points(.40, 8.23, pch=16, col='red', lwd=2.5, bg='red')
points(.80, 8.16, pch=16, col='red', lwd=2.5, bg='red')

plot(PS$pmMCAR, PS$RMSEA, ylab="RMSEA", xlab="% Missing", ylim=c(0,.10), mar=c(0,0,0,0), cex.lab=1.5, cex.axis=1.25)
m1 <- lm(RMSEA ~ pmMCAR, data=PS)
abline(reg = m1, col='blue', lwd=2.5)
points(.05, .012, pch=16, col='red', lwd=2.5, bg='red')
points(.40, .013, pch=16, col='red', lwd=2.5, bg='red')
points(.80, .014, pch=16, col='red', lwd=2.5, bg='red')

##Power analysis example
##Continously varying sample size
loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LX <- simMatrix(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPH <- symMatrix(latent.cor, 0.1)

error.cor <- matrix(0, 6, 6)
diag(error.cor) <- 1
RTD <- symMatrix(error.cor)

CFA.Model <- simSetCFA(LX = LX, RPH = RPH, RTD = RTD)
SimData <- simData(CFA.Model, 500)
SimModel <- simModel(CFA.Model)
ContN <- simUnif(100, 2000)
Output.pow <- simResult(3000, SimData, SimModel, n=ContN, multicore=F)
summary(Output.pow, digits=5)
pow<-continuousPower(Output.pow, powerParam = 'PS2_1')
pow[pow[,2]>.8 & pow[,2]<.84,][1,] #power of .80004 is sample size of 1436

#plot power
plot(pow[,1], pow[,2], type='l',ylab="Power", xlab="Sample", lwd=2, ylim=c(0,1), cex.lab=1.5, cex.axis=1.25)


#Test power traditional way
SimData <- simData(CFA.Model, 1436)
SimModel <- simModel(CFA.Model)
Output.trad <- simResult(3000, SimData, SimModel, multicore=T)
summary(Output.trad, digits=5) #power = .810



