#Combine MI results (Rubin's Rules, etc)
#Modified 10/27/2011
#Turn into a function... Done!
#Currently this function is optimized to use lavaan!
#MIpool returns a list of results with: parameter estimates, standard errors
#fit indices, and two types of fraction of missing information

#To do: Needs some test cases

#Example: MIpool(imputed.results)
#  imputed.results = simResult object from the RunMI function

miPool<-function(imputed.results,imps){
  ncol <- dim(imputed.results@coef)[1]
  nrow <- imputed.results@nRep


   MI.param<-(imputed.results@coef)
   MI.se<-(imputed.results@se)
   MI.fit<-(imputed.results@fit)
 #}

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
  Fit.indices <- as.vector(colMeans(MI.fit))
  
#Get rid of estimates from fixed variables
fixedParam <- Bm==0

Estimates <- subset(Estimates, !fixedParam)
SE <- subset(SE, !fixedParam)
FMI.1 <- subset(FMI.1, !fixedParam)
FMI.2 <- subset(FMI.2, !fixedParam)

  MI.res<-list(Estimates,SE,Fit.indices,FMI.1,FMI.2)
  names(MI.res)<-c('coef','se','fit','FMI.1','FMI.2')
#compute chi-square proportion (is this useful?)
#(MI.fit.mat$chisq.p is a placeholder for however we'll index the p-value of chi square)
#chisq <- sum(MI.fit.mat$chisq.pval<.05)/m
  return(MI.res)
}


  
