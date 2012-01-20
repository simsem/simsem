##  Functon to impute missing data, run Lavaan on each one 
##  input: data frames of raw data with missing data, model specification (either a simmodel object or lavaan script), number of imputations wanted)
##  Output: list of results with: parameter estimates, standard errors fit indices, and two types of fraction of missing information
##  Patrick Miller & Alexander Schoemann
##  Last modified 11/17/2011

#Conveniance function to run impuations on data and only return list of data
imputeMissing <- function(data.mat,m, ...){
  # pull out only the imputations
  require(Amelia)
  temp.am <- amelia(data.mat,m, ...)
  return(temp.am$imputations)

} # end imputeMissing

##Currently outputs a list of parameter estimates, standard errors, fit indices and fraction missing information
##TO DO: Get names for each element from the lavaan object

runMI<- function(data.mat,data.model, m, miPackage="amelia", ...) {
  #Currently only supports imputation by Amelia. We want to add mice, and maybe EM imputatin too...
  if(!miPackage=="amelia") stop("Currently runMI only supports imputation by amelia")

  #Impute missing data
  imputed.l<-imputeMissing(data.mat,m, ...)

  #nRep <- m
  args <- list(...)
  
  runSimMI <- function(MIdata,simModel) {
    model <- run(simModel, MIdata)
    return(model)
    }
  


    #Run models on each imputed data set using  simModel  GET simResult OUT!
  if (class(data.model)=="SimModel") {
    imputed.results.l <- lapply(imputed.l, runSimMI,data.model)
    
    
    fit.l <- NULL 
    coef.l <- NULL # We need them. Trust me (Sunthud).
    se.l <- NULL
    converged.l <- NULL
    param.l <- NULL
    
    for(i in 1:length(imputed.results.l)){
      converged.l[[i]] <- imputed.results.l[[i]]@converged			
      Labels <- make.labels(imputed.results.l[[1]]@param, "lavaan") #As a quick default to use OpenMx
      coef.l[[i]] <- vectorize.object(imputed.results.l[[i]]@coef, Labels)
      se.l[[i]] <- vectorize.object(imputed.results.l[[i]]@se, Labels)
      fit.l[[i]] <- imputed.results.l[[i]]@fit
}
  coef <- as.data.frame(do.call(rbind, coef.l))
	se <- as.data.frame(do.call(rbind, se.l))
	fit <- as.data.frame(do.call(rbind, fit.l))
	converged <- as.vector(unlist(converged.l))

	imputed.results <- new("SimResult", modelType=data.model@modelType, nRep=m, coef=coef, se=se, fit=fit, converged=converged)
  }
  
  #Run models on each imputed data set using lavaan syntax
  if (is.character(data.model)) {
    #Function to run lavaan using lapply
    #inputs: raw data, syntax
    #Output: list of parameter estimates, se and fit from each model

    runlavaanMI <- function(MIdata,syntax) {
     model <- cfa(syntax, data=MIdata)
     results <- list(param=coef(model),se=model@Fit@se[!model@Fit@se==0],fit=as.vector(fitmeasures(model)))
     return(results)
    }

    imputed.results.l <- lapply(imputed.l,runlavaanMI,data.model)
	
    results.param<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[1]]))
    results.se<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[2]]))
    results.fit<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[3]]))

    for(i in 1:length(imputed.results.l)){
      results.param[i,]<-unlist(imputed.results.l[[i]][[1]])
      results.se[i,]<-unlist(imputed.results.l[[i]][[2]])
      results.fit[i,]<-unlist(imputed.results.l[[i]][[3]])
    }
    
    coef <- as.data.frame(results.param)
    se <- as.data.frame(results.se)
    fit <- as.data.frame(results.fit)
    
#Need to remove columns representing fixed parameters
  coef <- coef[ , colMeans( MI.param==0 ) == 0, drop=FALSE ]
  coef <- coef[ , colMeans( MI.param==1 ) == 0, drop=FALSE ]
  se <- se[ , colMeans( MI.se==0 ) == 0, drop=FALSE ]
	  
    imputed.results <- new("SimResult", modelType='CFA',nRep=nRep, coef=coef, se=se, fit=fit, converged = c(0))
    #Result <- new("SimResult", modelType=modelType, nRep=nRep, coef=coef, se=se, fit=fit, converged=converged, seed=seed)
  }


  
  comb.results<-miPool(imputed.results,m)
 
 ##Name elements in the list
 ##Only  named when given lavaan syntax 
  if (is.character(data.model)) {
  lavaan.fit.names<-c("chisq", "df", "pvalue", "baseline.chisq", "baseline.df", "baseline.pvalue",
"cfi","tli","logl", "unrestricted.logl", "npar","aic","bic", "ntotal", "bic2", 
"rmsea", "rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr" )
  names(comb.results[[1]])<-names(imputed.results.l[[1]][[1]])
  names(comb.results[[2]])<-names(imputed.results.l[[1]][[1]])
  names(comb.results[[3]])<-lavaan.fit.names
  names(comb.results[[4]])<-names(imputed.results.l[[1]][[1]])
  names(comb.results[[5]])<-names(imputed.results.l[[1]][[1]])
	}
   
  return(comb.results)

}
  

testMI <- function() {
##Shamelessly using the example in lavaan

testd<-HolzingerSwineford1939[,-5]
HS.model <- ' visual  =~ x1 + x2 + x3
               textual =~ x4 + x5 + x6
               speed   =~ x7 + x8 + x9 '
cfa(HS.model,data=test)

##Impose missing data to test
log.mat1 <- makeMCAR(dim(testd),.3,)
testd[log.mat1] <- NA

runMI(testd,HS.model,3)
}
 
