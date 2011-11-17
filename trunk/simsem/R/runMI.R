##Functon to impute missing data, run Lavaan on each one 
##input: data frames of raw data with missing data, model specification (either a simmodel object or lavaan script), number of imputations wanted)
##Output: list of results with: parameter estimates, standard errors fit indices, and two types of fraction of missing information
##Patrick Miller & Alexander Schoemann
##Last modified 11/16/2011

#Conveniance function to run impuations on data and only return list of data
imputeMissing <- function(data.mat,imps){
  # pull out only the imputations
  require(Amelia)
  temp.am <- amelia(data.mat,m=imps)
  return(temp.am$imputations)

} # end imputeMissing

##Currently outputs a list of parameter estimates, standard errors, fit indices and fraction missing information
##TO DO: Get names for each element from the lavaan object

runMI<- function(data.mat,data.model,imps,miPackage="amelia") {
  #Currently only supports imputation by Amelia. We want to add mice, and maybe EM imputatin too...
  if(!miPackage=="amelia") stop("Currently runMI only supports imputation by amelia")

  #Impute missing data
  imputed.l<-imputeMissing(data.mat,imps)
  
    #Run models on each imputed data set using  simModel 
  if (class(data.model)=="simModel") {
    imputed.results <- lapply(imputed.l, simResult,data.model,1)
  }
  
  #Run models on each imputed data set using lavaan syntax
  if (is.character(data.model)) {
    #Function to run lavaan using lapply
	#inputs: raw data, syntax
	#Output: list of parameter estimates, se and fit from each model
   runlavaan <- function(MIdata,syntax) {
     model <- cfa(syntax, data=MIdata)
	 results <- list(param=coef(model),se=model@Fit@se[!model@Fit@se==0],fit=as.vector(fitmeasures(model)))
	 return(results)
	 }
    imputed.results.l <- lapply(imputed.l,runlavaan,data.model)
	
	results.param<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[1]]))
    results.se<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[2]]))
    results.fit<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[3]]))

    for(i in 1:length(imputed.results.l)){
      results.param[i,]<-unlist(imputed.results.l[[i]][[1]])
      results.se[i,]<-unlist(imputed.results.l[[i]][[2]])
      results.fit[i,]<-unlist(imputed.results.l[[i]][[3]])
      }
	
	imputed.results <- new("SimResult", modelType='CFA',nRep=imps, coef=as.data.frame(results.param), se=as.data.frame(results.se), fit=as.data.frame(results.fit), converged = c(0))
		#Result <- new("SimResult", modelType=modelType, nRep=nRep, coef=coef, se=se, fit=fit, converged=converged, seed=seed)

	}


  
  comb.results<-miPool(imputed.results,imps)
  ##Name elements in the list
  fit.names<-c( "chisq","df","pvalue","baseline.chisq","baseline.df","baseline.pvalue","cfi","tli","logl","unrestricted.logl","npar","aic","bic","ntotal","bic2","rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr")  
  names(comb.results[[3]])<-fit.names
  names(comb.results[[1]])<-names(imputed.results.l$imp1$param)
  names(comb.results[[2]])<-names(imputed.results.l$imp1$param)
  names(comb.results[[4]])<-names(imputed.results.l$imp1$param)
  names(comb.results[[5]])<-names(imputed.results.l$imp1$param)
	
  return(comb.results)

}



testMI <- function() {
##Shamelessly using the example in lavaan
test<-HolzingerSwineford1939[,-5]
cfa(HS.model,data=test)

##Impose missing data to test
log.mat1 <- makeMCAR(dim(test),.1,covs=NULL)
test[log.mat1] <- NA

runMI(test,HS.model,3)
}
 
