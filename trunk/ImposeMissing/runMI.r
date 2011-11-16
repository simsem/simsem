##Functon to impute missing data, run Lavaan on each one 
##input: data frames of raw data with missing data, model specification (either a simmodel object or lavaan script), number of imputations wanted)
##Output: list of results with: parameter estimates, standard errors fit indices, and two types of fraction of missing information


runMI<- function(data.mat,data.model,imps) {
  #Impute missing data
  imputed.l<-imputeMissing(data.mat,imps)
  
    #Run models on each imputed data set using  simModel 
  if (class(sim.data.model)=="simModel") {
    imputed.results <- lapply(imputed.l,result.object,data.model,1)
  }
  
  #Run models on each imputed data set using lavaan syntax
  if (is.character(data.model)) {
    #Function to run lavaan using lapply
	#inputs: raw data, syntax
	#Output: list of parameter estimates, se and fit from each model
   runlavaan <- function(MIdata,syntax) {
     model <- lavaan(syntax, data=MIdata)
	 results <- list(param=coef(model),se=model@Fit@se[!model@Fit@se==0],fit=as.vector(fitmeasures(model)
	 return(results)
	 }
    imputed.results.l <- lapply(imputed.l,runlavaan,data.model)
	
	results.param<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[1]]))
    results.se<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[2]]))
    results.fit<-matrix(NA,nrow=length(imputed.results.l),ncol=length(imputed.results.l[[1]][[3]]))

    for(i in 1:length(imputed.results)){
      results.param[i,]<-unlist(imputed.results.l[[i]][[1]])
      results.se[i,]<-unlist(imputed.results.l[[i]][[2]])
      results.fit[i,]<-unlist(imputed.results.l[i]][[3]])
      }
	
	imputed.results <- new("simResult", Replication=imps, Estimates=as.data.frame(results.param), SE=as.data.frame(results.se), Fit=as.data.frame(results.fit), Convergence = c(0))
	
	}

  
  
  comb.results<-miPool(imputed.results,imps)
  
  return(comb.results)

}

#Conveniance function to run impuations on data and only return list of data
imputeMissing <- function(data.mat,imps){
  # pull out only the imputations
  require(Amelia)
  temp.am <- amelia(data.mat,m=imps)
  return(temp.am$imputations)

} # end imputeMissing

