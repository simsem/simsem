##  Functon to impute missing data, run Lavaan on each one 
##  input: data frames of raw data with missing data, model specification (either a simmodel object or lavaan script), number of imputations wanted)
##  Output: list of results with: parameter estimates, standard errors fit indices, and two types of fraction of missing information
##  Patrick Miller & Alexander Schoemann
##  Last modified 11/17/2011

#Conveniance function to run impuations on data and only return list of data
imputeMissing <- function(data.mat,m, ...){
  # pull out only the imputations
  require(Amelia)
  temp.am <- amelia(data.mat,m, p2s=0, ...)
  return(temp.am$imputations)

} # end imputeMissing

##Currently outputs a list of parameter estimates, standard errors, fit indices and fraction missing information
##TO DO: Get names for each element from the lavaan object

runMI<- function(data.mat,data.model, m, miPackage="amelia", silent=FALSE, ...) {
################### I put the silent argument here as the 'runRep' and 'simResult' have one.


  #Currently only supports imputation by Amelia. We want to add mice, and maybe EM imputatin too...
  if(!miPackage=="amelia") stop("Currently runMI only supports imputation by amelia")

  #Impute missing data
  imputed.l<-imputeMissing(data.mat,m, ...)

  #nRep <- m
  args <- list(...)
  
  ## Return list of simModelOut objects, to be combined. 
  runSimMI <- function(MIdata,simModel) {
    model <- run(simModel, MIdata)
    return(model)
    }
  


    #Run models on each imputed data set using  simModel  
  if (class(data.model)=="SimModel") {
    imputed.results.l <- lapply(imputed.l, runSimMI,data.model)
    
  }
  
  #Run models on each imputed data set using lavaan syntax Can we switch to simSEM framework?
  if (is.character(data.model)) {
    #Function to run lavaan using lapply
    #inputs: raw data, syntax
    #Output: list of parameter estimates, se and fit from each model

    runlavaanMI <- function(MIdata,syntax) {
     fit <- cfa(syntax, data=MIdata)
     FitIndices <- extract.lavaan.summary(fit)
	   coef <- inspect(fit, "coef")
     se <- inspect(fit, "se")
	#Converged <- fit@fit@converged
	Converged = TRUE
    if(sum(unlist(lapply(inspect(fit, "se"), sum))) == 0) Converged = FALSE
    return(new("SimModelOut", package='lavaan', coef=coef,
        fit=FitIndices, se=se, converged=Converged))
    }

    imputed.results.l <- lapply(imputed.l, runlavaanMI, data.model)
    }


  ##New miPool should return simResult object. Can be used with runRep runSIM or can be summarized. 
  comb.results<-miPool(imputed.results.l)
 
 ##Name elements in the list
 ##Only  named when given lavaan syntax 
  
  # if (class(data.model)=="SimModel") {
  # lavaan.fit.names<-c('Chi', 'df', 'pvalue', 'baseline.Chi', 'baseline.df', 'baseline.pvalue', 'CFI', 'TLI', 'AIC', 'BIC', 'RMSEA', 'RMSEA.ci.lower', 'RMSEA.ci.upper', 'SRMR')
  # names(comb.results[[3]])<-lavaan.fit.names
  # }

# if (is.character(data.model)) {
  # names(comb.results[[1]])<-names(imputed.results.l[[1]][[1]])
  # names(comb.results[[2]])<-names(imputed.results.l[[1]][[1]])
  # lavaan.fit.names<-c("chisq", "df", "pvalue", "baseline.chisq", "baseline.df", "baseline.pvalue","cfi","tli","logl", "unrestricted.logl", "npar","aic","bic", "ntotal", "bic2", 
# "rmsea", "rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr" )
# names(comb.results[[3]])<-lavaan.fit.names
  # names(comb.results[[4]])<-names(imputed.results.l[[1]][[1]])
  # names(comb.results[[5]])<-names(imputed.results.l[[1]][[1]])
	# }
 
   
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
 
