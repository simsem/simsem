## Function to calculate power with continously varying parameters
## Will calculate power over continuously varying n, percent missing, or parameters
##  input: simResult object, continuously varying n?, continuously varing MCAR?, 
##         continuously varying MAR, paramters that vary continously (vector or parameter names)
##          alpha level desired, names of parameters to compute power for
##  Output: power for parameters of interest (defaults to all parameters)
##  Alexander Schoemann
##  Last modified 04/07/2012

continousPower <- function(simResult, contN = TRUE, contMCAR = FALSE, contMAR = FALSE, contParam = NULL, alpha = .05,
                            powerparam = NULL, desiredPower = .80){
                            
    #Change warning option to supress warnings
    warnT <- as.numeric(options("warn"))
    options(warn=-1)
    
   #Clean simResult object and get a replications X parameters matrix of 0s and 1s for logistic regression 
  object <- clean(simResult)
	crit.value <- qnorm(1 - alpha/2)
  sig <- 0 + (abs(object@coef/object@se) > crit.value)
	nrep <- dim(sig)[[1]]

##Find params to get power for
  if(!isNullObject(powerparam)) {	
  j <- grep(powerparam,dimnames(sig)[[2]])  # Return column indices that start with "param"
 	sig<- sig[,j]
  }
  

	
	#Create matrix of predictors (randomly varying params)
	x <- NULL
	pred <- NULL
	
	if (contN)  {
	if (!length(object@n)== nrep) {
	stop ("Number of random sample sizes is not the same as the number of replications, check to see if N varied across replications")
	}
	x <- cbind(x, object@n)
	pred$N <- min(object@n):max(object@n)
	}
	if (contMCAR) {
	if (!length(object@pmMCAR)== nrep) {
	stop ("Number of random pmMCARs is not the same as the number of replications, check to see if pmMCAR varied across replications")
	}
	x <- cbind(x, object@pmMCAR)
	pred$MCAR <- seq(min(object@pmMCAR),max(object@pmMCAR), by = .01)

	}
	if (contMAR) {
	if (!length(object@pmMAR)== nrep) {
	stop ("Number of random pmMARs is not the same as the number of replications, check to see if pmMAR varied across replications")
	}
	x <- cbind(x, object@pmMAR)
	pred$MAR <- seq(min(object@pmMAR),max(object@pmMAR), by = .01)

	}
	if(!isNullObject(contParam)) {	
	if (!(dim(object@paramValue)[[2]] == nrep)) {
	stop ("Number of random parameters is not the same as the number of replications, check to see if parameters varied across replications")
	}
	j <- grep(contParam,names(object@paramValue))  # Return column indices that start with "contParam"
 	x <- cbind(x,object@paramValue[,j])
	#need way to get predicted values in here...
	}
	
	res <- NULL
	powVal<-data.frame(expand.grid(pred))
	names(powVal) <-c('x1', 'x2')
	
    #Need way to handle params when power is 1 or 0... or atleast suppress warnings
	##WHY can't I predict more values than nRep!!!!!!
	
	for (i in 1:dim(sig)[[2]]){
	mod<-invisible(try(glm(sig[,i]~x, family=binomial(link = 'logit')),silent=TRUE))
 	res[[dimnames(sig)[[2]][[i]]]]<-data.frame(predict(mod,newdata=powVal,type='response'))
	}
	res <- do.call(cbind, res)
	names(res) <- names(object@coef)
	pow <- cbind(x,res)
  pow[pow < desiredPower] <- NA #puts NA in for below power deisred
  
  powCond <- NULL
  ##Find first column with power > desired power. Only works for 1 random parameters
  if(dim(x)[[2]] == 1) {
  powCond <- as.numeric((lapply(apply(pow > desiredPower, 2, which), head, 1)))
  powCond <- x[powCond][-1]
  names(powCond) <- names(object@coef)
  }

  
  ##Currently returns power for all combinations of randm parameters for all model parameters need to make this better.
  return(list(powCond, pow))
  
  ##Return warnings setting to user's settings
  options(warn=warnT)
}
