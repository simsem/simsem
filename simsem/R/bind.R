## So one of the tricky things about this function is that we want to infer the correct matrices and check input. The interface is designed to eliminate
## the need to create many input matrices. This is done by specifying a free/fixed matrix.
## Syntax Free/Fixed:
##         Parameters that are freed are indicated by "NA".
##         Parameters that are fixed are numbers.
##         Parameters that share the same character label are constrained to be equal.
## Population Parameters:
##         An element of the matrix can be set to a value or distribution. Data will be generated with those values.
##         Typically, we are interested in specifying the population value of the free parameters in advance.
## Misspecification:
##         However, by setting fixed parameters to values different from what they were fixed to, we can create model misspecification.
##         Misspecification can also be done by passing a value or distribution to the "misspec" argument, in which all fixed parameters will
##         receive that value or distribution.
##         

## Rules: 
##        1. If the free parameters are specified and if only 1 value or distribution is passed to popParam, all free parameters have the same value or distribution.
##        2. If the free parameters are specified, and if only 1 distribution is passed to misspec, all fixed parameters get that distribution or value.
##        
## Validity checks:
## 1. Input matrices have the same dimensions
## 2. The fix/free matrix is specified and has free values
## 3. Distributions are converted to expressions and are valid
## 4. All character vectors passed to popParam and misspec are able to be evaluated (and turned into expressions, because it's faster just to parse it once)
## 5. Free matrix actually contains free parameters
## 6. If both free and popParam are specified, all free parameters have a population value
## 

bind <- function(free = NULL, popParam = NULL, misspec = NULL) {
  if(is.matrix(free)) {
    
    if(!any(is.na(free)) && !validConstraints(free)) { stop("Free matrix contains no free parameters or valid constraints")}
    
    if(is.character(popParam)) {
      tryCatch(eval(parse(text=popParam)), error=function(e) stop(e))
      paramMat <- ifelse(is.na(free),popParam,"")
    }
    else if(is.numeric(popParam) && !is.matrix(popParam)) {
      paramMat <- ifelse(is.na(free),popParam,"")
    }    
    else if(is.matrix(popParam)) {
      if( !all(dim(free)==dim(popParam))) stop("Free matrix and popParam are not of same dimension")
      if( !(apply(paramMat,c(1,2),FUN=check) == is.na(free))) { stop("Please assign a value for any free parameters") }
      paramMat <- as.character(popParam)
    }
    else { paramMat <-  NULL }
    
    if(is.character(misspec)) {
      tryCatch(eval(parse(text=misspec)), error=function(e) stop(e))
      misspecMat <- ifelse(!is.na(free),misspec,"")
    }
    else if(is.numeric(misspec) && !is.matrix(misspec)) {
      misspecMat <- ifelse(!is.na(free),misspec,"")
    }    
    else if(is.matrix(misspec)) {
     if( !all(dim(free)==dim(misspec))) stop("Free matrix and popParam are not of same dimension")
     misspecMat <- as.character(misspec)
    }
    else { misspecMat <-  NULL }
    
  } else { 
    stop("Please specify a free/fixed parameter matrix.")
  }
  return(list(free,paramMat,misspecMat))
}
    
check <- function(x) { if(x == "" || is.na(x)) {FALSE} else {TRUE}}

# Finds valid labels, checks all combinations of label pairs to make sure at least one pair is the same.
validConstraints <- function(mat) {
  flat <- as.vector(mat)

  # The basic idea is to parse and evaluate the character string in the global namespace. If the object doesn't exist,
  # it is a constraint label.
  # However, this is a little sketchy. For instance:
  # If the TemporaryVariableName were x instead, if a label was x, this test would fail.
  maybeLabel <- sapply(flat, FUN= function(TemporaryVariableName) { tryCatch(eval(parse(text=TemporaryVariableName)),
                                  error = function(e) 1)})
  isLabel <- tryCatch(as.logical(maybeLabel), error=function(e)
                      stop("Invalid constraint: Label might be a function name or object in global namespace"))
  combs <- combn(flat[isLabel],2)
  res <- mapply(`==`,combs[1,],combs[2,])
  return(any(res))
}

## test <- function() {
##   a <- matrix(0,2,2)
##   a[,1] <- NA
##   a[,2] <- "a1"
##   bind(free=a)
##   bind(free=a, popParam=.7, misspec=.01)
##   bind(free=a, popParam="runif(1,0,1)", misspec=.01)
##   bind(free=a, popParam="runif(1,0,1)", misspec="runif(1,0,1)")

##   #Error - invalid expression
##   bind(free=a, popParam="runif(1,0,1)", misspec="runif(1,0,1")
##   bind(free=a, popParam="runif(1,0,1", misspec="runif(1,0,1)")

##   ## Doesn't mean anything, but doesn't throw an error?
##   bind(free=a, popParam="a")

##   ## Error - different dimensions
##   bind(free=a, popParam=matrix(0,3,3))
##   bind(free=a, misspec=matrix(0,3,3))

##   ## Equality Constraints
##   a <- matrix(0,2,2)
##   a[,1] <- 0
##   a[,2] <- "a1"
##   bind(free=a)

##   b <- matrix(0,3,3)
##   b[1:2,1] <- "b1"
##   b[2:3,2] <- "b2"
##   bind(free=b)

##   ## Invalid label name. Throws error
##   f <- matrix(0,3,3)
##   f[1,1] <- "c"
##   f[1,2] <- "c1"
##   bind(free=f)
## }

  
     
      
    
  


simMatrix <- function(free = NULL, popParam = NULL, misspec = NULL, prior = NULL) {
    if (is.null(popParam)) {
        if (any(is.na(free))) {
            #stop("There are free parameters but no parameter/starting values are specified.")
        } else {
            return(free, popParam = matrix("", nrow(free), ncol(free)))
        }
    } else {
        if (is.null(free)) {
            if (is.matrix(popParam)) {
                free <- matrix(0, nrow(value), ncol(value))
                free[!((popParam == 0) | (popParam == ""))] <- NA
            } else {
                stop("If the matrix of free parameters is not specified, the population parameter values should be specified as a matrix")
            }
        }
        lab <- matrix("", nrow(free), ncol(free))
        if (any(is.na(free))) {
            if (is.matrix(popParam)) {
                if (!(nrow(popParam) == nrow(free)) | !(ncol(value) == ncol(free))) 
                  stop("Two specified matrices do not have the same dimensions.")
                lab[is.na(free)] <- checkInputValueVector(value[is.na(free)])
            } else {
                lab[is.na(free)] <- checkInputValue(value)
            }
        }
        return(free, popParam, misspec)
    }
} 

# Starting values?
