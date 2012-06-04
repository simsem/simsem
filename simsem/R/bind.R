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
## 3. Distributions are converted to expressions and are valid
## 4. All character vectors passed to popParam and misspec are able to be evaluated (and turned into expressions, because it's faster just to parse it once)
## 5. If 
## 6. If both free and popParam are specified, all free parameters have a population value
## 

bind <- function(free = NULL, popParam = NULL, misspec = NULL) {
  ## SimMatrix
  if(is.matrix(free)) {
    
    if(any(is.character(free)) && !validConstraints(free)) { stop("At least one pair of constraint labels must be the same.")}
    
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
    else { paramMat <-  matrix(NaN) }
    
    if(is.character(misspec)) {
      tryCatch(eval(parse(text=misspec)), error=function(e) stop(e))
      misspecMat <- ifelse(!is.na(free),misspec,"")
    }
    else if(is.numeric(misspec) && !is.matrix(misspec)) {
      misspecMat <- ifelse(!is.na(free),misspec,"")
    }    
    else if(is.matrix(misspec)) {
     if( !all(dim(free)==dim(misspec))) stop("Free matrix and misspec are not of same dimension")
     misspecMat <- as.character(misspec)
    }
    else { misspecMat <-  matrix(NaN) }

    return(new("SimMatrix",free=free,popParam=paramMat,misspec=misspecMat))
    
    # SimVector
  } else if(is.vector(free)) {

    if(any(is.character(free)) && !validConstraints(free)) { stop("At least one pair of constraint labels must be the same.")}
    
    if(is.character(popParam) && length(popParam == 1)) {
      tryCatch(eval(parse(text=popParam)), error=function(e) stop(e))
      paramVec <- ifelse(is.na(free),popParam,"")
    }
    else if(is.numeric(popParam) && length(popParam) == 1) {
      paramVec <- ifelse(is.na(free),popParam,"")
    }    
    else if(is.vector(popParam)) {
      if((length(free) != length(popParam)) && length(popParam) > 1) stop("Free vector and popParam are not the same length")
      if( !(apply(paramVec,c(1,2),FUN=check) == is.na(free))) { stop("Please assign a value for any free parameters") }
      paramVec <- as.character(popParam)
    }
    else { paramVec <-  vector() }
    
    if(is.character(misspec) && length(misspec == 1)) {
      tryCatch(eval(parse(text=misspec)), error=function(e) stop(e))
      misspecVec <- ifelse(!is.na(free),misspec,"")
    }
    else if(is.numeric(misspec) && length(misspec)== 1) {
      misspecVec <- ifelse(!is.na(free),misspec,"")
    }    
    else if(is.vector(misspec)) {
      if((length(free) != length(misspec)) && length(misspec) > 1) stop("Free vector and misspec are not the same length")
      misspecVec <- as.character(misspec)
    }
    else { misspecVec <-  vector() }

    return(new("SimVector",free=free,popParam=paramVec,misspec=misspecVec))
  } else { 
    stop("Please specify a free/fixed parameter matrix or vector.")
  }
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

##   b <- c(NA,NA,"b1","b1")
##   bind(free=b)
##   bind(free=b, popParam=.7, misspec=.01)
##   bind(free=b, popParam="runif(1,0,1)", misspec=.01)
##   bind(free=b, popParam="runif(1,0,1)", misspec="runif(1,0,1)")
  
## }
