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
##        3. If equality constraints are specified, labels can't be objects already in the environment or function names.
##        4. Any numeric value in popParam or misspec (including 0) will be considered to be a parameter value for data generation. Empty values should be either "" or NA
##        
## Validity checks:
## 1. Input matrices have the same dimensions or vectors have same length
## 2. Distributions are converted to expressions and are valid
## 3. All character vectors passed to popParam and misspec are able to be evaluated
## 4. If both free and popParam are specified, all free parameters have a population value
## 5. If labels are included in free (specifying equality constraints), these labels are valid and at least one pair of the labels is the same.

bind <- function(free = NULL, popParam = NULL, misspec = NULL) {
  ## SimMatrix
  if(is.matrix(free)) {
    
    if(any(is.character(free)) && !validConstraints(free)) { stop("At least one pair of constraint labels must be the same.")}
    
    if(is.character(popParam)) {
      tryCatch(eval(parse(text=popParam)), error=function(e) stop(e))
      paramMat <- ifelse(is.free(free),popParam,"")
    }
    else if(is.numeric(popParam) && !is.matrix(popParam)) {
      paramMat <- ifelse(is.free(free),popParam,"")
    }    
    else if(is.matrix(popParam)) {
      if( !all(dim(free)==dim(popParam))) stop("Free matrix and popParam are not of same dimension")
      if( all(!is.empty(popParam) != is.free(free))) { stop("Please assign a value for any free parameters") }
      paramMat <- matrix(as.character(popParam),nrow=nrow(popParam),ncol=ncol(popParam))
    }
    else { paramMat <-  matrix(NaN) }
    
    if(is.character(misspec)) {
      tryCatch(eval(parse(text=misspec)), error=function(e) stop(e))
      misspecMat <- ifelse(!is.free(free),misspec,"")
    }
    else if(is.numeric(misspec) && !is.matrix(misspec)) {
      misspecMat <- ifelse(!is.free(free),misspec,"")
    }    
    else if(is.matrix(misspec)) {
     if( !all(dim(free)==dim(misspec))) stop("Free matrix and misspec are not of same dimension")
     misspecMat <- matrix(as.character(misspec),nrow=nrow(misspec),ncol=ncol(misspec))
    }
    else { misspecMat <-  matrix(NaN) }

    return(new("SimMatrix",free=free,popParam=paramMat,misspec=misspecMat))
    
    # SimVector
  } else if(is.vector(free)) {

    if(any(is.character(free)) && !validConstraints(free)) { stop("At least one pair of constraint labels must be the same.")}
    
    if(is.character(popParam) && length(popParam == 1)) {
      tryCatch(eval(parse(text=popParam)), error=function(e) stop(e))
      paramVec <- ifelse(is.free(free),popParam,"")
    }
    else if(is.numeric(popParam) && length(popParam) == 1) {
      paramVec <- ifelse(is.free(free),popParam,"")
    }    
    else if(is.vector(popParam)) {
      if((length(free) != length(popParam)) && length(popParam) > 1) stop("Free vector and popParam are not the same length")
      if( all(!is.empty(popParam) == is.free(free))) { stop("Please assign a value for any free parameters") }
      paramVec <- as.character(popParam)
    }
    else { paramVec <-  vector() }
    
    if(is.character(misspec) && length(misspec == 1)) {
      tryCatch(eval(parse(text=misspec)), error=function(e) stop(e))
      misspecVec <- ifelse(!is.free(free),misspec,"")
    }
    else if(is.numeric(misspec) && length(misspec)== 1) {
      misspecVec <- ifelse(!is.free(free),misspec,"")
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

combine <- function(dat) {
  if(
    
}

# Possible "empty values": "", or NA
is.empty <- function(dat) {
   if(is.null(dim(dat))) {
    temp <- sapply(dat, FUN=function(x) if(x == "" || is.na(x)) {TRUE} else {FALSE})
    names(temp) <- NULL
    return(temp)
  }
   apply(dat, c(1,2), FUN=function(x) if(x == "" || is.na(x)) {TRUE} else {FALSE})
        
}


# Finds valid labels, checks all combinations of label pairs to make sure at least one pair is the same.
validConstraints <- function(mat) {
  labels <- is.label(mat)
  combs <- combn(labels[labels],2)
  res <- combs[1,] & combs[2,]
  return(any(res))
}

is.label <- function(mat) {
  flat <- as.vector(mat)
  flat[is.na(flat)] <- 0
  isLabel <- sapply(flat, FUN= function(x) {suppressWarnings(is.na(as.numeric(x))) })
  return(isLabel)
}

is.free <- function(mat) {
  if(is.character(mat)) {
    isFree <- is.na(mat) | is.label(mat)
  } else {
    isFree <- is.na(mat)
  }
  return(isFree)
}
  

test <- function() {
  a <- matrix(0,2,2)
  a[1,1] <- NA
  a[1,2] <- 0
  a[,2] <- "a1"
  bind(free=a)
  bind(free=a, popParam=.7, misspec=.01)
  bind(free=a, popParam="runif(1,0,1)", misspec=.01)
  bind(free=a, popParam="runif(1,0,1)", misspec="runif(1,0,1)")

  b <- c(NA,0,"b1","b1")
  bind(free=b)
  bind(free=b, popParam=.7, misspec=.01)
  bind(free=b, popParam="runif(1,0,1)", misspec=.01)
  bind(free=b, popParam="runif(1,0,1)", misspec="runif(1,0,1)")
  
}
