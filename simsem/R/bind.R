## So one of the tricky things about this function is that we want to infer the
## correct matrices and check input. The interface is designed to eliminate the
## need to create many input matrices. This is done by specifying a free/fixed
## matrix.  Syntax Free/Fixed:

## Parameters that are freed are indicated by 'NA'.  Parameters that are fixed
## are numbers.  Parameters that share the same character label are constrained
## to be equal.  Population Parameters: An element of the matrix can be set to
## a value or distribution. Data will be generated with those values.
## Typically, we are interested in specifying the population value of the free
## parameters in advance.  Misspecification: However, by setting fixed
## parameters to values different from what they were fixed to, we can create
## model misspecification.  Misspecification can also be done by passing a
## value or distribution to the 'misspec' argument, in which all fixed
## parameters will receive that value or distribution.

## Rules: 1. If the free parameters are specified and if only 1 value or
## distribution is passed to popParam, all free parameters have the same value
## or distribution.  2. If the free parameters are specified, and if only 1
## distribution is passed to misspec, all fixed parameters get that
## distribution or value.  3. If equality constraints are specified, labels
## can't be objects already in the environment or function names.  4. Any
## numeric value in popParam or misspec (including 0) will be considered to be
## a parameter value for data generation. Empty values should be either '' or
## NA
## 
## Validity checks: 1. Input matrices have the same dimensions or vectors have
## same length 2. Distributions are converted to expressions and are valid 3.
## All character vectors passed to popParam and misspec are able to be
## evaluated 4. If both free and popParam are specified, all free parameters
## have a population value 5. If labels are included in free (specifying
## equality constraints), these labels are valid and at least one pair of the
## labels is the same.


bind <- function(free = NULL, popParam = NULL, misspec = NULL, symmetric = FALSE) {
    ## SimMatrix
    if (is.matrix(free)) {
        
        if (symmetric) {
            stopifnot(isSymmetric(free))
        }
		
        ## PopParam Must be either character or numeric
        if (is.character(popParam)) {
            tryCatch(eval(parse(text = popParam)), error = function(e) stop(e))
            if (!is.matrix(popParam)) {
                paramMat <- ifelse(is.free(free), popParam, "")
            }
        } else if (is.numeric(popParam) && !is.matrix(popParam)) {
            paramMat <- ifelse(is.free(free), popParam, "")
        }
        
        # Can optionally also be a matrix
        if (is.matrix(popParam)) {
            if (symmetric) {
                stopifnot(isSymmetric(popParam))
            }
            if (!all(dim(free) == dim(popParam))) 
                stop("Free matrix and popParam are not of same dimension")
			popParam[!is.free(free)] <- ""
            if (any(!is.empty(popParam) != is.free(free))) {
                stop("Please assign a value for any free parameters")
            }
            paramMat <- matrix(as.character(popParam), nrow = nrow(popParam), ncol = ncol(popParam))
            
        }
        
        if (is.null(popParam)) {
            paramMat <- matrix(NaN)
        }
        
        # Misspec - same tests as above, almost.
        if (is.character(misspec)) {
            tryCatch(eval(parse(text = misspec)), error = function(e) stop(e))
            if (!is.matrix(misspec)) 
                misspecMat <- ifelse(!is.free(free), misspec, "")
        } else if (is.numeric(misspec) && !is.matrix(misspec)) {
            misspecMat <- ifelse(!is.free(free), misspec, "")
        }
        
        if (is.matrix(misspec)) {
            if (symmetric) {
                stopifnot(isSymmetric(misspec))
            }
            if (!all(dim(free) == dim(misspec))) 
                stop("Free matrix and misspec are not of same dimension")
            misspecMat <- matrix(as.character(misspec), nrow = nrow(misspec), ncol = ncol(misspec))
            
        }
        if (is.null(misspec)) {
            misspecMat <- matrix(NaN)
        }
        
        return(new("SimMatrix", free = free, popParam = paramMat, misspec = misspecMat, 
            symmetric = symmetric))
        
        ## SimVector
    } else if (is.vector(free)) {
        
        if (symmetric) {
            stop("A vector cannot be symmetric")
        }
        
        # popParam
        if (is.character(popParam) && length(popParam == 1)) {
            tryCatch(eval(parse(text = popParam)), error = function(e) stop(e))
            paramVec <- ifelse(is.free(free), popParam, "")
        } else if (is.numeric(popParam) && length(popParam) == 1) {
            paramVec <- ifelse(is.free(free), popParam, "")
        } else if (is.vector(popParam)) {
            if ((length(free) != length(popParam)) && length(popParam) > 1) 
                stop("Free vector and popParam are not the same length")
			popParam[!is.free(free)] <- ""			
            if (any(!is.empty(popParam) != is.free(free))) {
                stop("Please assign a value for any free parameters")
            }
            paramVec <- as.character(popParam)
        } else {
            paramVec <- vector()
        }
        
        # Misspec
        if (is.character(misspec) && length(misspec) == 1) {
            tryCatch(eval(parse(text = misspec)), error = function(e) stop(e))
            misspecVec <- ifelse(!is.free(free), misspec, "")
        } else if (is.numeric(misspec) && length(misspec) == 1) {
            misspecVec <- ifelse(!is.free(free), misspec, "")
        } else if (is.vector(misspec)) {
            if ((length(free) != length(misspec)) && length(misspec) > 1) 
                stop("Free vector and misspec are not the same length")
            misspecVec <- as.character(misspec)
        } else {
            misspecVec <- vector()
        }
        
        return(new("SimVector", free = free, popParam = paramVec, misspec = misspecVec))
    } else {
        stop("Please specify a free/fixed parameter matrix or vector.")
    }
}

binds <- function(free = NULL, popParam = NULL, misspec = NULL, symmetric = TRUE) {
    return(bind(free = free, popParam = popParam, misspec = misspec, symmetric = symmetric))
}



# Possible 'empty values': '', or NA
is.empty <- function(dat) {
    if (is.null(dim(dat))) {
        temp <- sapply(dat, FUN = function(x) if (x == "" || is.na(x)) {
            TRUE
        } else {
            FALSE
        })
        names(temp) <- NULL
        return(temp)
    }
    apply(dat, c(1, 2), FUN = function(x) if (x == "" || is.na(x)) {
        TRUE
    } else {
        FALSE
    })
    
}


# Finds valid labels, checks all combinations of label pairs to make sure at
# least one pair is the same.  Assumes that matrix has at least one character
# label.
validConstraints <- function(mat) {
    if (class(mat) == "SimMatrix" || class(mat) == "SimVector") {
        mat <- mat@free
    }
    
    labels <- is.label(mat)
    combs <- combn(labels[labels], 2)
    res <- combs[1, ] & combs[2, ]
    
    return(any(res))
}

is.label <- function(mat) {
    flat <- as.vector(mat)
    flat[is.na(flat)] <- 0
    isLabel <- sapply(flat, FUN = function(x) {
        suppressWarnings(is.na(as.numeric(x)))
    })
    return(isLabel)
}

is.free <- function(mat) {
    if (is.character(mat)) {
        isFree <- is.na(mat) | is.label(mat)
    } else {
        isFree <- is.na(mat)
    }
    return(isFree)
}

test.bind <- function() {
	# Test
	# 1. Name of testing
	# 2. Successful run?
	# 3. Have expected errors?
	# 4. Time elapsed
	result <- NULL

	# 1. Vector
	test.bind.SimVector <- function() {
		# SimVector
		v <- rep(0, 3)
		v[1] <- NA
		v[3] <- "a1"
		vVal <- c(0.5, 0, 1)
		obj1 <- bind(free = v, popParam = vVal)
		obj2 <- bind(free = v)
		obj3 <- bind(free = v, popParam = 0.5)
		obj4 <- bind(free = v, popParam = 0.5, misspec = "runif(1, 0, 0.1)")
		obj5 <- bind(free = v, misspec = "runif(1, 0, 0.1)")
		
		v2 <- rep("con1", 3)
		obj6 <- bind(free = v2, popParam = 0.5)
		obj7 <- bind(free = v2, popParam = vVal) # To do: Should have a warning here!

		errore1 <- try(obje1 <- bind(popParam = vVal), silent=TRUE) # Expect error
		
		print1 <- capture.output(summary(obj1))
		print2 <- capture.output(summaryShort(obj2))
		print3 <- capture.output(show(obj4))
		
		success <- all(sapply(c(paste0("obj", 1:7), paste0("print", 1:3)), exists))
		expecterror <- is(errore1, "try-error")
		return(list(success, expecterror))
	}
	error1 <- try(t1 <- system.time(temp <- test.bind.SimVector()), silent=TRUE)
	if(is(error1, "try-error")) {
		result <- rbind(result, c("Check *bind* on vector object", FALSE, FALSE, NA))
	} else {
		result <- rbind(result, c("Check *bind* on vector object", temp[[1]], temp[[2]], t1[3]))
	}
	
	test.bind.SimMatrix <- function() {
		# SimMatrix
		a <- matrix(0, 2, 2)
		a[1, 1] <- NA
		a[1, 2] <- 0
		a[2, ] <- "a1"
		aVal <- matrix(0, 2, 2)
		aVal[1, 1] <- 0.7
		aVal[2, ] <- 0.5
		
		obj1 <- bind(free = a, popParam = aVal)
		obj2 <- bind(free = a)
		obj3 <- bind(free = a, popParam = 0.5)
		obj4 <- bind(free = a, popParam = 0.5, misspec = "runif(1, 0, 0.1)")
		obj5 <- bind(free = a, misspec = "runif(1, 0, 0.1)")
		
		aVal2 <- matrix(0, 2, 2)
		aVal2[1, 1] <- 0.7
		aVal2[2, 1] <- 0.5
		aVal2[2, 2] <- 0.6
		obj6 <- bind(free = a, popParam = aVal2) # To do: Should have a warning here!

		aValMis <- matrix("runif(1, 0, 0.1)", 2, 2)
		obj7 <- bind(free = a, popParam = aVal2, misspec = aValMis)

		# Expect errors
		errore1 <- try(obje1 <- bind(popParam = aVal), silent=TRUE) # No free parameters
		errore2 <- try(obje2 <- bind(free = a, popParam = matrix(0, 3, 2)), silent=TRUE) # Different lengths
		errore3 <- try(obje3 <- bind(free = a, popParam = rep(1, 4)), silent=TRUE) # Different types
		
		print1 <- capture.output(summary(obj1))
		print2 <- capture.output(summaryShort(obj2))
		print3 <- capture.output(show(obj4))
		
		success <- all(sapply(c(paste0("obj", 1:7), paste0("print", 1:3)), exists))
		expecterror <- all(sapply(list(errore1, errore2, errore3), is, "try-error"))
		return(list(success, expecterror))
	}
	error2 <- try(t2 <- system.time(temp <- test.bind.SimMatrix()), silent=TRUE)
	if(is(error2, "try-error")) {
		result <- rbind(result, c("Check *binds* on matrix object", FALSE, FALSE, NA))
	} else {
		result <- rbind(result, c("Check *binds* on matrix object", temp[[1]], temp[[2]], t2[3]))
	}
	
	test.bind.SimMatrix.symmetric <- function() {
		# Symmetric SimMatrix
		s <- diag(3)
		s[2, 1] <- s[1, 2] <- "s1"
		s[2, 3] <- s[3, 2] <- "s1"
		s[1, 3] <- s[3, 1] <- 0
		
		sVal <- diag(3)
		sVal[2, 1] <- sVal[1, 2] <- 0.1
		sVal[2, 3] <- sVal[3, 2] <- 0.1
		sVal[1, 3] <- sVal[3, 1] <- 0
		
		obj1 <- binds(free = s, popParam = sVal)
		obj2 <- binds(free = s)
		obj3 <- binds(free = s, popParam = 0.5)
		obj4 <- binds(free = s, popParam = 0.5, misspec = "runif(1, 0, 0.1)")
		obj5 <- binds(free = s, misspec = "runif(1, 0, 0.1)")
		
		sVal2 <- diag(3)
		sVal2[2, 1] <- sVal2[1, 2] <- 0.1
		sVal2[2, 3] <- sVal2[3, 2] <- 0.1
		sVal2[1, 3] <- sVal2[3, 1] <- 0.5
		obj6 <- bind(free = s, popParam = sVal2) # To do: Should have a warning here!

		sMis <- matrix(0, 3, 3)
		sMis[1, 3] <- sMis[3, 1] <- "rnorm(1, 0, 0.1)"
		obj7 <- binds(free = s, popParam = 0.5, misspec = sMis)
		
		s2 <- matrix(paste0("s", 1:9), 3, 3)

		sVal3 <- diag(3)
		sVal3[2, 1] <- 0.2
		sVal3[1, 2] <- 0.1
		sVal3[2, 3] <- 0.2
		sVal3[3, 2] <- 0.1
		sVal3[1, 3] <- 0
		sVal3[3, 1] <- 0

		# Expect errors
		errore1 <- try(obje1 <- binds(popParam = sVal), silent=TRUE) # No free parameters
		errore2 <- try(obje2 <- binds(free = s, popParam = matrix(0, 4, 4)), silent=TRUE) # Different lengths
		errore3 <- try(obje3 <- binds(free = s, popParam = rep(1, 9)), silent=TRUE) # Different types
		errore4 <- try(obje4 <- binds(free = s2), silent=TRUE) # Nonsymmetric Matrix at free
		errore5 <- try(obje5 <- binds(free = s, popParam = sVal3), silent=TRUE) # Nonsymmetric Matrix at popParam
		
		print1 <- capture.output(summary(obj1))
		print2 <- capture.output(summaryShort(obj2))
		print3 <- capture.output(show(obj4))
		
		success <- all(sapply(c(paste0("obj", 1:7), paste0("print", 1:3)), exists))
		expecterror <- all(sapply(list(errore1, errore2, errore3, errore4, errore5), is, "try-error"))
		return(list(success, expecterror))
	}
	error3 <- try(t3 <- system.time(temp <- test.bind.SimMatrix.symmetric()), silent=TRUE)
	if(is(error3, "try-error")) {
		result <- rbind(result, c("Check *bind* on symmetric matrix object", FALSE, FALSE, NA))
	} else {
		result <- rbind(result, c("Check *bind* on symmetric matrix object", temp[[1]], temp[[2]], t3[3]))
	}
	
	colnames(result) <- c("condition", "success", "expecterror", "time")
	return(result)
    
} 
