
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
