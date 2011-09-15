setClass("simData", 
	representation(
		Tag="character",
		N="numeric",
		Parameters="simMatrixSet",
		Misspecified="simMisspecifiedSet",
		Constraint="simConstraint",
		Constrain.Parameters.Only="logical",
		Misfit.bound="vector",
		Maximum.random="numeric"),
	prototype(
		Misspecified=.NULL.simMisspecifiedSet,
		Constraint=.NULL.simConstraint,
		Constrain.Parameters.Only=TRUE,
		Misfit.bound=.NULL.vector,
		Maximum.random=100)
)

data.object <- function(N, simMatrixSet, simMisspecifiedSet=.NULL.simMisspecifiedSet, simConstraint=.NULL.simConstraint, Constrain.Parameters.Only=TRUE, Misfit.bound=.NULL.vector, Maximum.random=100) {
	Tag <- simMatrixSet@Tag
	if(!is.null.object(simMisspecifiedSet)) {
		if(Tag != simMisspecifiedSet@Tag) stop("simMisspecifiedSet and simMatrixSet do not have the same tag")
	}
	if(!is.null.object(simConstraint)) {
		if(Tag != simConstraint@Tag) stop("simConstraint and simMatrixSet do not have the same tag")
	}
	if(!is.null.object(Misfit.bound)) {
		if(length(Misfit.bound) == 2) {
			if(Misfit.bound[1] >= Misfit.bound[2]) stop("The lower bound is higher than the upper bound")
		} else {
			stop("Misfit.bound must include only two numbers for lower and upper bound")
		}
	}
	return(new("simData", N=N, Tag=Tag, Parameters=simMatrixSet, Misspecified=simMisspecifiedSet,
		Constraint=simConstraint, Constrain.Parameters.Only=Constrain.Parameters.Only, Misfit.bound=Misfit.bound, Maximum.random=Maximum.random))
}

setMethod("summary", signature="simData", definition=function(object, detail=FALSE) {
		cat("DATA OBJECT\n")
		cat("Type\n")
		print(object@Tag)
		cat("Sample Size\n")
		print(object@N)
		cat("========= Parameters Set ============\n")
		summary(object@Parameters)
		cat("=====================================\n")
		if(detail) {
			cat("============Misspecified Set================\n")
			ifelse(!is.null.object(object@Misspecified), summary(object@Misspecified), print("None"))
			cat("============================================\n")
			cat("=============Constraint=====================\n")
			ifelse(!is.null.object(object@Constraint), summary(object@simConstraint), print("None"))
			cat("============================================\n")
		} else {
			cat("Adding Misspecification?\n")
			ifelse(!is.null.object(object@Misspecified), print("Yes"), print("No"))
			cat("Adding Constraint?\n")
			ifelse(!is.null.object(object@Constraint), print("Yes"), print("No"))
		}
		if(!is.null.object(object@Misspecified) & !is.null.object(object@Constraint)) {
			cat("Constain objects BEFORE or AFTER adding misspecification\n")
			ifelse(object@Constrain.Parameters.Only, print("Before"), print("After"))
		}
		if(!is.null.object(object@Misspecified)) {
			cat("Misfit bound\n")
			if(!is.null.object(object@Misfit.bound)) {
				print(paste("Lower =", object@Misfit.bound[1]))
				print(paste("Upper =", object@Misfit.bound[2]))
			} else {
				print("No")
			}
		}
		cat("Maximum Random Sampling Parameters\n")
		print(object@Maximum.random)
	}
)

# Chi.square <- function(observed.M, observed.CM, implied.M, implied.CM, N) {
	# n <- length(observed.M)
	# inv <- solve(implied.CM)
	# dis.CM <- observed.CM %*% inv
	# t.1 <- sum(diag(dis.CM))
	# t.2 <- log(det(dis.CM))
	# dis.M <- as.matrix(observed.M - implied.M)
	# t.3 <- t(dis.M) %*% inv %*% dis.M
	# result <- (N - 1) * (t.1 - t.2 - n + t.3)
	# return(result)
# }

# RMSEA <- function(Chi.square, N, degree.of.freedom) {
	# numer <- (Chi.square / degree.of.freedom) - 1
	# denom <- N - 1
	# result <- sqrt(numer/denom)
	# return(result)
# }

average.misfit <- function(observed.M, observed.CM, implied.M, implied.CM, degree.of.freedom) { 
#Should be renamed to average discrepancy; df is changed to added information
	p <- length(observed.M)
	inv <- solve(implied.CM)
	dis.CM <- observed.CM %*% inv
	t.1 <- sum(diag(dis.CM))
	t.2 <- log(det(dis.CM))
	dis.M <- as.matrix(observed.M - implied.M)
	t.3 <- t(dis.M) %*% inv %*% dis.M
	F.statistic <- t.1 - t.2 - p + t.3
	result <- sqrt(F.statistic/degree.of.freedom)
	return(result)
}

###################### Should include Fan & Sivo method here

setMethod("run", signature="simData", definition=function(object, N=NULL) {
	if(!require(MASS)) stop("Please install MASS package")
	Tag <- object@Tag
	if(is.null(N)) N <- object@N
	Parameters <- NULL
	Misspecified <- NULL
	implied.CM.Parameters <- NULL
	implied.CM.Misspecified <- NULL
	misfit <- NULL
	count <- 0
	repeat {
		#browser()
		if(!is.null.object(object@Misspecified)) {
			Output <- run.misspecified(object@Parameters, object@Misspecified, object@Constraint, object@Constrain.Parameters.Only)
			Parameters <- Output$Parameters
			Misspecified <- Output$Misspecified
			if(validate.object(Parameters) | validate.object(Misspecified)) {
				Parameters <- reduce.matrices(Parameters)
				Misspecified <- reduce.matrices(Misspecified)
				implied.CM.Parameters <- create.implied.MACS(Parameters)
				implied.CM.Misspecified <- create.implied.MACS(Misspecified)
				misfit <- average.misfit(implied.CM.Misspecified$M, implied.CM.Misspecified$CM, 
					implied.CM.Parameters$M, implied.CM.Parameters$CM, count.random.object(object@Misspecified))
				Parameters <- Misspecified # Pretend Misspecified as real parameters for data generation
				if(is.null.object(object@Misfit.bound)) break
				if(misfit > object@Misfit.bound[1] & misfit < object@Misfit.bound[2]) break
			}
		} else {
			Parameters <- run(object@Parameters, simConstraint=object@Constraint)
			if(validate.object(Parameters)) {
				Parameters <- reduce.matrices(Parameters)
				implied.CM.Parameters <- create.implied.MACS(Parameters)
				implied.CM.Misspecified <- implied.CM.Parameters
				break
			}
		}
		count <- count + 1
		if(count > object@Maximum.random) stop("The model cannot make a good set of parameters within limit of maximum random sampling of parameters")
	}
	# if(Tag == "CFA") {
		# factor.score <- mvrnorm(N, Parameters@AL, Parameters@PS)
		# error.score <- mvrnorm(N, rep(0, length(Parameters@TY), Parameters@TE)
		# intercept <- as.data.frame(matrix(Parameters@TY, ncol=length(Parameters@TY), byrow=TRUE))
		# Data <- (factor.score %*% t(Parameters@LY)) + error.score + intercept
	# } else if (Tag == "Path") {
		# error.score <- mvrnorm(N, Parameters@AL, Parameters@PS)
		# ID <- matrix(0, nrow(Parameters@BE), ncol(Parameters@BE))
		# diag(ID) <- 1
		# data <- error.score %*% t(solve(ID - Parameters@BE))
	# } else if (Tag == "Path.exo") {
	
	# } else if (Tag == "SEM") {
	
	# } else if (Tag == "SEM.exo") {
	
	# }
	Data <- mvrnorm(N, implied.CM.Parameters$M, implied.CM.Parameters$CM)
	varnames <- NULL
	if(Tag == "Path.exo") {
		nx <- ncol(run(object@Parameters@PH))
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else if(Tag == "SEM.exo") {
		nx <- nrow(run(object@Parameters@LX))
		for(i in 1:nx) {
			temp <- paste("x", i, sep="")
			varnames <- c(varnames, temp)
		}	
		for(i in 1:(ncol(Data) - nx)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	} else {
		for(i in 1:ncol(Data)) {
			temp <- paste("y", i, sep="")
			varnames <- c(varnames, temp)
		}
	}
	colnames(Data) <- varnames
	return(Data)
})

# Make data object to remember amount of misfit and parameters and labels it as appropriate

