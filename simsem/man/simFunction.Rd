\name{simFunction}
\alias{simFunction}
\title{
	Create function object
}
\description{
This function is a constrctor of a function object which can be used for data transformation. The aim of the object is to create a function but will use later in a simulation study. For example, set up a mean centering for a dataset for using in a simulation.
}
\usage{
simFunction(fun, ...)
}
\arguments{
  \item{fun}{
	The desired function that will be used for data transformation
}
  \item{\dots}{
	Additional arguments of the desired function.
}
}
\value{
	\code{\linkS4class{SimFunction}} that saves the function to use later in the simulation (within \code{\link{simResult}})
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimResult}} for how to use the \code{simFunction} in a simulation study
	}
}
\examples{
# The example still does not work
loading <- matrix(0, 9, 3)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
loading[7:9, 3] <- NA
loading.start <- matrix("", 9, 3)
loading.start[1:3, 1] <- 0.7
loading.start[4:6, 2] <- 0.7
loading.start[7:9, 3] <- "runif(1, 0.6, 0.8)"

loading.trivial <- matrix("runif(1, -0.2, 0.2)", 9, 3)
loading.trivial[is.na(loading)] <- 0

LY <- bind(loading, loading.start, misspec=loading.trivial)

error.cor.trivial <- matrix("rnorm(1, 0, 0.1)", 9, 9)
diag(error.cor.trivial) <- 0

RTE <- binds(diag(9), misspec=error.cor.trivial)

factor.cor <- diag(3)
factor.cor[1, 2] <- factor.cor[2, 1] <- NA
RPS <- binds(factor.cor, 0.5)

path <- matrix(0, 3, 3)
path[3, 1:2] <- NA
path.start <- matrix(0, 3, 3)
path.start[3, 1] <- "rnorm(1, 0.6, 0.05)"
path.start[3, 2] <- "runif(1, 0.3, 0.5)"
BE <- bind(path, path.start)

datGen <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

#loading <- matrix(0, 12, 4)
#loading[1:3, 1] <- NA
#loading[4:6, 2] <- NA
#loading[7:9, 4] <- NA
#loading[10:12, 3] <- NA

#path <- matrix(0, 4, 4)
#path[4, 1:3] <- NA

#analysis <- simParamSEM(BE=path, LY=loading)

#Model <- simModel(analysis)

# Find the products of indicators
#newFUN <- function(data, var1, var2, namesProd) {
#	prod <- data[,var1] * data[,var2]
#	colnames(prod) <- namesProd
#	return(data.frame(data, prod))
#}

#fun <- simFunction(newFUN, var1=paste("y", 1:3, sep=""), var2=paste("y", 4:6, sep=""), namesProd=paste("y", 10:12, sep=""))

# Real simulation will need more than just 10 replications
#Output <- simResult(10, Data.Mis, Model, objFunction=fun)
#summary(Output)
}
