\name{simEqualCon}
\alias{simEqualCon}
\title{
	Equality Constraint Object
}
\description{
	This function will be used to specify equality constraints. 
}
\usage{
simEqualCon(..., modelType)
}
\arguments{
  \item{\dots}{
	Each equality constraint in the model will be specified as a matrix. Rows represent elements that users wish to constrain.
	For single-group analysis, two columns are needed in the matrix. The first column indicates row of elements and second columns indicates
	columns of elements. Rownames will represent the matrix of elements that they are in. The detail section will discuss about how to specify row names.
	The first example shown below will show how to specify equality constraints for \code{LY(1,1)}, \code{LY(2,1)}, and \code{LY(3,1)}.
	For multiple groups, the columns will be three instead. The first column represent groups. The second and third columns represent row and column, respectively.
	The second example shown below will show how to specify equality constraints for \code{BE(2, 1)} of two groups.
	If you have multiple equality constriants, you can make multiple matrices to represent them and add in the function. See the third example for multiple constraints.
}
  \item{modelType}{
	Type of analysis: \code{CFA}, \code{Path}, \code{Path.exo}, \code{SEM}, or \code{SEM.exo}.
}
}
\details{
	Row names specification depends on type of model. If users specify CFA model, the specification in shown in \code{\link{simSetCFA}} function. 
	If users specify Path analysis with or without exogenous variables, the specification is shown in \code{\link{simSetPath}} function.
	If users specify SEM model with or without exogenous variables, the specification is shown in \code{\link{simSetSEM}} function. 
	However, basically, the names of matrices you put in these function are also eligible for this function as well.
}
\value{
	Object in \code{\linkS4class{SimEqualCon}} that save those equality constraints.
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\note{
	The available constraints now are equality constraints. We expect to create nonlinear constraints soon.
}
\seealso{
	\itemize{
		\item \code{\link{simSetCFA}} to see model specification in CFA model
		\item \code{\link{simSetPath}} to see model specification in Path analysis model
		\item \code{\link{simSetSEM}} to see model specification in SEM model
		\item \code{\linkS4class{SimEqualCon}} for the simResult
	}
}
\examples{
# Example 1: Single-group, one constraint
constraint <- matrix(0, 3, 2)
constraint[1,] <- c(1, 1)
constraint[2,] <- c(2, 1)
constraint[3,] <- c(3, 1)
rownames(constraint) <- rep("LY", 3)
equal.loading <- simEqualCon(constraint, modelType="SEM.exo")

# Example 2: Multiple-group, one constraint
group.con <- matrix(0, 2, 3)
group.con[1,] <- c(1, 2, 1)
group.con[2,] <- c(2, 2, 1)
rownames(group.con) <- rep("BE", 2)
equal.path <- simEqualCon(group.con, modelType="Path")

# Example 3: Single-group, multiple constraints
constraint1 <- matrix(1, 3, 2)
constraint1[,1] <- 1:3
rownames(constraint1) <- rep("LY", 3)
constraint2 <- matrix(2, 3, 2)
constraint2[,1] <- 4:6
rownames(constraint2) <- rep("LY", 3)
constraint3 <- matrix(3, 2, 2)
constraint3[,1] <- 7:8
rownames(constraint3) <- rep("LY", 2)
equal.loading2 <- simEqualCon(constraint1, constraint2, constraint3, modelType="SEM")
summary(equal.loading2)
}
