\name{plotDist}
\alias{plotDist}
\title{
	Plot a distribution of a data distribution object
}
\description{
	Plot a distribution of a data distribution object
}
\usage{
plotDist(object, xlim = NULL, ylim = NULL, r = 0, var = NULL, contour = TRUE)
}
\arguments{
  \item{object}{
	The data distribution object (\code{\linkS4class{SimDataDist}}) to plot a distribution 
}
  \item{xlim}{
	A numeric vector with two elements specifying the lower and upper limit of the x-axis to be plotted. 
}
  \item{ylim}{
	A numeric vector with two elements specifying the lower and upper limit of the y-axis to be plotted. This argument is applicable for the joint distribution of two dimensions only
}
  \item{r}{
	The correlation of two dimensions in the joint distribution
}
  \item{var}{
	A vector of the index of variables to be plotted. The length of vector cannot be greater than 2.
}
  \item{contour}{
	Applicable if two variables are used only. If TRUE, the contour plot is provided. If FALSE, the perspective plot is provided.
}
}
\value{
	No return value. This function will plot a graph only.
}
\author{
    Sunthud Pornprasertmanit (Texas Tech University; \email{sunthud.pornprasertmanit@ttu.edu})
}
\seealso{
	\itemize{
		\item \code{\linkS4class{SimDataDist}} for plotting a data distribution object
	}
}
\examples{
datadist <- bindDist(c("chisq", "t", "f"), list(df=5), list(df=3), list(df1=3, df2=5))

# Plot the joint distribution of Variables 1 and 2 with correlation of 0.5
plotDist(datadist, r=0.5, var=1:2)

# Plot the marginal distribution of the variable 3
plotDist(datadist, var=3)

datadist2 <- bindDist(skewness = c(0, -2, 2), kurtosis = c(2, 4, 4))

# Plot the joint distribution of Variables 1 and 2 with correlation of 0.5
plotDist(datadist2, r=0.5, var=1:2)

# Plot the marginal distribution of the variable 3
plotDist(datadist2, var=3)
}
