\name{indProd}
\alias{indProd}
\title{
	Make a product of indicators using mean centering or double-mean centering
}
\description{
This function will use mean centering or double is a constrctor of a function object which can be used for data transformation. The aim of the object is to create a function but will use later in a simulation study. For example, set up a mean centering for a dataset for using in a simulation.
}
\usage{
indProd(data, var1, var2, match = TRUE, meanC = TRUE, residualC = FALSE, doubleMC = TRUE, namesProd = NULL)
}
\arguments{
  \item{data}{
	The desired data to be transformed.
}
  \item{var1}{
	Names or indices of the variables loaded on the first factor
}
  \item{var2}{
	Names or indices of the variables loaded on the second factor
}
  \item{match}{
	Specify \code{TRUE} to use match-paired approach (Marsh, Wen, & Hau, 2004). If \code{FALSE}, the resulting products are all possible products.
}
  \item{meanC}{
	Specify \code{TRUE} for mean centering the main effect indicator before making the products
}
  \item{residualC}{
	Specify \code{TRUE} for residual centering the products by the main effect indicators (Little, Bovaird, & Widaman, 2006).
}
  \item{doubleMC}{
	Specify \code{TRUE} for centering the resulting products (Lin et. al., 2010)
}
  \item{namesProd}{
	The names of resulting products
}
}
\value{
	The original data attached with the products.
}
\references{
	\itemize{
		\item Marsh, H. W., Wen, Z. & Hau, K. T. (2004). Structural equation models of latent interactions: Evaluation of alternative estimation strategies and indicator construction. \emph{Psychological Methods, 9,} 275-300.
		\item Lin, G. C., Wen, Z., Marsh, H. W., & Lin, H. S. (2010). Structural equation models of latent interactions: Clarification of orthogonalizing and double-mean-centering strategies. \emph{Structural Equation Modeling, 17}, 374-391.
		\item Little, T. D., Bovaird, J. A., & Widaman, K. F. (2006). On the merits of orthogonalizing powered and product terms: Implications for modeling interactions among latent variables. \emph{Structural Equation Modeling, 13}, 497-519.
	}
}
\author{
    Sunthud Pornprasertmanit (University of Kansas; \email{psunthud@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{simFunction}} to use this function within a simulation study
	}
}
\examples{
dat <- indProd(attitude[,-1], var1=1:3, var2=4:6)
}
