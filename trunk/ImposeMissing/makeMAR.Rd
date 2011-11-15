\name{makeMAR}
\alias{makeMAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to make Missing at Random (MAR) missing data based on 2 covariates
}
\description{
Function to make Missing at Random (MAR) missing data based on 2 covariates
}
\usage{
makeMAR(data, pm, covs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Raw data, either matrix or data frame
}
  \item{pm}{
Percent of missing data wanted
}
  \item{covs}{
Indices of covariates (column numbers corresponding to covariates)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Logical matrix of items to be deleted. Values marked TRUE will be deleted.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
?? Patrick, Kyle?
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(data,pm,covs)
  {
    
    len.scale <- dim(data)[2]
    
    mar.pred1 <- covs[1]
    mar.pred2 <- covs[2]
    dat <- matrix(FALSE,dim(data)[1],dim(data)[2])
    
    ## Create a vector of small deviations 1/2 the length of your scales
    Y <- runif(len.scale*.5,0,.25*pm)

    ## Bind Y with -Y and add pm to each element
       ## Permute the elements just described to get a vector whose elements will average to pm
    Z <- sample(c(Y+pm,-Y+pm), size=len.scale, replace=F)

    ## fun1 asks the question:
    ## Is the prob. associated with the current value of some covariate (normally dist. attached to the dataset) <= x
       ## x = an element of Z (from above)
    fun1 <- function(x,dat) pnorm(dat,mean(dat),sd(dat)) <= x

    # THIS INDEXING IS GOING TO BE REAL - run it through the interpreter to verify before making changes.
    # Basically, this divides the items (columns) into 2 groups that don't include the covariates
    c <- (1:len.scale)[-covs]
    g1 <- c[1:(length(c)/2)]
    g2 <- c[((length(c)/2 + 1 - length(c)\%\% 2):length(c))]

      
    ## Apply fun1 to half of the variables using one covariate and to the other half using a different covariate
       ## Because Z is m elements long and our covariate is p elements long cbind(R1, R2) will total p x m
    R1 <- sapply(Z[g1],fun1,dat=data[,mar.pred1])
    R2 <- sapply(Z[g2],fun1,dat=data[,mar.pred2])

    dat[,g1] <- R1
    dat[,g2] <- R2
        
    return(dat)
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
