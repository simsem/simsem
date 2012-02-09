\name{miPool}
\alias{miPool}
\title{
Function to pool imputed results
}
\description{
The function takes a list of imputed results (parameters, standard errors and fit indices)
and returns pooled parameter estimates, standard errors, and fit indices.
}
\usage{
miPool(imputed.results, imps)
}
\arguments{
  \item{imputed.results}{
A simmodel object with results from each imputed data set
}
  \item{imps}{
The number of imputations used.
}
}
\details{
Parameters and standard errors are combined using Rubin's Rules (Rubin, 1987). Fit indices are averaged across imputations.
}
\value{
MIpool returns a list with pooled estimates, standard errors, fit indices and fraction missing information
\item{Estimates}{Pooled parameter estimates. The order of parameter estimates corresponds to the order reported by Lavaan}
\item{SE}{Pooled standard errors. The order of standard errors corresponds to the order reported by Lavaan}
\item{Fit.indices}{Pooled fit indices. The order of fit indices corresponds to the order reported by Lavaan}
\item{FMI.1}{Fraction of missing information for each parameter. The order of fraction missing corresponds to the order of parameters reported by Lavaan}
\item{FMI.2}{Fraction of missing information for each parameter. The order of fraction missing corresponds to the order of parameters reported by Lavaan}
}
\references{
Rubin, D.B. (1987) Multiple Imputation for Nonresponse in Surveys. J. Wiley & Sons, New York.
}
\author{
Alexander M. Schoemann (University of Kansas; \email{schoemann@ku.edu})
Mijke Rhemtulla (University of Kansas; \email{mijke@ku.edu})
}
\seealso{
	\itemize{
		\item \code{\link{runMI}} for imputing missing values by multiple imputation and analyzing the imputed datasets.
	}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(imputed.results,imps){

MI.param<-matrix(NA,nrow=length(imputed.results),ncol=length(imputed.results[[1]]@Estimates))
MI.se<-matrix(NA,nrow=length(imputed.results),ncol=length(imputed.results[[1]]@SE))
MI.fit<-matrix(NA,nrow=length(imputed.results),ncol=length(imputed.results[[1]]@Fit))

for(i in 1:length(imputed.results)){
MI.param[i,]<-unlist(imputed.results[[i]]@Estimates)
MI.se[i,]<-unlist(imputed.results[[i]]@SE)
MI.fit[i,]<-unlist(imputed.results[[i]]@Fit)
  }

#Need to remove columns representing fixed parameters
MI.param <- MI.param[ , colMeans( MI.param==0 ) == 0, drop=FALSE ]
MI.param <- MI.param[ , colMeans( MI.param==1 ) == 0, drop=FALSE ]
MI.se <- MI.se[ , colSums( MI.se==0 ) == 0, drop=FALSE ]

#compute parameter estimates
Estimates <- colMeans(MI.param)

#compute between-imputation variance: variance of parameter estimates
Bm <- apply(MI.param,2,var)



#compute within-imputation variance: average of squared estimated SEs 
#Um <- colSums(MI.se^2/m)
Um <- apply(MI.se^2,2,mean)

#Total variance
#Tm <- Um + (Bm)*((1+m)/m+1)

#compute total variance: sum of between- and within- variance with correction
SE <- Um + ((imps+1)/imps)*Bm

#compute correction factor for fraction of missing info
nu <- (imps-1)*((((1+1/imps)*Bm)/SE)^-2)

#compute 2 estimates of fraction of missing information
FMI.1 <- 1-(Um/SE)
FMI.2 <- 1- ((nu+1)*Um)/((nu+3)*SE)
FMI<-rbind(FMI.1,FMI.2)

#compute average fit index estimates (only some of these will be interpretable!)
Fit.indices <- colMeans(MI.fit)

MI.res<-list(Estimates,SE,Fit.indices,FMI.1,FMI.2)
names(MI.res)<-c('Estimates','SE','Fit.indices','FMI.1','FMI.2')
#compute chi-square proportion (is this useful?)
#(MI.fit.mat$chisq.p is a placeholder for however we'll index the p-value of chi square)
#chisq <- sum(MI.fit.mat$chisq.pval<.05)/m
return(MI.res)
  }
}

