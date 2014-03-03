library(simsem)
library(lavaan)

pop.model<-'
# regression
Y ~  0.66*X1 + 0.05*X2 + -0.30*X3

# predictor correlations
X1~~ 0.40*X2 + 0.60*X3
X2~~ 0.05*X3

# error variance
Y~~ 0.69*Y
'

analysis.model <- '
Y ~  X1 + X2 + X3
'

analysis.n <- sim(nRep = NULL, model=analysis.model, n = 200:500, generate=pop.model, lavaanfun = "sem", multicore=FALSE,fixed.x = FALSE)


# This gives me the CI widths
summaryParam(analysis.n, detail=TRUE)

# These both return error messages
getCIwidth(analysis.n, assurance = 0.95, nVal = 210) 
plotCIwidth(analysis.n, c("Y~X1"), assurance = 0.95)
