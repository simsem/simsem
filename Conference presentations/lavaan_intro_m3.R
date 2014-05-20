##Example from introduction to Exploring the lavaan ecosystem
##Symposiom presented at Modern Modeling Methods, 2014
##Example data from Little, 2013

##Alexander Schoemann

library(lavaan)

################################################################################
## data preparation
################################################################################
dat <- matrix(c(1, .61251, .61733,
				.61251, 1, .65757,
				.61733, .65757, 1), 3, 3)

rownames(dat) <-c( "Great", "Cheerful", "Happy")
colnames(dat) <-c( "Great", "Cheerful", "Happy")

nObs <- 100

################################################################################
## Example 
################################################################################
mod <- '
Positive =~ Great + Cheerful + Happy 
Positive ~~ 1*Positive #Will be included by default
Great ~~ Great #Will be included by default
Cheerful ~~ Cheerful #Will be included by default
Happy ~~ Happy #Will be included by default
'

fit <- lavaan(mod, data=dat std.lv=TRUE)

summary(fit, fit.measures=TRUE)