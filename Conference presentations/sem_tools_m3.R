##Example from semTools in Exploring the lavaan ecosystem
##Symposiom presented at Modern Modeling Methods, 2014


##Alexander Schoemann

library(lavaan)
library(semTools)

##Example
#Specify model (based on lavaan example)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

#Create missing data in the HS data. Use a MCAR process
HSMiss <- HolzingerSwineford1939[,paste("x", 1:9, sep="")]
randomMiss <- rbinom(prod(dim(HSMiss)), 1, 0.1)
randomMiss <- matrix(as.logical(randomMiss), nrow=nrow(HSMiss))
HSMiss[randomMiss] <- NA

#Fit model with runMI
#HSMIss is data set with 10% missing MCAR
out <- runMI(HS.model, data=HSMiss, m = 20, chi="all", fun = "cfa")
summary(out)
inspect(out, "fit")
#Provides fraction of missing information and fit for each chi square type
inspect(out, "impute")

##measurementInvariance example

HW.model <- ' visual =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed =~ x7 + x8 + x9 '

measurementInvariance(HW.model, data=HolzingerSwineford1939, group="school")


Measurement invariance tests:

Model 1: configural invariance:
   chisq       df   pvalue      cfi    rmsea      bic 
 115.851   48.000    0.000    0.923    0.097 7706.822 

Model 2: weak invariance (equal loadings):
   chisq       df   pvalue      cfi    rmsea      bic 
 124.044   54.000    0.000    0.921    0.093 7680.771 

[Model 1 versus model 2]
  delta.chisq      delta.df delta.p.value     delta.cfi 
        8.192         6.000         0.224         0.002 

Model 3: strong invariance (equal loadings + intercepts):
   chisq       df   pvalue      cfi    rmsea      bic 
 164.103   60.000    0.000    0.882    0.107 7686.588 

[Model 1 versus model 3]
  delta.chisq      delta.df delta.p.value     delta.cfi 
       48.251        12.000         0.000         0.041 

[Model 2 versus model 3]
  delta.chisq      delta.df delta.p.value     delta.cfi 
       40.059         6.000         0.000         0.038 

Model 4: equal loadings + intercepts + means:
   chisq       df   pvalue      cfi    rmsea      bic 
 204.605   63.000    0.000    0.840    0.122 7709.969 

[Model 1 versus model 4]
  delta.chisq      delta.df delta.p.value     delta.cfi 
       88.754        15.000         0.000         0.083 

[Model 3 versus model 4]
  delta.chisq      delta.df delta.p.value     delta.cfi 
       40.502         3.000         0.000         0.042 
