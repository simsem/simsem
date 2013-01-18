library(simsem)
library(lavaan)

model <- ' 
	ind60 =~ x1 + x2 + x3
	dem60 =~ y1 + a*y2 + b*y3 + c*y4
	dem65 =~ y5 + a*y6 + b*y7 + c*y8

	dem60 ~ ind60
	dem65 ~ ind60 + dem60

	y1 ~~ y5
	y2 ~~ y4 + y6
	y3 ~~ y7
	y4 ~~ y8
	y6 ~~ y8
'

usedData <- imposeMissing(PoliticalDemocracy, pmMCAR=0.03)
fit <- sem(model, data=usedData)

misstemplate <- miss(logical=is.na(usedData))
Output <- sim(1000, n=nrow(usedData), model=fit, generate=fit, miss=misstemplate)
pValue(fit, Output)
