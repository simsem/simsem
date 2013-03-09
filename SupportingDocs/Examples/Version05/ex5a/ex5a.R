library(simsem)

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6
f3 =~ con1*y7 + con1*y8 + 0.6*y7 + 0.6*y8 
f3 ~ 0.6*f1 + 0.3*f2
f1 ~~ 0.5*f2
"

dat<-simulateData(popModel, sample.nobs=300, standardized=TRUE,return.fit=TRUE)
parTable(attr(dat,"fit"))
# F3 variance is wrong: 1 - 0.6^2 - 0.3^2 - (0.6*0.3*0.5)

