library(simsem)

popModel <- "
f1 =~ 0.65*y1 + 0.65*y2 + 0.65*y3
f2 =~ 0.65*y4 + 0.65*y5 + 0.65*y6
f3 =~ 0.65*y7 + 0.65*y8 + 0.65*y9
f4 =~ 0.4*y1 + 0.4*y4 + 0.4*y7
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
f4 ~~ 1*f4
f1 ~~ 0.4*f2
f1 ~~ 0.2*f3
f2 ~~ 0.3*f3
f1 ~~ 0*f4
f2 ~~ 0*f4
f3 ~~ 0*f4
y1 ~~ 2*y1
y2 ~~ 0.5*y2
y3 ~~ 0.5*y3
y4 ~~ 0.5*y4
y5 ~~ 0.5*y5
y6 ~~ 0.5*y6
y7 ~~ 0.5*y7
y8 ~~ 0.5*y8
y9 ~~ 0.5*y9
"

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f3 =~ y7 + y8 + y9
f4 =~ y1 + y4 + y7
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
f4 ~~ 1*f4
f1 ~~ 0*f4
f2 ~~ 0*f4
f3 ~~ 0*f4
f1 ~~ f2
f1 ~~ f3
f2 ~~ f3
"

miss.model <- miss(pmMCAR=0.2, m=5) 

Output <- sim(1000, n=500, model=analyzeModel, generate=popModel, lavaanfun = "cfa", miss=miss.model)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
