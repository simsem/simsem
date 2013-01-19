library(simsem)

popModel <- "
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
i ~ 0.5*x
s ~ 0.1*x
i ~~ 1*i
s ~~ 0.25*s
i ~~ 0.05*s
x ~~ 0.25*x
x ~ 0.5*1
i ~ 5*1
s ~ 2*1
t1 ~ 0*1
t2 ~ 0*1
t3 ~ 0*1
t4 ~ 0*1
t1 ~~ 1.2*t1
t2 ~~ 1.2*t2
t3 ~~ 1.2*t3
t4 ~~ 1.2*t4
"

analyzeModel <- "
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
i ~ x
s ~ x
"

pois <- list(lambda = 3)
n01 <- list(mean=0, sd=1)
indDist <- bindDist(c("norm", "norm", "norm", "norm", "pois"), n01, n01, n01, n01, pois, keepScale=c(TRUE, TRUE, TRUE, TRUE, FALSE))

Output <- sim(NULL, n=50:1000, pmMCAR=seq(0, 0.4, 0.1), indDist=indDist, model = analyzeModel, generate = list(model = popModel, fixed.x = FALSE), fixed.x = FALSE, lavaanfun = "growth")
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200, pmMCARval = 0)
getCutoff(Output, 0.05, nVal = 300, pmMCARval = 0.33)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200, pmMCARval = 0.35)
findPower(Cpow, "N", 0.80)
findPower(Cpow, "MCAR", 0.80)
plotPower(Output, powerParam=c("i~x", "s~x"))
