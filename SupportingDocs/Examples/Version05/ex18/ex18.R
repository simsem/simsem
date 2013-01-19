library(simsem)

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.51*y2
y3 ~~ 0.51*y3
y4 ~~ 0.51*y4
y5 ~~ 0.51*y5
y6 ~~ 0.51*y6
"

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"

Output <- sim(NULL, n=50:1000, model = analyzeModel, generate = popModel, std.lv = TRUE, lavaanfun = "cfa")
summary(Output)
plotCutoff(Output, 0.05)
getCutoff(Output, 0.05, nVal = 200)	

Cpow <- getPower(Output)
Cpow2 <- getPower(Output, nVal = 200)
findPower(Cpow, "N", 0.80)
plotPower(Output, powerParam=c("f1=~y1", "f1~~f2"))
