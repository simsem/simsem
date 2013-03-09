library(simsem)

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6
f1 ~~ 0.5*f2
"

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
"

Output <- sim(1000, analyzeModel, n=200, generate=list(model = popModel, standardized = TRUE), lavaanfun = "cfa", std.lv=TRUE)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
summaryFit(Output)
summaryParam(Output)
summaryParam(Output, detail=TRUE)
