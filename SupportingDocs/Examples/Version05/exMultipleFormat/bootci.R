library(simsem)

popModel <- "
f1 =~ 0.6*y1 + 0.6*y2 + 0.6*y3 + 0.6*y4
y1 ~~ 0.5*y1
y2 ~~ 0.5*y2
y3 ~~ 0.5*y3
y4 ~~ 0.5*y4
f1 ~~ 1*f1
"

analyzeModel <- "
f1 =~ y1 + y2 + y3 + y4
"

Output <- sim(1000, n = 200, model = analyzeModel, generate = popModel, lavaanfun = "cfa", std.lv = TRUE, se = "boot")
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
getCoverage(Output)
getCoverage(Output, coverValue = 0)
getCIwidth(Output, assurance = 0.50)
getCIwidth(Output, assurance = 0.90)
plotCIwidth(Output, "f1=~y2", assurance = 0.50)
plotCIwidth(Output, "f1=~y2", assurance = 0.90)

Output2 <- sim(NULL, n = 50:500, model = analyzeModel, generate = popModel, lavaanfun = "cfa", std.lv = TRUE, se = "boot")
getCutoff(Output2, 0.05, nVal = 250)
plotCutoff(Output2, 0.05)
summary(Output2)
getCoverage(Output2, nVal = 200)
cover <- getCoverage(Output2, coverValue = 0)
findCoverage(cover, "N", 0.05)
plotCoverage(Output2, "f1=~y2")
plotCoverage(Output2, "f1=~y2", coverValue = 0)
getCIwidth(Output2, assurance = 0.50, nVal = 200)
getCIwidth(Output2, assurance = 0.90, nVal = 200)
plotCIwidth(Output2, "f1=~y2", assurance = 0.50)
plotCIwidth(Output2, "f1=~y2", assurance = 0.90)
