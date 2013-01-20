library(simsem)

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3 + 0.7*y4
f1 ~~ 1*f1
y1 | 0.5*t1
y2 | 0.25*t1
y3 | 0*t1
y4 | -0.5*t1
"

analyzeModel <- "
f1 =~ y1 + y2 + y3 + y4
"

Output <- sim(1000, n = 200, model = analyzeModel, generate = popModel, lavaanfun = "cfa", std.lv = TRUE, ordered = c("y1", "y2", "y3", "y4"))
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)

Output2 <- sim(NULL, n = 50:500, model = analyzeModel, generate = popModel, lavaanfun = "cfa", std.lv = TRUE, ordered = c("y1", "y2", "y3", "y4"))
getCutoff(Output2, 0.05, nVal = 250)
plotCutoff(Output2, 0.05)
summary(Output2)
