library(simsem)

popNull <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3 + 0.7*y4 + 0.7*y5
f2 =~ 0.7*y6 + 0.7*y7 + 0.7*y8
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.51*y2
y3 ~~ 0.51*y3
y4 ~~ 0.51*y4
y5 ~~ 0.51*y5
y6 ~~ 0.51*y6
y7 ~~ 0.51*y7
y8 ~~ 0.51*y8
"

popAlt <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3 + 0.7*y4
f2 =~ 0.7*y5 + 0.7*y6 + 0.7*y7 + 0.7*y8
f1 ~~ 1*f1
f2 ~~ 1*f2
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.51*y2
y3 ~~ 0.51*y3
y4 ~~ 0.51*y4
y5 ~~ 0.51*y5
y6 ~~ 0.51*y6
y7 ~~ 0.51*y7
y8 ~~ 0.51*y8
"

analyzeNull <- "
f1 =~ y1 + y2 + y3 + y4 + y5
f2 =~ y6 + y7 + y8
"

Output.NULL <- sim(NULL, n = 25:500, analyzeNull, generate = popNull, std.lv = TRUE, lavaanfun = "cfa")
Output.ALT <- sim(NULL, n = 25:500, analyzeNull, generate = popAlt, std.lv = TRUE, lavaanfun = "cfa")

cutoff <- getCutoff(Output.NULL, alpha = 0.05, nVal = 250)
plotCutoff(Output.NULL, alpha = 0.05)
getPowerFit(Output.ALT, nullObject = Output.NULL, alpha = 0.05, nVal = 250)
getPowerFit(Output.ALT, cutoff = cutoff, nVal = 250, condCutoff = TRUE)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05)
plotPowerFit(Output.ALT, Output.NULL, alpha = 0.05, logistic = FALSE)

cutoff2 <- c(RMSEA = 0.05, CFI = 0.95, TLI = 0.95, SRMR = 0.06)
getPowerFit(Output.ALT, cutoff = cutoff2, nVal = 250, condCutoff = FALSE)
plotPowerFit(Output.ALT, cutoff = cutoff2)
plotPowerFit(Output.ALT, cutoff = cutoff2, logistic = FALSE)
