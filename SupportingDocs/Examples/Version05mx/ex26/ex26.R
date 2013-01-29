library(simsem)

popNested <- "
y2 ~ 0.4*y1 + con*y1
y3 ~ 0.4*y2 + con*y2
y4 ~ 0.4*y3 + con*y3
y5 ~ 0.4*y4 + con*y4
y1 ~~ 1*y1
y2 ~~ 0.8*y2
y3 ~~ 0.8*y3
y4 ~~ 0.8*y4
y5 ~~ 0.8*y5
"

popParent <- "
y2 ~ 0.4*y1
y3 ~ 0.5*y2
y4 ~ 0.3*y3
y5 ~ 0.7*y4
y1 ~~ 1*y1
y2 ~~ 0.8*y2
y3 ~~ 0.8*y3
y4 ~~ 0.8*y4
y5 ~~ 0.8*y5
"

analyzeNested <- "
y2 ~ con*y1
y3 ~ con*y2
y4 ~ con*y3
y5 ~ con*y4
"

analyzeParent <- "
y2 ~ y1
y3 ~ y2
y4 ~ y3
y5 ~ y4
"

outDatNestedModNested <- sim(NULL, n = 50:500, analyzeNested, generate = list(model = popNested, fixed.x = FALSE), lavaanfun = "sem", pmMCAR=seq(0, 0.3, 0.1))
outDatNestedModParent <- sim(NULL, n = 50:500, analyzeParent, generate = list(model = popNested, fixed.x = FALSE), lavaanfun = "sem", pmMCAR=seq(0, 0.3, 0.1))

anova(outDatNestedModNested, outDatNestedModParent)

cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent, nVal=250, pmMCARval=0.2)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(NULL, n = 50:500, analyzeNested, generate = list(model = popParent, fixed.x = FALSE), lavaanfun = "sem", pmMCAR=seq(0, 0.3, 0.1))
outDatParentModParent <- sim(NULL, n = 50:500, analyzeParent, generate = list(model = popParent, fixed.x = FALSE), lavaanfun = "sem", pmMCAR=seq(0, 0.3, 0.1))

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, nVal=250, pmMCARval=0.2)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff, nVal=250, pmMCARval=0.2)

plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, useContour=FALSE)

cutoff2 <- c(Chi=3.84, CFI=-0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, nVal=250, pmMCARval=0.2, condCutoff=FALSE)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2, useContour=FALSE)
