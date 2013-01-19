library(simsem)

popNested <- "
f1 =~ 1*y1 + 0.8*y2 + 1.2*y3 + con1*y2 + con2*y3
f2 =~ 1*y4 + 0.8*y5 + 1.2*y6 + con1*y5 + con2*y6
f3 =~ 1*y7 + 0.8*y8 + 1.2*y9 + con1*y8 + con2*y9
f1 ~~ 1*f1
f2 ~~ 1.2*f2
f3 ~~ 1.4*f3
f1 ~~ 0.77*f2
f2 ~~ 0.91*f3
f1 ~~ 0.58*f3
y1 ~~ 0.4*y1
y2 ~~ 0.4*y2
y3 ~~ 0.4*y3
y4 ~~ 0.4*y4
y5 ~~ 0.4*y5
y6 ~~ 0.4*y6
y7 ~~ 0.4*y7
y8 ~~ 0.4*y8
y9 ~~ 0.4*y9
y1 ~~ 0.08*y4
y2 ~~ 0.08*y5
y3 ~~ 0.08*y6
y4 ~~ 0.08*y7
y5 ~~ 0.08*y8
y6 ~~ 0.08*y9
y1 ~~ 0.016*y7
y2 ~~ 0.016*y8
y3 ~~ 0.016*y9
"

popParent <- "
f1 =~ 1*y1 + 0.8*y2 + 1.2*y3
f2 =~ 1*y4 + 0.4*y5 + 1.6*y6
f3 =~ 1*y7 + 1.2*y8 + 0.8*y9
f1 ~~ 1*f1
f2 ~~ 1.2*f2
f3 ~~ 1.4*f3
f1 ~~ 0.77*f2
f2 ~~ 0.91*f3
f1 ~~ 0.58*f3
y1 ~~ 0.4*y1
y2 ~~ 0.4*y2
y3 ~~ 0.4*y3
y4 ~~ 0.4*y4
y5 ~~ 0.4*y5
y6 ~~ 0.4*y6
y7 ~~ 0.4*y7
y8 ~~ 0.4*y8
y9 ~~ 0.4*y9
y1 ~~ 0.08*y4
y2 ~~ 0.08*y5
y3 ~~ 0.08*y6
y4 ~~ 0.08*y7
y5 ~~ 0.08*y8
y6 ~~ 0.08*y9
y1 ~~ 0.016*y7
y2 ~~ 0.016*y8
y3 ~~ 0.016*y9
"

analyzeNested <- "
f1 =~ 1*y1 + con1*y2 + con2*y3
f2 =~ 1*y4 + con1*y5 + con2*y6
f3 =~ 1*y7 + con1*y8 + con2*y9
f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
f1 ~~ f2
f2 ~~ f3
f1 ~~ f3
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
y7 ~~ y7
y8 ~~ y8
y9 ~~ y9
y1 ~~ y4
y2 ~~ y5
y3 ~~ y6
y4 ~~ y7
y5 ~~ y8
y6 ~~ y9
y1 ~~ y7
y2 ~~ y8
y3 ~~ y9
"

analyzeParent <- "
f1 =~ 1*y1 + y2 + y3
f2 =~ 1*y4 + y5 + y6
f3 =~ 1*y7 + y8 + y9
f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
f1 ~~ f2
f2 ~~ f3
f1 ~~ f3
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
y7 ~~ y7
y8 ~~ y8
y9 ~~ y9
y1 ~~ y4
y2 ~~ y5
y3 ~~ y6
y4 ~~ y7
y5 ~~ y8
y6 ~~ y9
y1 ~~ y7
y2 ~~ y8
y3 ~~ y9
"

outDatNestedModNested <- sim(1000, n = 200, analyzeNested, generate = popNested, lavaanfun = "lavaan")
outDatNestedModParent <- sim(1000, n = 200, analyzeParent, generate = popNested, lavaanfun = "lavaan")

anova(outDatNestedModNested, outDatNestedModParent)
cutoff <- getCutoffNested(outDatNestedModNested, outDatNestedModParent)
plotCutoffNested(outDatNestedModNested, outDatNestedModParent, alpha=0.05)

outDatParentModNested <- sim(1000, n = 200, analyzeNested, generate = popParent, lavaanfun = "lavaan")
outDatParentModParent <- sim(1000, n = 200, analyzeParent, generate = popParent, lavaanfun = "lavaan")

anova(outDatParentModNested, outDatParentModParent)

getPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, usedFit="RMSEA")

cutoff2 <- c(Chi = 3.84, CFI = -0.01)
getPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, cutoff=cutoff2)
plotPowerFitNested(outDatParentModNested, outDatParentModParent, nullNested=outDatNestedModNested, nullParent=outDatNestedModParent, cutoff=cutoff2)
