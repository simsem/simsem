library(simsem)

popA <- "
ix =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
sx =~ 0*x1 + 1*x2 + 2*x3 + 3*x4
iy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
sy =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
x1 ~~ 0.5*x1
x2 ~~ 0.5*x2
x3 ~~ 0.5*x3
x4 ~~ 0.5*x4
y1 ~~ 0.5*y1
y2 ~~ 0.5*y2
y3 ~~ 0.5*y3
y4 ~~ 0.5*y4
ix ~~ 1*ix
sx ~~ 0.25*sx
iy ~~ 1*iy
sy ~~ 0.25*sy
ix ~~ 0.2*sx
iy ~~ 0.2*sy
ix ~~ 0.5*iy
sx ~~ 0*sy
ix ~~ 0*sy
iy ~~ 0*sx
x1 ~ 0*1
x2 ~ 0*1
x3 ~ 0*1
x4 ~ 0*1
y1 ~ 0*1
y2 ~ 0*1
y3 ~ 0*1
y4 ~ 0*1
ix ~ 5*1
sx ~ 2*1
iy ~ 5*1
sy ~ 2*1
"

popB <- "
ix =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
sx =~ 0*x1 + 1*x2 + 2*x3 + 3*x4
iy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
sy =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
x1 ~~ 0.5*x1
x2 ~~ 0.5*x2
x3 ~~ 0.5*x3
x4 ~~ 0.5*x4
y1 ~~ 0.5*y1
y2 ~~ 0.5*y2
y3 ~~ 0.5*y3
y4 ~~ 0.5*y4
ix ~~ 1*ix
sx ~~ 0.25*sx
iy ~~ 1*iy
sy ~~ 0.25*sy
ix ~~ 0.2*sx
iy ~~ 0.2*sy
ix ~~ 0*iy
sx ~~ 0.125*sy
ix ~~ 0*sy
iy ~~ 0*sx
x1 ~ 0*1
x2 ~ 0*1
x3 ~ 0*1
x4 ~ 0*1
y1 ~ 0*1
y2 ~ 0*1
y3 ~ 0*1
y4 ~ 0*1
ix ~ 5*1
sx ~ 2*1
iy ~ 5*1
sy ~ 2*1
"

analyzeA <- "
ix =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
sx =~ 0*x1 + 1*x2 + 2*x3 + 3*x4
iy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
sy =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
ix ~~ sx
iy ~~ sy
ix ~~ iy
sx ~~ 0*sy
ix ~~ 0*sy
iy ~~ 0*sx
"

analyzeB <- "
ix =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
sx =~ 0*x1 + 1*x2 + 2*x3 + 3*x4
iy =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
sy =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
ix ~~ sx
iy ~~ sy
ix ~~ 0*iy
sx ~~ sy
ix ~~ 0*sy
iy ~~ 0*sx
"

outAA <- sim(1000, n = 200, model = analyzeA, generate = popA, lavaanfun="growth")
outAB <- sim(1000, n = 200, model = analyzeB, generate = popA, lavaanfun="growth")
outBA <- sim(1000, n = 200, model = analyzeA, generate = popB, lavaanfun="growth")
outBB <- sim(1000, n = 200, model = analyzeB, generate = popB, lavaanfun="growth")

getCutoffNonNested(outAA, outAB, outBA, outBB)
getCutoffNonNested(outAA, outAB)
getCutoffNonNested(outBB, outBA)
getCutoffNonNested(outAA, outAB, outBA, outBB, onetailed=TRUE)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05)
plotCutoffNonNested(outAA, outAB, outBA, outBB, alpha=0.05, onetailed=TRUE)

getPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, usedFit="RMSEA")

cutoff <- c(AIC=0, BIC=0)
cutoff2 <- c(AIC=2, BIC=2)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff)
getPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, cutoff=cutoff2)
plotPowerFitNonNested(outBA, outBB, dat1Mod1=outAA, dat1Mod2=outAB, cutoff=cutoff2)
