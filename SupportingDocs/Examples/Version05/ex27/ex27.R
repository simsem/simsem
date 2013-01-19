library(simsem)

analyzeNested <- "
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4
"

analyzeParent <- "
i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + NA*t2 + NA*t3 + 3*t4
"

outNested <- growth(analyzeNested, data=Demo.growth)
outParent <- growth(analyzeParent, data=Demo.growth)

samplesize <- nrow(Demo.growth)

simNestedNested <- sim(1000, n = samplesize, model=outNested, generate=outNested)
simNestedParent <- sim(1000, n = samplesize, model=outParent, generate=outNested)
simParentNested <- sim(1000, n = samplesize, model=outNested, generate=outParent)
simParentParent <- sim(1000, n = samplesize, model=outParent, generate=outParent)

pValueNested(outNested, outParent, simNestedNested, simNestedParent)
getPowerFitNested(simParentNested, simParentParent, nullNested=simNestedNested, nullParent=simNestedParent)
