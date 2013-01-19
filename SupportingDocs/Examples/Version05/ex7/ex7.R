library(simsem)
library(lavaan)

HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939)

output.nomis <- sim(1000, n=nrow(HolzingerSwineford1939), model = fit, generate = fit)
plotCutoff(output.nomis, 0.05)
pValue(fit, output.nomis)
