library(simsem)

genmod <- "
f1 =~ 1*y1 + 1*y2 + 1*y3
f2 =~ 1*y4 + 1*y5 + 1*y6
f3 =~ 1*y7 + 1*y8 + 1*y9
f =~ 1*f1 + 1*f2 + 1*f3
f ~~ 0.7*f
f1 ~~ 0.7*f1
f2 ~~ 0.7*f2
f3 ~~ 0.7*f3
y1 ~~ 0.3*y1
y2 ~~ 0.3*y2
y3 ~~ 0.3*y3
y4 ~~ 0.3*y4
y5 ~~ 0.3*y5
y6 ~~ 0.3*y6
y7 ~~ 0.3*y7
y8 ~~ 0.3*y8
y9 ~~ 0.3*y9
"

mod <- "
f1 =~ 1*y1 + NA*y2 + NA*y3
f2 =~ 1*y4 + NA*y5 + NA*y6
f3 =~ 1*y7 + NA*y8 + NA*y9
f =~ 1*f1 + NA*f2 + NA*f3
"

dat <- simulateData(genmod, sample.nobs = 300)
out <- cfa(mod, dat) 

Output <- sim(1000, n=300, model = mod, generate = list(model = genmod, standardized = FALSE), lavaanfun = "cfa") 
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
