library(simsem)

popModel <- "
f1 =~ 0.7*y1 + 0.7*y2 + 0.7*y3
f2 =~ 0.7*y4 + 0.7*y5 + 0.7*y6
f3 =~ con1*y7 + con1*y8 + 0.6*y7 + 0.6*y8 
f3 ~ 0.6*f1 + 0.3*f2
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
f1 ~~ 0.5*f2
y1 ~~ 0.51*y1
y2 ~~ 0.51*y2
y3 ~~ 0.51*y3
y4 ~~ 0.51*y4
y5 ~~ 0.51*y5
y6 ~~ 0.51*y6
y7 ~~ 0.64*y7
y8 ~~ 0.64*y8
"

analyzeModel <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f3 =~ con1*y7 + con1*y8
f3 ~ f1 + f2
"

Output <- sim(1000, analyzeModel, n=200, generate=popModel, std.lv=TRUE, lavaanfun = "sem")
# Output <- sim(1000, list(model = analyzeModel, std.lv=TRUE), n=200, generate=popModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
