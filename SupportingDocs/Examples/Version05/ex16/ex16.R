library(simsem)

# First analysis model: Model without covariate

popModel <- "
f1 =~ 0.6*y1 + 0.6*y2 + 0.6*y3
f2 =~ 0.6*y4 + 0.6*y5 + 0.6*y6
f3 =~ 0.4*y1 + 0.4*y2 + 0.4*y3 + 0.4*y4 + 0.4*y5 + 0.4*y6 + 1*y7
f2 ~ 0.4*f1
f1 ~~ 0*f3
f2 ~~ 0*f3
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
y1 ~~ 0.64*y1
y2 ~~ 0.64*y2
y3 ~~ 0.64*y3
y4 ~~ 0.64*y4
y5 ~~ 0.64*y5
y6 ~~ 0.64*y6
y7 ~~ 0*y7
"

analyzeModel1 <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f2 ~ f1
"

Output1 <- sim(10, n=200, analyzeModel1, generate=popModel, std.lv=TRUE, lavaanfun="sem")
summary(Output1)

# Second analysis model: Model accounting for covariate in the indicator level

analyzeModel2 <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f3 =~ y1 + y2 + y3 + y4 + y5 + y6 + 1*y7
f2 ~ f1
f1 ~~ 0*f3
f2 ~~ 0*f3
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ f3
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
y7 ~~ 0*y7
"

Output2 <- sim(10, n=200, analyzeModel2, generate=popModel, lavaanfun="lavaan")
summary(Output2)

# Third analysis model: Model accounting for covariate with orthogonalization

library(semTools)

datafun <- function(data) {
	residualCovariate(data, targetVar=1:6, covVar=7)
}

analyzeModel3 <- analyzeModel1

Output3 <- sim(10, n=200, analyzeModel3, generate=popModel, std.lv=TRUE, lavaanfun="sem", datafun=datafun)
summary(Output3)

# Fourth analysis model: Model accounting for covariate in factor level

analyzeModel4 <- "
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f2 ~ f1 + y7
f1 ~ y7
f1 ~~ 1*f1
f2 ~~ 1*f2
"

Output4 <- sim(10, n=200, analyzeModel4, generate=popModel, std.lv=TRUE, fixed.x=FALSE, lavaanfun="sem")
summary(Output4)
summaryParam(Output4, matchParam=TRUE) # Ignore y7 ~~ y7 which means error variance in data-generation model but total variance in analysis model
