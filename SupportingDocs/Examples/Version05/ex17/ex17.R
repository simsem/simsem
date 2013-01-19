library(simsem)

popModel <- "
f1 =~ 1*y1 + 1*y2 + 1*y3 + con1*y2 + con1*y3
f2 =~ 1*y4 + 1*y5 + 1*y6 + con2*y5 + con2*y6
f3 =~ 1*y7 + 1*y8 + 1*y9 + con3*y8 + con3*y9
f4 =~ 1*y10 + 1*y11 + 1*y12 + con4*y11 + con4*y12
f5 =~ 1*y13 + 1*y14 + 1*y15 + con5*y14 + con5*y15
f6 =~ 1*y16 + 1*y17 + 1*y18 + con6*y17 + con6*y18
f7 =~ 1*y19 + 1*y20 + 1*y21 + con7*y20 + con7*y21
f8 =~ 1*y22 + 1*y23 + 1*y24 + con8*y23 + con8*y24
f9 =~ 1*y25 + 1*y26 + 1*y27 + con9*y26 + con9*y27
f4 ~ 0.6*f1 + con10*f1
f5 ~ start(0.3)*f1 + 0.6*f2 + con11*f1 + con12*f2
f6 ~ start(0.3)*f2 + 0.6*f3 + con13*f2 + con14*f3
f7 ~ 0.6*f4 + con10*f4
f8 ~ start(0.3)*f4 + 0.6*f5 + con11*f4 + con12*f5
f9 ~ start(0.3)*f5 + 0.6*f6 + con13*f5 + con14*f6
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
f4 ~~ 0.6*f4 + con15*f4
f5 ~~ 0.6*f5 + con16*f5
f6 ~~ 0.6*f6 + con17*f6
f7 ~~ 0.6*f7 + con15*f7
f8 ~~ 0.6*f8 + con16*f8
f9 ~~ 0.6*f9 + con17*f9
f1 ~~ 0.4*f2
f1 ~~ 0.4*f3
f2 ~~ 0.4*f3
y1 ~~ 0.5*y1
y2 ~~ 0.5*y2
y3 ~~ 0.5*y3
y4 ~~ 0.5*y4
y5 ~~ 0.5*y5
y6 ~~ 0.5*y6
y7 ~~ 0.5*y7
y8 ~~ 0.5*y8
y9 ~~ 0.5*y9
y10 ~~ 0.5*y10
y11 ~~ 0.5*y11
y12 ~~ 0.5*y12
y13 ~~ 0.5*y13
y14 ~~ 0.5*y14
y15 ~~ 0.5*y15
y16 ~~ 0.5*y16
y17 ~~ 0.5*y17
y18 ~~ 0.5*y18
y19 ~~ 0.5*y19
y20 ~~ 0.5*y20
y21 ~~ 0.5*y21
y22 ~~ 0.5*y22
y23 ~~ 0.5*y23
y24 ~~ 0.5*y24
y25 ~~ 0.5*y25
y26 ~~ 0.5*y26
y27 ~~ 0.5*y27
y1 ~~ 0.2*y10
y2 ~~ 0.2*y11
y3 ~~ 0.2*y12
y4 ~~ 0.2*y13
y5 ~~ 0.2*y14
y6 ~~ 0.2*y15
y7 ~~ 0.2*y16
y8 ~~ 0.2*y17
y9 ~~ 0.2*y18
y10 ~~ 0.2*y19
y11 ~~ 0.2*y20
y12 ~~ 0.2*y21
y13 ~~ 0.2*y22
y14 ~~ 0.2*y23
y15 ~~ 0.2*y24
y16 ~~ 0.2*y25
y17 ~~ 0.2*y26
y18 ~~ 0.2*y27
y1 ~~ 0.04*y19
y2 ~~ 0.04*y20
y3 ~~ 0.04*y21
y4 ~~ 0.04*y22
y5 ~~ 0.04*y23
y6 ~~ 0.04*y24
y7 ~~ 0.04*y25
y8 ~~ 0.04*y26
y9 ~~ 0.04*y27
med := con11 * con13
"

analyzeModel1 <- "
f1 =~ 1*y1 + con1*y2 + con1*y3
f2 =~ 1*y4 + con2*y5 + con2*y6
f3 =~ 1*y7 + con3*y8 + con3*y9
f4 =~ 1*y10 + con4*y11 + con4*y12
f5 =~ 1*y13 + con5*y14 + con5*y15
f6 =~ 1*y16 + con6*y17 + con6*y18
f7 =~ 1*y19 + con7*y20 + con7*y21
f8 =~ 1*y22 + con8*y23 + con8*y24
f9 =~ 1*y25 + con9*y26 + con9*y27
f4 ~ con10*f1
f5 ~ con11*f1 + con12*f2
f6 ~ con13*f2 + con14*f3
f7 ~ con10*f4
f8 ~ con11*f4 + con12*f5
f9 ~ con13*f5 + con14*f6
f1 ~~ f1
f2 ~~ f2
f3 ~~ f3
f4 ~~ con15*f4
f5 ~~ con16*f5
f6 ~~ con17*f6
f7 ~~ con15*f7
f8 ~~ con16*f8
f9 ~~ con17*f9
f1 ~~ f2
f1 ~~ f3
f2 ~~ f3
y1 ~~ y1
y2 ~~ y2
y3 ~~ y3
y4 ~~ y4
y5 ~~ y5
y6 ~~ y6
y7 ~~ y7
y8 ~~ y8
y9 ~~ y9
y10 ~~ y10
y11 ~~ y11
y12 ~~ y12
y13 ~~ y13
y14 ~~ y14
y15 ~~ y15
y16 ~~ y16
y17 ~~ y17
y18 ~~ y18
y19 ~~ y19
y20 ~~ y20
y21 ~~ y21
y22 ~~ y22
y23 ~~ y23
y24 ~~ y24
y25 ~~ y25
y26 ~~ y26
y27 ~~ y27
y1 ~~ y10
y2 ~~ y11
y3 ~~ y12
y4 ~~ y13
y5 ~~ y14
y6 ~~ y15
y7 ~~ y16
y8 ~~ y17
y9 ~~ y18
y10 ~~ y19
y11 ~~ y20
y12 ~~ y21
y13 ~~ y22
y14 ~~ y23
y15 ~~ y24
y16 ~~ y25
y17 ~~ y26
y18 ~~ y27
y1 ~~ y19
y2 ~~ y20
y3 ~~ y21
y4 ~~ y22
y5 ~~ y23
y6 ~~ y24
y7 ~~ y25
y8 ~~ y26
y9 ~~ y27
med := con11 * con13
"

Output1 <- sim(1000, n=200, analyzeModel1, generate=popModel, lavaanfun="lavaan")
summary(Output1)

analyzeModel2 <- "
f7 =~ 1*y19 + con7*y20 + con7*y21
f8 =~ 1*y22 + con8*y23 + con8*y24
f9 =~ 1*y25 + con9*y26 + con9*y27
f8 ~ a*f7 
f9 ~ b*f8 
f7 ~~ f7
f8 ~~ f8
f9 ~~ f9
y19 ~~ y19
y20 ~~ y20
y21 ~~ y21
y22 ~~ y22
y23 ~~ y23
y24 ~~ y24
y25 ~~ y25
y26 ~~ y26
y27 ~~ y27
med := a * b
"

Output2 <- sim(1000, n=200, analyzeModel2, generate=popModel, lavaanfun="lavaan")
summary(Output2)
summaryParam(Output2, matchParam = TRUE)
