
################################################################################
##Code to accompany Schoemann, A.M., Short, S. D., & Little, T. D. (2014)
##Examining Between, Within, and Mixed Factorial Designs with Structural Equation Modeling
##American Psychological Association Annual Convention, Washington D.C.
##Last modified 8/4/2014
################################################################################
library(lavaan)

################################################################################
## data preparation
################################################################################

##To access data download the Chapter 5 materials for Little (2013) from this website: http://www.guilford.com/companion-site/Longitudinal-Structural-Equation-Modeling
#The data are under the Mplus mateirals for Chapter 5 
dat <- read.table("10.CarpThesis.dat")
names(dat) <- c("PA1_1", "PA2_1", "PA3_1", "NA1_1", "NA2_1", "NA3_1",
								"PA1_2", "PA2_2", "PA3_2", "NA1_2", "NA2_2", "NA3_2", "Group")

################################################################################
## Strong Invariance Model (Loadings & Intercepts)
################################################################################

##This model has all contrasts specified. 
##Each contrast is estimated and the model is identical to the strong invariance model
##The scale of each latent variable is set using effects coding
mod10.3int <- '
## define latent variables (Lambda matrix), include labels for model constraints
					posAff1 =~ c(L42, L42, L42)*PA1_1 + c(L52, L52, L52)*PA2_1 + c(L62, L62, L62)*PA3_1
					posAff2 =~ c(L42, L42, L42)*PA1_2 + c(L52, L52, L52)*PA2_2 + c(L62, L62, L62)*PA3_2

## indicator residual variances (Theta-Epsilon matrix)
					PA1_1 ~~ PA1_1
					PA2_1 ~~ PA2_1
					PA3_1 ~~ PA3_1
					PA1_2 ~~ PA1_2
					PA2_2 ~~ PA2_2
					PA3_2 ~~ PA3_2
	## correlate residuals of indicators with themselves across time
					PA1_1 ~~ PA1_2
					PA2_1 ~~ PA2_2
					PA3_1 ~~ PA3_2

## indicator intercepts (Tau matrix), include labels for model constraints
					PA1_1 ~ c(t4, t4, t4)*1
					PA2_1 ~ c(t5, t5, t5)*1
					PA3_1 ~ c(t6, t6, t6)*1
					PA1_2 ~ c(t4, t4, t4)*1
					PA2_2 ~ c(t5, t5, t5)*1
					PA3_2 ~ c(t6, t6, t6)*1

## latent variances and covariances (Psi matrix)
					posAff1 ~~ c(p1, p2, p3)*posAff1 + posAff2
					posAff2 ~~ c(p4, p5, p6)*posAff2

## latent means (Alpha matrix)
					posAff1 ~ c(a1, a2, a3)* 1
					posAff2 ~ c(a4, a5, a6)*1

## model constraints for effects-coding method of identification
					L42 == 3 - L52 - L62
					t4 == 0 - t5 - t6
					
#Contrast codes for ME and interactions					
                                        #Means
										#ME of group
										c1 := 2*a1 + -1*a2 + -1*a3 + 2*a4 + -1*a5 + -1*a6
										c2 := 0*a1 + 1*a2 + -1*a3 + 0*a4 + 1*a5 + -1*a6
										#ME of time
										c3 := 1*a1 + 1*a2 + 1*a3 + -1*a4 + -1*a5 + -1*a6
										#Interaction for means
										c4 := 2*a1 + -1*a2 + -1*a3 + -2*a4 + 1*a5 + 1*a6
                                        c5 := 0*a1 + 1*a2 + -1*a3 + 0*a4 + -1*a5 + 1*a6
										#Variances
										#ME of group
										c6 := 2*p1 + -1*p2 + -1*p3 + 2*p4 + -1*p5 + -1*p6
										c7 := 0*p1 + 1*p2 + -1*p3 + 0*p4 + 1*p5 + -1*p6
										#ME of time
										c8 := 1*p1 + 1*p2 + 1*p3 + -1*p4 + -1*p5 + -1*p6
										#Interaction for means
										c9 := 2*p1 + -1*p2 + -1*p3 + -2*p4 + 1*p5 + 1*p6
                                        c10 := 0*p1 + 1*p2 + -1*p3 + 0*p4 + -1*p5 + 1*p6
'
#Strong invariance model. To be used for model comparisons 
fit10.3int <- lavaan(mod10.3int, data=dat, std.lv=F, auto.fix.first=F, meanstructure=T, group="Group", estimator = "MLR")
summary(fit10.3int)

#Use the constraints option in lavaan to fix contrast coeffcients to 0
#Test ME of group means 
fit10.3gm <- lavaan(mod10.3int, data=dat, std.lv=F, auto.fix.first=F, meanstructure=T, group="Group", estimator = "MLR", constraints = c('c1==0', 'c2==0'))
anova(fit10.3int, fit10.3gm) #LR test group means for neg affect

#Test ME of group vars 
fit10.3gv <- lavaan(mod10.3int, data=dat, std.lv=F, auto.fix.first=F, meanstructure=T, group="Group", estimator = "MLR", constraints = c('c6==0', 'c7==0'))
anova(fit10.3int, fit10.3gv) #LR test group means for neg affect

#Test ME of time means 
fit10.3tm <- lavaan(mod10.3int, data=dat, std.lv=F, auto.fix.first=F, meanstructure=T, group="Group", estimator = "MLR", constraints = c('c3==0'))
anova(fit10.3int, fit10.3tm) #LR test group means for neg affect

#Test ME of time vars 
fit10.3tv <- lavaan(mod10.3int, data=dat, std.lv=F, auto.fix.first=F, meanstructure=T, group="Group", estimator = "MLR", constraints = c('c8==0'))
anova(fit10.3int, fit10.3tv) #LR test group means for neg affect

#Test int  means 
fit10.3tm <- lavaan(mod10.3int, data=dat, std.lv=F, auto.fix.first=F, meanstructure=T, group="Group", estimator = "MLR", constraints = c('c4==0', 'c5==0'))
anova(fit10.3int, fit10.3tm) #LR test group means for neg affect

#Test int vars 
fit10.3tv <- lavaan(mod10.3int, data=dat, std.lv=F, auto.fix.first=F, meanstructure=T, group="Group", estimator = "MLR", constraints = c('c9==0', 'c10==0'))
anova(fit10.3int, fit10.3tv) #LR test group means for neg affect

##We could also use a Wald test to test the contrast coefficients
lavTestWald(fit10.3int, constraints = c('c9==0', 'c10==0'))#interaction for positive affect
