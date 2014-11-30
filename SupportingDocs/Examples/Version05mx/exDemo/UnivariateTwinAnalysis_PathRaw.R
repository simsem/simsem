library(simsem)
library(semTools)
library(OpenMx)

############# Fitting twinACEFit

data(twinData)
twinVars <- c('fam','age','zyg','part','wt1','wt2','ht1','ht2','htwt1','htwt2','bmi1','bmi2')
summary(twinData)
selVars <- c('bmi1','bmi2')
aceVars <- c("A1","C1","E1","A2","C2","E2")
mzData <- as.matrix(subset(twinData, zyg==1, c(bmi1,bmi2)))
dzData <- as.matrix(subset(twinData, zyg==3, c(bmi1,bmi2)))
colMeans(mzData,na.rm=TRUE)
colMeans(dzData,na.rm=TRUE)
cov(mzData,use="complete")
cov(dzData,use="complete")

ACEModel <- mxModel("ACE", # Twin ACE Model -- Path Specification
	type="RAM",
    manifestVars=selVars,
    latentVars=aceVars,
    mxPath(
    	from=aceVars, 
    	arrows=2, 
    	free=FALSE, 
    	values=1
    ),
    mxPath(
    	from="one", 
    	to=aceVars, 
    	arrows=1, 
    	free=FALSE, 
    	values=0
    ),
    mxPath(
    	from="one", 
    	to=selVars, 
    	arrows=1, 
    	free=TRUE, 
    	values=20, 
    	labels= "mean"
    ),
    mxPath(
    	from=c("A1","C1","E1"), 
    	to="bmi1", 
    	arrows=1, 
    	free=TRUE, 
    	values=.6, 
    	label=c("a","c","e")
    ),
    mxPath(
    	from=c("A2","C2","E2"), 
    	to="bmi2", 
    	arrows=1, 
    	free=TRUE, 
    	values=.6, 
    	label=c("a","c","e")
    ),
    mxPath(
    	from="C1", 
    	to="C2", 
    	arrows=2, 
    	free=FALSE, 
    	values=1
    )
)    
mzModel <- mxModel(ACEModel, name="MZ",
	mxPath(
		from="A1", 
		to="A2", 
		arrows=2, 
		free=FALSE, 
		values=1
	),
	mxData(
		observed=mzData, 
		type="raw"
	)
)
dzModel <- mxModel(ACEModel, name="DZ", 
    mxPath(
    	from="A1", 
    	to="A2", 
    	arrows=2, 
    	free=FALSE, 
    	values=.5
    ),
    mxData(
    	observed=dzData, 
    	type="raw"
    )
)

twinACEModel <- mxModel("twinACE", mzModel, dzModel,
    mxAlgebra(
    	expression=MZ.objective + DZ.objective, 
    	name="twin"
    ), 
    mxFitFunctionAlgebra("twin")
)

twinACEFit <- mxRun(twinACEModel)
fitMeasuresMx(twinACEFit)
twinACEFitSim <- sim(10, twinACEFit, n = list(100, 100), mxFit=TRUE)
summary(twinACEFitSim)

####################### Fitting twinAEFit

AEModel <- mxModel(ACEModel, 
    mxPath(
    	from=c("A1","C1","E1"), 
    	to="bmi1", 
    	arrows=1, 
    	free=c(T,F,T),
    	values=c(.6,0,.6), 
    	label=c("a","c","e")
    ),
    mxPath(
    	from=c("A2","C2","E2"), 
    	to="bmi2", 
    	arrows=1, 
    	free=c(T,F,T),
    	values=c(.6,0,.6), 
    	label=c("a","c","e")
    )
)
mzModel <- mxModel(AEModel, name="MZ",
	mxPath(
		from="A1", 
		to="A2", 
		arrows=2, 
		free=FALSE, 
		values=1
	),
	mxData(
		observed=mzData, 
		type="raw"
	)
)
dzModel <- mxModel(AEModel, name="DZ", 
    mxPath(
    	from="A1", 
    	to="A2", 
    	arrows=2, 
    	free=FALSE, 
    	values=.5
    ),
    mxData(
    	observed=dzData, 
    	type="raw"
    )
)    
twinAEModel <- mxModel("twinAE", mzModel, dzModel,
    mxAlgebra(
    	expression=MZ.objective + DZ.objective, 
    	name="twin"
    ), 
    mxFitFunctionAlgebra("twin")
)

twinAEFit <- mxRun(twinAEModel)
fitMeasuresMx(twinAEFit)
twinAEEFitSim <- sim(10, twinAEFit, n = list(100, 100), mxFit=TRUE)
summary(twinAEEFitSim)
