# simMissing
# function -- simsem package
# A constructor of missing object
# Argument:
#	cov:		Column indices of any normally distributed covariates used in the data set.
#	pmMCAR:		Decimal percent of missingness to introduce completely at random on all variables.
#	pmMAR:		Decimal percent of missingness to introduce using the listed covariates as predictors.
#	nforms:		The number of forms for planned missing data designs, not including the shared form.
#	itemGroups:		List of lists of item groupings for planned missing data forms. Without this, items will be divided into groups sequentially (e.g. 1-3,4-6,7-9,10-12)
#	twoMethod:		Vector of (percent missing, column index). Will put a given percent missing on that column in the matrix to simulate a two method planned missing data research design. 
#	prAttr:		Probability (or vector of probabilities) of an entire case being removed due to attrition at a given time point. See imposeMissing function for further details.
#	timePoints:		Number of timepoints items were measured over. For longitudinal data, planned missing designs will be implemented within each timepoint.
# 	numImps:		The number of imputations to be used when multiply imputing missing data. Setting numImps to 0 will use FIML to handle missing data.
#	ignoreCols:		The columns not imposed any missing values for any missing data patterns
#	threshold:		The threshold of covariates that divide between the area to impose missing and the area not to impose missing. The default threshold is the mean of the covariate.
#	covAsAux:		If TRUE, the covariate listed in the object will be used as auxiliary variables when putting in the model object. If FALSE, the covariate will be included in the analysis.
#	logical:		A matrix of logical values (TRUE/FALSE). If a value in the dataset is corresponding to the TRUE in the logical matrix, the value will be missing.
# Return:	A missing object
# Author: Patrick Miller, Alex Schoemann, Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)


#Need container for longitudinal planned missing (e.g., number of items per time)

simMissing <- function(cov=0, pmMCAR=0, pmMAR=0, nforms=0, itemGroups=list(0), timePoints=1, twoMethod=0, prAttr = 0, numImps=0, ignoreCols=0, threshold=0, covAsAux=TRUE, logical=new("NullMatrix")) {
	if(is(logical, "data.frame")) logical <- as.matrix(logical)
  return(new("SimMissing",cov=cov, pmMCAR=pmMCAR, pmMAR=pmMAR, nforms=nforms, itemGroups=itemGroups,
             twoMethod=twoMethod, prAttr=prAttr, timePoints=timePoints, threshold=threshold, ignoreCols=ignoreCols, 
			 numImps=numImps, covAsAux=covAsAux, logical=logical))
}
