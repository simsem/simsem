# summaryShort: Provide short summary if it is available. Otherwise, it is an alias for summary.

setMethod("summaryShort", signature = "ANY", definition = function(object) {
    summary(object)
})

setMethod("summaryShort", signature = "SimMatrix", definition = function(object) {
    Data <- object@free
    Labels <- object@value
    Labels[!is.na(Data)] <- as.character(Data[!is.na(Data)])
    Labels[is.na(Data)] <- paste("NA:", Labels[is.na(Data)], sep = "")
    print(Labels)
})

setMethod("summaryShort", signature = "SimVector", definition = function(object) {
    Data <- object@free
    Labels <- object@value
    Labels[!is.na(Data)] <- as.character(Data[!is.na(Data)])
    Labels[is.na(Data)] <- paste("NA:", Labels[is.na(Data)], sep = "")
    print(Labels)
})

setMethod("summaryShort", signature = "vector", definition = function(object) {
    print(object)
})

setMethod("summaryShort", signature = "matrix", definition = function(object) {
    print(object)
})

# Distribution object: Provide a summary of each distribution object

setMethod("summaryShort", signature(object = "SimNorm"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Normal Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimUnif"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Uniform Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimBeta"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Beta Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimBinom"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Binomial Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimCauchy"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Cauchy Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimChisq"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Chi-squared Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimExp"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Exponential Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimF"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random F Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimGamma"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Gamma Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimGeom"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Geometric Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimHyper"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Hypergeometric Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimLnorm"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Log Normal Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimLogis"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Logistic Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimNbinom"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Negative Binomial Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimPois"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Poisson Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimT"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random t Distribution Object: ", lab, ".\n", sep = ""))
})

setMethod("summaryShort", signature(object = "SimWeibull"), function(object) {
    lab <- makeLabels(object)
    cat(paste("Random Weibull Distribution Object: ", lab, ".\n", sep = ""))
}) 
