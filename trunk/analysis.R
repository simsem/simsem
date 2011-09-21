# The analysis Class

setClass("analysis",
         representation(
                        model="matrix",
                        estimates="numeric",
                        fit="numeric",
                        se="numeric",
                        convergence="logical"),
         prototype(
                   model=matrix(),
                   estimates=c(0),
                   fit=c(0),
                   se=c(0),
                   convergence=logical()),
         sealed=TRUE)

# Internal function to do the analysis.
# Input: List of imputed data matrices, and an analysis matrix (maybe list?)
# Returns: List of estimates, fit, se, vectors, and convergence 
.analyze <- function(data.mat.ls, analysis.mat) {

  # return(list(estimates,fit,se,convergence))
}





# User Interface for generating analysis objects.
# Input: dataGen object, and an analysis matrix
# Returns: an analysis object

run <- function(ob.dataGen, analysis.mat) {}
setGeneric("run")

setMethod("run",signature="analysis",
          definition=function(ob.dataGen, analysis.mat) {
            analysis.ob <- new("analysis")
            analysis.ob@model <- analysis.mat
            res <- .analyze(ob.dataGen@missing.data, analysis.mat)
            analysis.ob@estimates <- res[[1]]
            analysis.ob@fit <- res[[2]]
            analysis.ob@se <- res[[3]]
            analysis.ob@convergence <- res[[4]]
            return(analysis.ob)
          })


# Questions:
# 1) Is convergence a bool (T or F) or a percentage?
