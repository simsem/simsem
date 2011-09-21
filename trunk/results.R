# The results Class

setClass("results",
         representation(
                        pop.model="matrix",
                        analysis.model="matrix",
                        combined.estimates="numeric",
                        combined.fit="numeric",
                        combined.se="numeric"),
         prototype(
                   pop.model=matrix(),
                   analysis.model=matrix(),
                   combined.estimates=c(0),
                   combined.fit=c(0),
                   combined.se=c(0)),
         sealed=TRUE)


# Public combine method. This will create the results object from a list of analysis objects.
# Input: List of analysis objects
# Returns: new results object
combine <- function(analysis.ls) {

  # res <- new("results")
  # do some things
  # return(res)
  
}


# Public summary method.
# Input: results object
# Output: numerical summaries
setMethod("summary",signature="results",
          definition=function(object, detail=FALSE) {
           
          })

# Public plot method. Weird clunkiness with the definition that will have to be examined further.
# Input: results object
# Output: Pretty things
setMethod("plot",signature="results",
          definition=function(x = object, y = object) {

          })



# Questions
# 1) Convergence?
