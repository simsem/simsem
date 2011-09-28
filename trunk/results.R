# The results Class

setClass("results",
         representation(
                        pop.model="list",
                        analysis.model="list",
                        combined.estimates="numeric",
                        combined.fit="numeric",
                        combined.se="numeric"),
         prototype(
                   pop.model=list(),
                   analysis.model=list(),
                   combined.estimates=c(0),
                   combined.fit=c(0),
                   combined.se=c(0)),
         sealed=TRUE)


# Public combine method. This will create the results object from a list of analysis objects.
# Input: List of analysis objects
# Returns: new results object
combine <- function(rep, analysis.ls) {

  # res <- new("results")
  # do some things (rbind?)
  # return(res)
  
}


# Public summary method.
# Input: results object
# Output: numerical summaries (means of simulations? can we compute bias, emp se, power?)
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

#Answers
# 1) Convergences can be a variable in the analysis object and then summarized (this is probably the way to go).
#    Or, non-convergence could simply not output an analysis object 
