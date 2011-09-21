# The dataGen Class

# pop.model - population model
# analysis.model - analysis model
# data - raw simulated data
# missing.data - raw data set with missing values
# impute.data - list of imputed data sets

# "Public" functions (or the user interface) will be generic methods? Maybe. 
# "Helper" functions (or private functions) will be denoted by a "." in front of the function.
# All helper functions will assume that a dataGen object has already been created and will
# update it.

setClass("dataGen",
         representation(
                        pop.model="matrix",
                        #analysis.model="matrix",
                        data="matrix",
                        missing.data="matrix",
                        impute.data="list"),
         prototype(
                   pop.model=matrix(),
                   #analysis.model=matrix(),
                   data=matrix(),
                   missing.data=matrix(),
                   impute.data=list()),
         sealed=TRUE)


# Creates the raw data from the population model. This function will be the interface to
# the work already completed by Sunthud.
# Input: A population model matrix
# Returns: raw data matrix

.create.data <- function(pop.model, misfit) {

  # return(data.mat)

}

# Helper function to impose the missing values on a data set
# Input: raw data matrix
# Returns: matrix with missingness imposed by misFunc
.create.missing <- function(data.mat,misFunc) {

   # Grab function from parameter, maybe check that it exists. Not sure about scoping.
  # missing <- match.fun(misFunc)

  # return(missing.data.mat)

}

# Helper function to impute the missing values.
# Input: matrix with missing values
# Returns: a list of imputed data sets
.impute.missing <- function(missing.data.mat) {


}




# Creates the dataGen object. This is the public interface, but is non-generic.
create.dataGen <- function(pop.model, analysis.model, imputation.type, misFunc, misfit, seed) {
            data <- new("dataGen")
            data@pop.model <- pop.model
            #data@analysis.model <- analysis.model
            data@data <- .create.data(pop.model,misfit)
            data@missing.data <- .create.missing(data@data,misFunc)
            data@impute.data <- .impute.missing(data@missing.data)           
            
          }

# Questions:
# 1) Do we want a run function to actually do the steps, or should we do all the steps (create data, impose missing, impute
# when we create the dataGen object?
# 2) Do we want functions to create the population and analysis matrices?
# 3) Do we need analysis matrices in this object? Or will it be an input to build the analysis object in the next step?
