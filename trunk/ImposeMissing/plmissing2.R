x <- rnorm(25,1,1)

data <- matrix(rep(rnorm(10,1,1),20),ncol=20)

# Function to poke holes in the data for planned missing designs. Currently, we assume a 3-form design.
# Input: Data Set
# Output: Boolean matrix of values to delete
#
# Right now, function assumes a basic 3-form design, and that items are grouped sequentially
# (i.e. columns 1-5 are shared, 6-10 are A, 11-15 are B, and 16-20 are C)

# TODO:
# Pass item indices for grouping
# Warnings for illegal groupings
# Also deal with covariates?

# Two method planned missing design: everyone gets cheap measure, few get expensive. 2 form? 1 form design.
# n-form design - list of groupings. 
planned.missing <- function(data) {

  
  nitems <- dim(data)[2]
  nobs <- dim(data)[1]
  nforms <- 3

  log.mat <- matrix(FALSE,ncol=nitems,nrow=nobs)

  items.in.group <- nitems/(nforms+1)
  # groups items into sets of column indices (in the 3 form case, shared/a/b/c)
  item.index.list <- generate.indices(nforms+1,items.in.group)

  obs.in.group <- nobs / (nforms)
  # groups observations into sets of row indices. Each set "receives" a different "form"
  obs.index.list <- generate.indices(nforms,obs.in.group)

  # Create Missing Matrix
  for(i in 1:nforms) {
    log.mat[obs.index.list[[i]],item.index.list[[i+1]]] <- TRUE
       }

  return (log.mat)
}


# Generates sequential groups of lists of numbers based on the desired number of groups,
# and the number of items in a group.
#
# EX: generate.indices(3,4)
# [[1]]
# [1] 1 2 3 4
# [[2]]
# [1] 5 6 7 8
# [[3]]
# [1] 9 10 11 12
generate.indices <- function(ngroups, items.in.group) {

  
  for (i in 1:ngroups) {
    if (i == 1) {
      index.list <- list(1:items.in.group)
    }
    else {
      last.element.index <- length(index.list[[i-1]])
      new.group.index <- (index.list[[i-1]][last.element.index]+1):(items.in.group*(i))
      index.list <- c(index.list,list(new.group.index))
    }
  }
  return(index.list)
}


