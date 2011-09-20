x <- rnorm(25,1,1)

data <- matrix(rep(rnorm(100,1,1),15),ncol=15)

# Function to poke holes in the data for planned missing designs. Currently, we assume a 3-form design.
# Input: Data Set
# Output: Data Set wit data removed
#
# So, each row is an observation and each column an item
#

poke.holes <- function(data) {

  nitems <- dim(data)[2]
  nobs <- dim(data)[1]
  nforms <- 3

  items.in.group <- nitems/(nforms+1)
  item.index.list <- generate.indices(nforms+1,items.in.group)

  # Items (col indices) each group will be missing
  groupA.miss <- item.index.list[[2]]
  groupB.miss <- item.index.list[[3]]
  groupC.miss <- item.index.list[[4]]

  obs.in.group <- nobs / (nforms)
  obs.index.list <- generate.indices(nforms,obs.in.group)

  groupA <- obs.index.list[[1]]
  groupB <- obs.index.list[[2]]
  groupC <- obs.index.list[[3]]

  data[groupA,groupA.miss] <- NA
  data[groupB,groupB.miss] <- NA
  data[groupC,groupC.miss] <- NA
  
  return (data)
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


