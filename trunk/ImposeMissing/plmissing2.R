
tests <- function() {
  data <- matrix(rep(rnorm(10,1,1),20),ncol=20)

  pl1 <- planned.missing(dim(data))
  

  # First item group is shared
  itemGroups <- list(c(1,2,3,4,5),c(7,8,9,10,11),c(12,14,16,18,20),c(13,15,17,19))

  pl2 <- planned.missing(dim(data), itemGroups=itemGroups)

  obsGroups <- list(c(1,2,3,4),c(6,8,10),c(5,7,9))
  pl3 <- planned.missing(dim(data),obsGroups = obsGroups)

  # Rows 6/8/10 should be the same, as should 5,7,9
  # Should alternate T/F on columns 12 - 20
  pl4 <- planned.missing(dim(data),itemGroups=itemGroups,obsGroups=obsGroups)

  pl5 <- planned.missing(dim(data),nforms=2)

  pl6 <- planned.missing(dim(data),twoMethod=c(20,.8))
}

# Function to poke holes in the data for planned missing designs. Currently, we default a 3-form design.
# Input: Data Set
# Output: Boolean matrix of values to delete
#
# Right now, function defaults to a basic 3-form design,with sequentially grouped items
# (i.e. columns 1-5 are shared, 6-10 are A, 11-15 are B, and 16-20 are C)

# TODO:
# Pass item indices for grouping
# Warnings for illegal groupings
# Also deal with covariates?
# Check to see if item groupings are valid?
  
# Two method planned missing design: everyone gets cheap measure, few get expensive. 2 form? 1 form design.
# n-form design - list of groupings. 
planned.missing <- function(dims=c(0,0),nforms=3,itemGroups=NULL,obsGroups=NULL,twoMethod=NULL) {
  
  nitems <- dims[2]
  nobs <- dims[1]

  log.mat <- matrix(FALSE,ncol=nitems,nrow=nobs)

  if ( ((!is.null(itemGroups)) && (class(itemGroups) != "list")) ) {
    stop("itemGroups not a list")
  } 

  if ( ((!is.null(obsGroups)) && (class(obsGroups) != "list")) ) {
    stop("obsGroups not a list")
  }

  if (!is.null(twoMethod) && (class(twoMethod) != "numeric")) {
    stop("twoMethod not a valid value")
  }
 
  # groups items into sets of column indices (in the 3 form case, shared/a/b/c)
  if (is.null(itemGroups)) {
    items.in.group <- nitems/(nforms+1)
    itemGroups <- generate.indices(nforms+1,items.in.group)
  }

 # groups observations into sets of row indices. Each set "receives" a different "form"
  if (is.null(obsGroups)) {
    obs.in.group <- nobs / (nforms)
    obsGroups <- generate.indices(nforms,obs.in.group)
 
  }

  if (!is.null(twoMethod)) {
    col <- twoMethod[1]
    percent <- twoMethod[2]
    toDelete <- 1:((percent)*nobs)
    log.mat[toDelete,col] <- TRUE
  }
  else {
  # Create Missing Matrix
    for(i in 1:nforms) {
      log.mat[obsGroups[[i]],itemGroups[[i+1]]] <- TRUE
    }
  }

  return (log.mat)
}


# Default generation method for item groupings and observation groupings.
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


