
tests <- function() {
  data <- matrix(rep(rnorm(10,1,1),19),ncol=19)

  pl1 <- planned.missing(dim(data))
  

  # First item group is shared
  itemGroups <- list(c(1,2,3,4,5),c(7,8,9,10,11),c(12,14,16,18,20),c(13,15,17,19))

  pl2 <- planned.missing(dim(data), itemGroups=itemGroups)

  pl5 <- planned.missing(dim(data),nforms=4)

  pl6 <- planned.missing(dim(data),twoMethod=c(19,.8))

  pl7 <- planned.missing(dim(data),covs=c(19,20))
  pl8 <- planned.missing(dim(data),covs=c(4,5,6))
}

# Function to poke holes in the data for planned missing designs. Currently, we default a 3-form design.
# Input: Data Set
# Output: Boolean matrix of values to delete
#
# Right now, function defaults to NULL missingness. If number of forms is specified, items are divided equally and
# grouped sequentially.
# (i.e. columns 1-5 are shared, 6-10 are A, 11-15 are B, and 16-20 are C)

# TODO:
# Warnings for illegal groupings
# Check to see if item groupings are valid?
  
planned.missing <- function(dims=c(0,0),nforms=NULL,itemGroups=NULL,twoMethod=NULL, covs=NULL) {
  
  nitems <- dims[2]
  nobs <- dims[1]
  excl <- covs

  log.mat <- matrix(FALSE,ncol=nitems,nrow=nobs)

  if(!is.null(nforms) && nforms != 0) {
    if ( ((!is.null(itemGroups)) && (class(itemGroups) != "list")) ) {
      stop("itemGroups not a list")
    } 

   # groups items into sets of column indices (in the 3 form case, shared/a/b/c)

   if (is.null(itemGroups)) {
     items.in.group <- nitems/(nforms+1)
     itemGroups <- generate.indices(nforms+1,items.in.group,excl)
   }

   # groups observations into sets of row indices. Each set "receives" a different "form"

   obs.in.group <- nobs / (nforms)
   obsGroups <- generate.indices(nforms,obs.in.group,excl=NULL)

   # Create Missing Matrix
     for(i in 1:nforms) {
       log.mat[obsGroups[[i]],itemGroups[[i+1]]] <- TRUE
  
     }
  }
   if (!is.null(twoMethod)) {
     col <- unlist(twoMethod[1])
     percent <- unlist(twoMethod[2])
     toDelete <- 1:((percent)*nobs)
     log.mat[toDelete,col] <- TRUE
   }

  return (log.mat)
}


# Default generation method for item groupings and observation groupings.
# Generates sequential groups of lists of numbers based on the desired number of groups,
# and the number of items in a group, excluding certain indices.
#
# EX: generate.indices(3,4)
# [[1]]
# [1] 1 2 3 4
# [[2]]
# [1] 5 6 7 8
# [[3]]
# [1] 9 10 11 12
generate.indices <- function(ngroups, items.in.group,excl=NULL) {
  
  a <- 1:(ngroups*items.in.group) # set of item indices
  
  if(!is.null(excl)){
    anot <- a[-excl]
  } else { anot <- a}
  
  ipg <- length(anot)/ngroups

  for (i in 1:ngroups) {
    if (i==1) {
      index.list <- list(anot[1:ipg])
    }
    else {
      indices.used <- length(unlist(index.list))
      index.list[[i]] <- anot[(indices.used+1):(ipg*i)]
    }
  }
    
  return(index.list)
}


