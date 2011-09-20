x <- rnorm(25,1,1)

data <- matrix(rep(rnorm(100,1,1),5),ncol=5)



# Function to poke holes in the data for planned missing designs.
# Input: Data Set
# Output: Data Set wit data removed
#
# Base, 3 form exampe:
#      [,1]  [,2]  [,3]
# [1,]   1     2     3
# [2,]   NA    2     3
# [3,]   1     NA    3
# [4,]   1     2     NA

# This function assumes that each column in the data set is a form, and each row an item.
# For a 3 form design, a different 25% of the observations in each column will be removed.
# This works for n forms and m observations, where percent removed in a column is m/(n+1)

poke.holes <- function(data) {

  nforms <- dim(data)[2]
  nitems <- dim(data)[1]

  num.in.group <- nitems/(nforms+1)

  index.list <- list()

  # Beware, the indexing here is a little ridiculous. The basic idea is that we build up a list of indices
  # to remove, and then use the indeces to mark an observation as NA in the column.  
for (i in 0:nforms) {
  if (i == 0) {
    index.list <- list(1:num.in.group)
  }
  else {
    last.element.index <- length(index.list[[i]])
    new.group.index <- (index.list[[i]][last.element.index]+1):(num.in.group*(i+1))
    index.list <- c(index.list,list(new.group.index))
    data[new.group.index,i] <- NA
  }
}

return(data)

}



