x <- rnorm(25,1,1)

data <- matrix(rep(rnorm(100,1,1),10),ncol=10)



# Function to poke holes in the data for planned missing designs.
# Input: Data Set
# Output: Data Set wit data removed
#
# So, each row is an observation and each column an item
#

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



