x <- rnorm(25,1,1)

data <- matrix(rep(rnorm(100,1,1),5),ncol=5)


poke.holes <- function(data) {

  nforms <- dim(data)[2]
  nitems <- dim(data)[1]


  num.in.group <- nitems/(nforms+1)

  index.list <- list()

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



