
parms <- list()
parms$len.scale <- 6
parms$mar.pred1 <- "dat1"
parms$mar.pred2 <- "dat2"
parms$pm <- .15
parms$nobs <- 500


data <- cbind(matrix(rnorm(parms$len.scale*parms$nobs,10,15),parms$nobs,parms$len.scale),as.matrix(rnorm(parms$nobs,2,1)),as.matrix(rnorm(parms$nobs,0,1)))

colnames(data) <- c(paste("a",1:(parms$len.scale*.5),sep=""),paste("b",1:(parms$len.scale*.5),sep=""),"dat1","dat2")

dat <- data

makeMAR <- function(dat,parms)
  {
    pm <- parms$pm
    len.scale <- parms$len.scale
    mar.pred1 <- parms$mar.pred1
    mar.pred2 <- parms$mar.pred2

    Y <- runif(len.scale*.5,0,.25*pm)

    Z <- sample(c(Y+pm,-Y+pm), size=len.scale, replace=F)

    fun1 <- function(x,dat) pnorm(dat,mean(dat),sd(dat)) <= x

    R1 <- sapply(Z[1:(length(Z)*.5)],fun1,dat=data[,mar.pred1])
    R2 <- sapply(Z[((length(Z)*.5)+1):length(Z)],fun1,dat=data[,mar.pred2])

    R <- cbind(R1,R2,matrix(FALSE,dim(dat)[1],(dim(dat)[2]-(dim(R1)[2]*2))))
    
    dat[R] <- NA

    dat
    
  }# End impose.probit.missing()

miss.test <- makeMAR(dat=data,parms=parms)


sum(is.na(miss.test))/(parms$nobs*parms$len.scale)

makeMCAR <- function(dat,parms)
  {
    len.scale <- parms$len.scale
    pm <- parms$pm
    
    MCARfun <- function(x) runif(1,0,1) < pm

    mcarR <- apply(dat[,1:len.scale],c(1,2),FUN=MCARfun)

    R <- cbind(mcarR,matrix(FALSE,dim(dat)[1],(dim(dat)[2]-(dim(mcarR)[2]))))

    dat[R] <- NA

    dat
  }

mcarTest <- makeMCAR(data,parms)

sum(is.na(mcarTest))/(dim(mcarTest)[1]*parms$len.scale)
