setwd("/nfs/home/patr1ckm/simsem/trunk/")

install.packages("simsem_0.0-4.tar.gz", repos=NULL, type="source")
library(simsem)
library(debug)
library(Rmpi)

# For safety?
.Last <- function(){ 
  if (is.loaded("mpi_initialize")){ 
    if (mpi.comm.size(1) > 0){ 
      print("Please use mpi.close.Rslaves() to close slaves.") 
      mpi.close.Rslaves() 
      } 
      print("Please use mpi.quit() to quit R") 
      .Call("mpi_finalize") 
    } 
} 


# After we spawn computing minions, we need to send them our instructions
mpi.spawn.Rslaves(nslaves=5)
mpi.bcast.Robj2slave(planned.missing)

# Generate complete data -

build.example.model <- function() {
  factor.loading <- matrix(NA, 4, 2)
  factor.loading[,1] <- 1
  factor.loading[,2] <- 0:3
  LY <- simMatrix(factor.loading)

  factor.mean <- rep(NA, 2)
  factor.mean.starting <- c(5,2)
  AL <- simVector(factor.mean, factor.mean.starting)

  factor.var <- rep(NA,2)
  factor.var.starting <- c(1, 0.25)
  VPS <- simVector(factor.var, factor.var.starting)

  factor.cor <- matrix(NA,2,2)
  diag(factor.cor) <- 1
  PS <- symMatrix(factor.cor, 0.5)

  VTE <- simVector(rep(NA,4),1.2)

  TE <- symMatrix(diag(4))

  TY <- simVector(rep(0,4))

  LCA.Model <- simSetCFA(LY=LY, PS=PS, VPS=VPS, AL=AL,
                               VTE=VTE, TE=TE, TY=TY)
  return(LCA.Model)
  
}


data.model <- build.example.model()
data.object <- simData(300, data.model)
sim.data.model <- simModel(data.model) # this one is the actual model


complete.l <- list()
build.data.sets <- function(model,obs,sets) {
  for(i in 1:sets) { complete.l[[i]] <- run(model,obs) }
  return(complete.l) }

complete.l <- build.data.sets(data.object,100,3)


missing.l <- lapply(complete.l,imposeMissing,nforms=3)
missing.l <- mpi.applyLB(complete.l,imposeMissing)

#results <- runMI(missing.l,data.model=sim.data.model,imps=2)

results.l <- lapply(missing.l,runMI,data.model=sim.data.model,imps=2)


mtrace(runMI)

mpi.close.Rslaves()
mpi.quit()
