 # Configural Invariance
 loading <- matrix(0, 6, 2)
 loading[1:3, 1] <- paste0("con",1:3)
 loading[4:6, 2] <- paste0("con",4:6)
 LY <- bind(loading, 0.7)
 
 latent.cor <- matrix(NA, 2, 2)
 diag(latent.cor) <- 1
 RPS <- binds(latent.cor, 0.5)
 
 RTE <- binds(diag(6))
 
 VTE <- bind(rep(NA, 6), 0.51)
 
 CFA.Model <- model(LY = list(LY, LY), RPS = list(RPS, RPS), RTE = list(RTE, RTE), VTE=list(VTE, VTE), ngroups=2, modelType = "CFA")
