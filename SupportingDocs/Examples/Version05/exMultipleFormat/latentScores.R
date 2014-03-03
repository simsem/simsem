library(simsem)

loading <- matrix(0, 6, 2)
loading[1:3, 1] <- NA
loading[4:6, 2] <- NA
LY <- bind(loading, 0.7)

latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5)

RTE <- binds(diag(6))

VTE <- bind(rep(NA, 6), 0.51)

CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, VTE=VTE, modelType = "CFA", indLab=c("pos1", "pos2", "pos3", "neg1", "neg2", "neg3"), facLab=c("posaffect", "negaffect"))

outfundata <- function(out, data) {
	predictcor <- inspect(out, "coef")$psi[2, 1]
	latentvar <- attr(data, "latentVar")[,c("f1", "f2")]
	latentcor <- cor(latentvar)[2,1]
	latentcor - predictcor
}

Output <- sim(1000, CFA.Model, n=200, sequential = TRUE, saveLatentVar = TRUE, 
	outfundata = outfundata)
summary(Output)

diffcor <- unlist(getExtraOutput(Output))
mean(diffcor, na.rm = TRUE)
sd(diffcor, na.rm = TRUE)