get.keywords <- function() {
	LY <- c("LY", "ly", "Ly")		#Factor Loading of Y from E
	RTE <- c("RTE", "rte", "Rte")		#Measurement Error Correlation
	RPS <- c("RPS", "rps", "Rps")		#Factor Residual Correlation
	BE <- c("BE", "be", "Be")		#Path within endogeneous factors
	VY <- c("VY", "vy", "Vy")		#Variance of indicators
	VPS <- c("VPS", "Vps", "vps")	#Variance of factor residual
	VE <- c("VE", "Ve", "ve")		#Variance of factors
	TY <- c("TY", "ty", "Ty")		#Measurement Intercept
	ME <- c("ME", "Me", "me")		#mean of Factor
	VTE <- c("VTE", "vte", "Vte")	#Variance of measurement error
	AL <- c("AL", "Al", "al")		#Intercept of latent residuals
	MY <- c("MY", "My", "my")		#mean of indicators

	LX <- c("LX", "Lx", "lx")		#Exo Factor Loading
	RTD <- c("RTD", "rtd", "Rtd")		#Factor Measurement Error Correlation
	RPH <- c("RPH", "rph", "Rph")		#Exo factor correlation
	GA <- c("GA", "ga", "Ga")		#Path from Exo to Endo
	VX <- c("VX", "Vx", "vx")		#Variance of exo indicators
	VPH <- c("VK", "vk", "Vk", "Vph", "VPH", "vph")		#Variance of exo factors
	TX <- c("TX", "Tx", "tx")		#Exo measurement intercept
	KA <- c("KA", "Ka", "ka", "MK", "mk", "Mk")		#Exo factor mean
	VTD <- c("VTD", "Vtd", "vtd")	#Variance of exo measurement error
	MX <- c("MX", "mx", "Mx")		#Exo indicator mean
	RTH <- c("RTH", "Rth", "Rth")		#Correlated error of exo (row) and endo (column)

	loading <- c(LY, LX, "loading", "Loading", "Factor Loading")
	error <- c(RTE, RTD, "error", "Error", "Error covariance")
	latent.cor <- c(RPS, RPH, "latent.cor", "Latent.cor", "Latent.cov", "latent.cov", "Factor Covariance")
	intercept <- c(TY, TX, "intercept", "Intercept", "Measurement Intercept")
	factor.mean <- c(ME, KA, AL, "Factor mean", "factor mean", "Factor Mean")

	result <- list(LY=LY, RTE=RTE, RPS=RPS, BE=BE, VY=VY, VPS=VPS, VE=VE, TY=TY, ME=ME, VTE=VTE, AL=AL, MY=MY,
		LX=LX, RTD=RTD, RPH=RPH, GA=GA, VX=VX, VPH=VPH, TX=TX, KA=KA, VTD=VTD, MX=MX, RTH=RTH, 
		loading=loading, error=error, latent.cor=latent.cor, intercept=intercept, factor.mean=factor.mean)
	return(result)
}
