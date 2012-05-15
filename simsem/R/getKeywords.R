# getKeywords: List of all keywords used in the simsem package

getKeywords <- function() {
    LY <- "LY"  #Factor Loading of Y from E
    TE <- "TE"  #Measurement Error Covariance
    RTE <- "RTE"  #Measurement Error Correlation
    PS <- "PS"  #Factor Residual Covariance
    RPS <- "RPS"  #Factor Residual Correlation
    BE <- "BE"  #Path within endogeneous factors
    VY <- "VY"  #Variance of indicators
    VPS <- "VPS"  #Variance of factor residual
    VE <- "VE"  #Variance of factors
    TY <- "TY"  #Measurement Intercept
    ME <- "ME"  #mean of Factor
    VTE <- "VTE"  #Variance of measurement error
    AL <- "AL"  #Intercept of latent residuals
    MY <- "MY"  #mean of indicators
    
    LX <- "LX"  #Exo Factor Loading
    TD <- "TD"  #Exo Measurement Error Covariance
    RTD <- "RTD"  #Exo Measurement Error Correlation
    PH <- "PH"  #Exo factor covariance
    RPH <- "RPH"  #Exo factor correlation
    GA <- "GA"  #Path from Exo to Endo
    VX <- "VX"  #Variance of exo indicators
    VPH <- c("VK", "VPH")  #Variance of exo factors
    TX <- "TX"  #Exo measurement intercept
    KA <- c("KA", "MK")  #Exo factor mean
    VTD <- "VTD"  #Variance of exo measurement error
    MX <- "MX"  #Exo indicator mean
    TH <- "TH"  #Error Covariance between exo (row) and endo (column)
    RTH <- "RTH"  #Error Correlation of exo (row) and endo (column)
    
    loading <- c(LY, LX, "loading", "factor loading")
    errorCov <- c(TE, TD, "error.cov", "errorcov", "error covariance")
    errorCor <- c(RTE, RTD, "error.cor", "errorcor", "error correlation")
    errorVar <- c(VTD, VTE, "error.var", "errorVar", "Variance of Measurement Error")
    indicatorVar <- c(VX, VY, "indVar", "ind.var", "indicator.var", "indicatorVar", "Variance of Indicators")
    indicatorMean <- c(MX, MY, "indMean", "ind.mean", "indicator.mean", "indicatorMean", "means of Indicators")
    facCov <- c(PS, PH, "latent.cov", "latentCov", "factor.cov", "factorCov", "faccov", "factor covariance")
    facCor <- c(RPS, RPH, "latent.cor", "latentCor", "factor.cor", "factorCor", "faccor", "factor correlation")
    intercept <- c(TY, TX, "intercept", "measurement intercept")
    facMean <- c(ME, KA, AL, "factor.mean", "factor mean")
    facVar <- c(VE, VPS, VPH, "latent.var", "latentVar", "factor.var", "factorVar", "facvar", "factor variance")
    
    usedFit <- c("Chi", "AIC", "BIC", "RMSEA", "CFI", "TLI", "SRMR")
    usedFitPop <- c("f0", "rmsea", "srmr")
    optMin <- c("min", "minimum", "lower")
    optMax <- c("max", "maximum", "upper")
    optNone <- c("none", "null")
    
    result <- list(LY = LY, TE = TE, RTE = RTE, PS = PS, RPS = RPS, BE = BE, VY = VY, VPS = VPS, VE = VE, TY = TY, ME = ME, VTE = VTE, AL = AL, MY = MY, LX = LX, TD = TD, RTD = RTD, PH = PH, RPH = RPH, 
        GA = GA, VX = VX, VPH = VPH, TX = TX, KA = KA, VTD = VTD, MX = MX, TH = TH, RTH = RTH, loading = loading, errorCov = errorCov, errorCor = errorCor, errorVar = errorVar, indicatorVar = indicatorVar, 
        indicatorMean = indicatorMean, facCov = facCov, facCor = facCor, intercept = intercept, facMean = facMean, facVar = facVar, usedFit = usedFit, usedFitPop = usedFitPop, optMin = optMin, optMax = optMax, 
        optNone = optNone)
    return(result)
} 
