extract.lavaan.summary <- function(Output) {
    Indices <- fitmeasures(Output)
	result <- list(Chi=Indices["chisq"], df=Indices["df"], p.Chi=Indices["pvalue"],
		Chi.base=Indices["baseline.chisq"], df.base=Indices["baseline.df"], p.Chi.base=Indices["baseline.pvalue"],
		CFI=Indices["cfi"], TLI=Indices["tli"], AIC=Indices["aic"], BIC=Indices["bic"], RMSEA=Indices["rmsea"], CI.90.RMSEA=c(Indices["rmsea.ci.lower"], Indices["rmsea.ci.upper"]),
		p.RMSEA=Indices["rmsea.pvalue"], SRMR=Indices["srmr"])
	return(result)
}



# extract.lavaan.summary <- function(Output) {
    # templine <- grep("Minimum Function Chi-square", Output)
    # Chi <- as.numeric(extract.line(Output, templine[1])[4])
    # templine <- grep("Degrees of freedom", Output)
    # degree.freedom <- as.numeric(extract.line(Output, templine[1])[4])
    # templine <- grep("P-value", Output)
    # p.Chi <- as.numeric(extract.line(Output, templine[1])[2])
       
	# templine <- grep("Minimum Function Chi-square", Output)
    # Chi.base <- as.numeric(extract.line(Output, templine[2])[4])
    # templine <- grep("Degrees of freedom", Output)
    # degree.freedom.base <- as.numeric(extract.line(Output, templine[2])[4])
    # templine <- grep("P-value", Output)
    # p.Chi.base <- as.numeric(extract.line(Output, templine[2])[2])
	
	# templine <- grep("Comparative Fit Index", Output)
    # CFI <- as.numeric(extract.line(Output, templine)[5])
    # templine <- grep("Tucker-Lewis Index", Output)
    # TLI <- as.numeric(extract.line(Output, templine)[4])

	# templine <- grep("Akaike", Output)
    # AIC <- as.numeric(extract.line(Output, templine)[3])
    # templine <- grep("Bayesian", Output)
    # BIC <- as.numeric(extract.line(Output, templine)[3])
	
	# templine <- grep("RMSEA", Output)
    # RMSEA <- as.numeric(extract.line(Output, templine)[2])
    # lower.RMSEA <- as.numeric(extract.line(Output, templine + 1)[5])	
    # upper.RMSEA <- as.numeric(extract.line(Output, templine + 1)[6])	
    # p.RMSEA <- as.numeric(extract.line(Output, templine + 2)[5])

	# templine <- grep("SRMR", Output)
	# SRMR <- as.numeric(extract.line(Output, templine)[2])
	# result <- list(Chi=Chi, df=degree.freedom, p.Chi=p.Chi,
		# Chi.base=Chi.base, df.base=degree.freedom.base, p.Chi.base=p.Chi.base,
		# CFI=CFI, TLI=TLI, AIC=AIC, BIC=BIC, RMSEA=RMSEA, CI.90.RMSEA=c(lower.RMSEA, upper.RMSEA),
		# p.RMSEA=p.RMSEA, SRMR=SRMR)
	# return(result)
# }
