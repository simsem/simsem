extract.lavaan.summary <- function(Output) {
    Indices <- fitmeasures(Output)
	result <- c(Indices["chisq"], Indices["df"], Indices["pvalue"],
		Indices["baseline.chisq"], Indices["baseline.df"], Indices["baseline.pvalue"],
		Indices["cfi"], Indices["tli"], Indices["aic"], Indices["bic"], Indices["rmsea"], Indices["rmsea.ci.lower"], Indices["rmsea.ci.upper"],
		Indices["srmr"])
	old.name <- c("chisq", "cfi", "tli", "aic", "bic", "rmsea", "srmr")
	new.name <- c("Chi", "CFI", "TLI", "AIC", "BIC", "RMSEA", "SRMR")
	name <- names(result)
	for(i in 1:length(old.name)) { name <- gsub(old.name[i], new.name[i], name) }
	names(result) <- name
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
