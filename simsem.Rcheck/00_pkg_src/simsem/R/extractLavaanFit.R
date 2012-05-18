# extractLavaanFit: Extract fit indices from the lavaan object

extractLavaanFit <- function(Output) {
    Indices <- fitmeasures(Output)
    result <- c(Indices["chisq"], Indices["df"], Indices["pvalue"], Indices["baseline.chisq"], Indices["baseline.df"], Indices["baseline.pvalue"], Indices["cfi"], Indices["tli"], Indices["aic"], 
        Indices["bic"], Indices["rmsea"], Indices["rmsea.ci.lower"], Indices["rmsea.ci.upper"], Indices["srmr"])
    old.name <- c("chisq", "cfi", "tli", "aic", "bic", "rmsea", "srmr")
    new.name <- c("Chi", "CFI", "TLI", "AIC", "BIC", "RMSEA", "SRMR")
    name <- names(result)
    for (i in 1:length(old.name)) {
        name <- gsub(old.name[i], new.name[i], name)
    }
    names(result) <- name
    return(result)
}
 
