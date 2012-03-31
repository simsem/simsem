fitMeasures <- function(X2, df, p, X2.null, df.null, p.null, N, fit.measures="all") {

    fit.measures <- tolower(fit.measures)
    if("all" %in% fit.measures) {
        chisq <- c("chisq", "df", "pvalue")
        baseline <- c("baseline.chisq", "baseline.df", "baseline.pvalue")
        cfi.tli <- c("cfi", "tli")
        logl <- c("logl", "unrestricted.logl", "npar", "aic", "bic", "ntotal")
        logl <- c(logl, "bic2")
        rmsea.ci <- c("rmsea", "rmsea.ci.lower", "rmsea.ci.upper")
        srmr <- c("srmr")
		fit.measures <- c(chisq, baseline, cfi.tli, logl, rmsea.ci, srmr)
    }
    indices <- list()

    # Chi-square value estimated model (H0)
    if(any(c("chisq") %in% fit.measures)) {
		indices["Chi"] <- X2
    }
    if(any(c("df", "df.scaled") %in% fit.measures)) {
        indices["df"] <- df
    }
    if(any(c("pvalue", "pvalue.scaled") %in% fit.measures)) {
        indices["pvalue"] <- p
    }


    if(any(c("cfi", "tli", "baseline.chisq", "baseline.pvalue") %in% fit.measures)) {
       if("baseline.chisq" %in% fit.measures) {
            indices["baseline.Chi"] <- X2.null
        }
        if("baseline.df" %in% fit.measures) {
            indices["baseline.df"] <- df.null
        }
        if("baseline.pvalue" %in% fit.measures) {
            indices["baseline.pvalue"] <- p.null
        }
        # CFI 
        if("cfi" %in% fit.measures) {
            indices["CFI"] <- ( 1 - max(c(X2 - df,0)) / 
                                    max( c(X2-df, X2.null-df.null, 0) ) 
                              )
        }
        # TLI 
        if("tli" %in% fit.measures) {
            if(df > 0) {
                TLI <- (X2.null/df.null - X2/df)/(X2.null/df.null - 1)
				#if(TLI < 0) TLI <- NaN
            } else {
                TLI <- 1
            }
            indices["TLI"] <- TLI
        }
    }

    if(any(c("rmsea") %in% fit.measures)) {
        # RMSEA
        if(df > 0) {
            RMSEA <- sqrt( max( c((X2/N)/df - 1/N, 0) ) )
        } else {
            RMSEA <- 0
        }
        indices["RMSEA"] <- RMSEA
    }

    if("rmsea.ci.lower" %in% fit.measures) {
        lower.lambda <- function(lambda) {
            (pchisq(X2, df=df, ncp=lambda) - 0.95)
        }
		
        if(df < 1 || lower.lambda(0) < 0.0) {
            indices["RMSEA.ci.lower"] <- 0
        } else {
            lambda.l <- try(uniroot(f=lower.lambda, lower=0, upper=X2)$root)
            if(inherits(lambda.l, "try-error")) { lambda.l <- NA }
            indices["RMSEA.ci.lower"] <- sqrt( lambda.l/(N*df) )
        }
    }

	N.RMSEA <- max(N, X2*2)
    if("rmsea.ci.upper" %in% fit.measures) {
        upper.lambda <- function(lambda) {
            (pchisq(X2, df=df, ncp=lambda) - 0.05)
        }
        if(df < 1 || upper.lambda(N.RMSEA) > 0 || upper.lambda(0) < 0) {
            indices["RMSEA.ci.upper"] <- 0
        } else {
            lambda.u <- try(uniroot(f=upper.lambda, lower=0, upper=N.RMSEA)$root)
            if(inherits(lambda.u, "try-error")) { lambda.u <- NA }
            indices["RMSEA.ci.upper"] <- sqrt( lambda.u/(N*df) )
        }
    }

    out <- unlist(indices)

    if(length(out) > 0L) {
        class(out) <- c("lavaan.vector", "numeric")
    } else {
        return( invisible(numeric(0)) )
    }

    out
}

