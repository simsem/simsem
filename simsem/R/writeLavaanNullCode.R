# writeLavaanNullCode: Write a lavaan code for a null model

writeLavaanNullCode <- function(var, aux = NULL) {
    result <- NULL
    varAll <- c(var, aux)
    varCode <- paste(paste(paste(varAll, " ~~ NA*", varAll, sep = ""), collapse = "\n"), 
        "\n")
    corCode <- outer(var, var, paste, sep = " ~~ 0*")
    diag(corCode) <- ""
    corCode <- corCode[lower.tri(corCode)]
    corCode <- paste(paste(corCode, collapse = "\n"), "\n")
    result <- paste(varCode, corCode)
    if (!is.null(aux)) {
        if (length(aux) > 1) {
            corCode2 <- outer(aux, aux, paste, sep = " ~~ NA*")
            diag(corCode2) <- ""
            corCode2 <- corCode2[lower.tri(corCode2)]
            corCode2 <- paste(paste(corCode2, collapse = "\n"), "\n")
            result <- paste(result, corCode2)
        }
        corCode3 <- paste(outer(aux, var, paste, sep = " ~~ NA*"), collapse = "\n")
        result <- paste(result, corCode3, "\n")
    }
    return(result)
} 
