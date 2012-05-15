# indProd: Make a product of indicators using mean centering or double-mean centering

indProd <- function(data, var1, var2, match = TRUE, meanC = TRUE, residualC = FALSE, doubleMC = TRUE, namesProd = NULL) {
    if (all(is.numeric(var1))) 
        var1 <- colnames(data)[var1]
    if (all(is.numeric(var2))) 
        var2 <- colnames(data)[var2]
    dat1 <- data[, var1]
    dat2 <- data[, var2]
    if (meanC) {
        dat1 <- scale(dat1, scale = FALSE)
        dat2 <- scale(dat2, scale = FALSE)
    }
    if (match) {
        if (length(var1) != length(var2)) 
            stop("If the match-paired approach is used, the number of variables in both sets must be equal.")
        datProd <- dat1 * dat2
        if (residualC) {
            colnames(datProd) <- paste("interactionProduct", 1:ncol(datProd), sep = "")
            temp <- data.frame(datProd, dat1, dat2)
            express <- paste("cbind(", paste(colnames(datProd), collapse = ", "), ") ~ ", paste(c(colnames(dat1), colnames(dat2)), collapse = " + "), sep = "")
            datProd <- lm(express, data = temp)$residuals
        }
        if (doubleMC) 
            datProd <- scale(datProd, scale = FALSE)
        if (is.null(namesProd)) {
            colnames(datProd) <- paste(var1, var2, sep = ".")
        } else {
            colnames(datProd) <- namesProd
        }
        data <- data.frame(data, datProd)
    } else {
        datProd <- matrix(0, nrow(data), 1)
        for (i in 1:length(var1)) {
            datProd <- data.frame(datProd, matrix(rep(dat1[, i], length(var2)), ncol = length(var2)) * dat2)
        }
        datProd <- datProd[, -1]
        if (residualC) {
            colnames(datProd) <- paste("interactionProduct", 1:ncol(datProd), sep = "")
            temp <- data.frame(datProd, dat1, dat2)
            express <- paste("cbind(", paste(colnames(datProd), collapse = ", "), ") ~ ", paste(c(colnames(dat1), colnames(dat2)), collapse = " + "), sep = "")
            datProd <- lm(express, data = temp)$residuals
        }
        if (doubleMC) 
            datProd <- scale(datProd, scale = FALSE)
        if (is.null(namesProd)) {
            temp <- NULL
            for (i in 1:length(var1)) {
                temp <- c(temp, paste(var1[i], var2, sep = "."))
            }
            colnames(datProd) <- temp
        } else {
            colnames(datProd) <- namesProd
        }
        data <- data.frame(data, datProd)
    }
    return(data)
} 
