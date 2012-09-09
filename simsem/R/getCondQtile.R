## getCondQtile: Get a quantile of a variable given values of predictors

getCondQtile <- function(y, x = NULL, xval = NULL, df = 0, qtile = 0.5) {
    if (is.null(x)) {
        return(quantile(y, probs = qtile, na.rm = TRUE))
    } else {
        if (!is.matrix(x)) 
            x <- as.matrix(x)
        p <- ncol(x)
        name <- paste("x", 1:p, sep = "")
        colnames(x) <- name
        if (df == 0) {
            name2 <- name
        } else {
            library(splines)
            name2 <- paste("ns(", name, ",", df, ")", sep = "")
        }
        firstord <- paste(name2, collapse = " + ")
        FUN <- function(x, y) paste(x, " * ", y, sep = "")
        secondord <- outer(name2, name2, FUN)[lower.tri(diag(length(name2)))]
        secondord2 <- paste(secondord, collapse = " + ")
        if (secondord2 == "") {
            express <- paste("y ~ ", firstord, sep = "")
        } else {
            express <- paste("y ~ ", firstord, " + ", secondord2, sep = "")
        }
        dat <- data.frame(y = y, x)
        library(quantreg)
        mod <- rq(express, data = dat, tau = qtile)
        if (length(xval) == 1 && xval == "all") {
            result <- predict(mod, as.data.frame(x), interval = "none")
        } else {
            names(xval) <- name
            xvalSecondord <- outer(as.vector(xval), as.vector(xval), "*")[lower.tri(diag(length(xval)))]
            predictorVal <- c(1, xval, xvalSecondord)
            pred <- data.frame(t(xval))
            colnames(pred) <- colnames(x)
            result <- predict(mod, pred, interval = "none")
        }
        return(result)
    }
} 
