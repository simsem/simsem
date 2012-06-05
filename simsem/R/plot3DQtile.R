# plot3DQtile: Build a persepctive plot or contour plot of a quantile of
# predicted values

plot3DQtile <- function(x, y, z, df = 0, qtile = 0.5, useContour = TRUE, 
    xlab = NULL, ylab = NULL, zlab = NULL, main = NULL) {
    library(quantreg)
    if (length(qtile) > 1) 
        stop("Please use only one quantile value at a time")
    xyz <- data.frame(x = x, y = y, z = z)
    xyz <- xyz[apply(is.na(xyz), 1, sum) == 0, ]
    mod <- NULL
    if (df == 0) {
        mod <- rq(z ~ x + y + x * y, data = xyz, tau = qtile)
    } else {
        library(splines)
        mod <- rq(z ~ ns(x, df) + ns(y, df) + ns(x, df) * ns(y, df), data = xyz, 
            tau = qtile)
    }
    xseq <- seq(min(x), max(x), length = 20)
    yseq <- seq(min(y), max(y), length = 20)
    f <- function(x, y) {
        r <- mod$coefficients[1] + mod$coefficients[2] * x + mod$coefficients[3] * 
            y + mod$coefficients[3] * x * y
    }
    zpred <- outer(xseq, yseq, f)
    if (useContour) {
        contour(xseq, yseq, zpred, xlab = xlab, ylab = ylab, main = main)
    } else {
        persp(xseq, yseq, zpred, theta = 30, phi = 30, expand = 0.5, col = "lightblue", 
            ltheta = 120, shade = 0.75, ticktype = "detailed", xlab = xlab, ylab = ylab, 
            main = main, zlab = zlab)
    }
} 
