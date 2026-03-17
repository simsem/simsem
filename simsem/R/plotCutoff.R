### Sunthud Pornprasertmanit
### Last updated: 6 March 2026
### Plot sampling distributions of fit indices with vertical lines of cutoffs

#' Plot sampling distributions of fit indices from a data frame
#'
#' Internal helper function used by \code{\link{plotCutoff}} and related
#' plotting functions. It visualizes sampling distributions of fit indices
#' and optionally overlays cutoff values.
#'
#' @importFrom graphics abline hist par
#' @keywords internal
plotCutoffDataFrame <- function(
  object,
  cutoff = NULL, revDirec = FALSE, usedFit = NULL, vector1 = NULL, vector2 = NULL,
  nameVector1 = NULL, nameVector2 = NULL, alpha = NULL, useContour = TRUE, cutoff2 = NULL
) {
  usedFit <- cleanUsedFit(usedFit, tolower(colnames(object)))
  object <- as.data.frame(object[, match(usedFit, tolower(colnames(object)))])
  if (!is.null(cutoff)) {
    names(cutoff) <- cleanUsedFit(names(cutoff))
    cutoff <- cutoff[usedFit]
  }
  if (!is.null(cutoff2)) {
    names(cutoff2) <- cleanUsedFit(names(cutoff2))
    cutoff2 <- cutoff2[usedFit]
  }
  object <- as.data.frame(object[, !apply(object, 2, function(vec) all(is.na(vec)))])
  colnames(object) <- usedFit
  if (ncol(object) == 2) {
    obj <- par(mfrow = c(1, 2))
  } else if (ncol(object) == 3) {
    obj <- par(mfrow = c(1, 3))
  } else if (ncol(object) > 3) {
    obj <- par(mfrow = c(2, ceiling(ncol(object) / 2)))
  } else if (ncol(object) == 1) {
    # Intentionally leaving as blank
  } else {
    stop("Some errors occur")
  }
  for (i in 1:ncol(object)) {
    val <- NULL
    if (!is.null(alpha)) {
      val <- 1 - alpha
      if (usedFit[i] %in% getKeywords()$reversedFit) {
        val <- alpha
      }
    }
    if (is.null(vector1) & is.null(vector2)) {
      hist(object[, i],
        main = colnames(object)[i], breaks = 10, col = "yellow",
        xlab = "Value"
      )
      if (!is.null(cutoff)) {
        abline(v = cutoff[i], col = "red", lwd = 3)
      }
      if (!is.null(cutoff2)) {
        abline(v = cutoff2[i], col = "red", lwd = 3)
      }
    } else if (!is.null(vector1) & is.null(vector2)) {
      plotQtile(vector1, object[, i],
        xlab = nameVector1, ylab = "Value", main = colnames(object)[i],
        qtile = val, df = 5
      )
    } else if (!is.null(vector1) & !is.null(vector2)) {
      plot3DQtile(vector1, vector2, object[, i],
        xlab = nameVector1, ylab = nameVector2,
        zlab = "Value", main = colnames(object)[i], qtile = val, useContour = useContour,
        df = 0
      )
    } else {
      stop("Something is wrong!")
    }
  }
  if (ncol(object) > 1) {
    par(obj)
  }
}

#' Plot sampling distributions of fit indices with cutoff values
#'
#' Plot sampling distributions of fit indices from a
#' \code{\linkS4class{SimResult}} object. Optional cutoff values can be
#' added either by specifying an \code{alpha} level or by supplying
#' predefined cutoffs.
#'
#' If simulation conditions vary (e.g., sample size or missing data rates),
#' the function visualizes how cutoff values change across these conditions.
#'
#' @param object A \code{\linkS4class{SimResult}} object containing simulation
#' results.
#'
#' @param alpha Significance level used to derive cutoff values.
#'
#' @param revDirec Logical indicating whether the direction of the cutoff
#' should be reversed. By default, the cutoff is placed on the side that
#' indicates worse fit (e.g., the right side of RMSEA or the left side of
#' CFI).
#'
#' @param usedFit Character vector specifying which fit indices should
#' be plotted.
#'
#' @param useContour Logical indicating whether contour plots should be
#' used when two varying parameters are present. If \code{FALSE},
#' perspective plots are produced instead.
#'
#' @return No value is returned. This function produces plots.
#'
#' @seealso
#' \itemize{
#' \item \code{\linkS4class{SimResult}} for simulation result objects.
#' \item \code{\link{getCutoff}} to compute cutoff values based on
#' null-hypothesis sampling distributions.
#' }
#'
#' @examples
#' \dontrun{
#' loading <- matrix(0, 6, 2)
#' loading[1:3, 1] <- NA
#' loading[4:6, 2] <- NA
#'
#' loadingValues <- matrix(0, 6, 2)
#' loadingValues[1:3, 1] <- 0.7
#' loadingValues[4:6, 2] <- 0.7
#'
#' LY <- bind(loading, loadingValues)
#'
#' latent.cor <- matrix(NA, 2, 2)
#' diag(latent.cor) <- 1
#'
#' RPS <- binds(latent.cor, 0.5)
#'
#' error.cor <- matrix(0, 6, 6)
#' diag(error.cor) <- 1
#'
#' RTE <- binds(error.cor)
#'
#' CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")
#'
#' Output <- sim(5, n = 200, model = CFA.Model)
#'
#' plotCutoff(Output, 0.05, usedFit = c("RMSEA", "SRMR", "CFI", "TLI"))
#'
#' Output2 <- sim(NULL, n = seq(450, 500, 10), model = CFA.Model)
#'
#' plotCutoff(Output2, 0.05)
#'
#' Output3 <- sim(NULL,
#'   n = seq(450, 500, 10),
#'   pmMCAR = c(0, 0.05, 0.1, 0.15),
#'   model = CFA.Model
#' )
#'
#' plotCutoff(Output3, 0.05)
#' }
#'
#' @export
plotCutoff <- function(
  object,
  alpha = NULL, revDirec = FALSE, usedFit = NULL, useContour = TRUE
) {
  object <- clean(object)
  cutoff <- NULL
  Data <- as.data.frame(object@fit)

  condition <- c(length(unique(object@pmMCAR)) > 1, length(unique(object@pmMAR)) >
    1, length(unique(object@n)) > 1)
  condValue <- cbind(object@pmMCAR, object@pmMAR, object@n)
  colnames(condValue) <- c("Percent MCAR", "Percent MAR", "N")
  if (!is.null(alpha)) {
    if (revDirec) {
      alpha <- 1 - alpha
    }
    if (all(!condition)) {
      cutoff <- getCutoffDataFrame(Data, alpha)
    }
  }
  if (sum(condition) == 0) {
    plotCutoffDataFrame(Data, cutoff, revDirec, usedFit)
  } else if (sum(condition) == 1) {
    plotCutoffDataFrame(Data, cutoff, revDirec, usedFit,
      vector1 = condValue[, condition],
      nameVector1 = colnames(condValue)[condition], alpha = alpha
    )
  } else if (sum(condition) == 2) {
    condValue <- condValue[, condition]
    plotCutoffDataFrame(Data, cutoff, revDirec, usedFit,
      vector1 = condValue[, 1], vector2 = condValue[
        ,
        2
      ], nameVector1 = colnames(condValue)[1], nameVector2 = colnames(condValue)[2],
      alpha = alpha, useContour = useContour
    )
  } else {
    stop("This function cannot plot when there more than two dimensions of varying parameters")
  }
}

#' Plot quantile surfaces for two predictors
#'
#' Internal function that builds contour or perspective plots of predicted
#' quantiles from quantile regression models.
#'
#' @importFrom graphics contour persp
#' @keywords internal
plot3DQtile <- function(
  x, y, z, df = 0, qtile = 0.5, useContour = TRUE, xlab = NULL,
  ylab = NULL, zlab = NULL, main = NULL
) {
  if (length(qtile) > 1) {
    stop("Please use only one quantile value at a time")
  }
  xyz <- data.frame(x = x, y = y, z = z)
  xyz <- xyz[apply(is.na(xyz), 1, sum) == 0, ]
  mod <- NULL
  if (df == 0) {
    mod <- quantreg::rq(z ~ x + y + x * y, data = xyz, tau = qtile)
  } else {
    requireNamespace("splines")
    if (!("package:splines" %in% search())) attachNamespace("splines")
    mod <- quantreg::rq(z ~ ns(x, df) + ns(y, df) + ns(x, df) * ns(y, df),
      data = xyz,
      tau = qtile
    )
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
    persp(xseq, yseq, zpred,
      theta = 30, phi = 30, expand = 0.5, col = "lightblue",
      ltheta = 120, shade = 0.75, ticktype = "detailed", xlab = xlab, ylab = ylab,
      main = main, zlab = zlab
    )
  }
}

#' Plot conditional quantile curves
#'
#' Internal helper function used to visualize conditional quantiles
#' estimated via quantile regression.
#'
#' @importFrom graphics lines
#' @keywords internal
plotQtile <- function(x, y, df = 0, qtile = NULL, ...) {
  xy <- data.frame(x = x, y = y)
  plot(x, y, ...)
  if (!is.null(qtile)) {
    mod <- NULL
    requireNamespace("quantreg")
    if (!("package:quantreg" %in% search())) attachNamespace("quantreg")
    if (df == 0) {
      mod <- quantreg::rq(y ~ x, tau = qtile)
    } else {
      requireNamespace("splines")
      if (!("package:splines" %in% search())) attachNamespace("splines")
      mod <- quantreg::rq(y ~ ns(x, df), tau = qtile)
    }
    xseq <- seq(min(x), max(x), length = nrow(xy))
    pred <- predict(mod, data.frame(x = xseq), interval = "none", level = 0.95)
    pred <- as.matrix(pred)
    for (i in 1:ncol(pred)) {
      lines(xseq, pred[, i], col = "red")
    }
  }
}
