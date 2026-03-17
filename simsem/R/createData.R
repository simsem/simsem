### Sunthud Pornprasertmanit, Patrick Miller, Terry Jorgensen; with contributions from Mikko Rönkkö
### Last updated: 5 March 2026
### generate simulated data from a drawn parameter set

#' Generate simulated data from model parameters
#'
#' Creates simulated data from a set of drawn parameters (a \code{paramSet})
#' produced by \code{\link{draw}}. This function is primarily used internally
#' by \code{sim()}, but it is available for users who want to directly generate
#' data from a parameter template or for debugging simulation workflows.
#'
#' Data can be generated using three approaches:
#'
#' \enumerate{
#' \item The model-implied method, which generates data from the model-implied
#' mean vector and covariance matrix.
#' \item The sequential method, where latent variables are generated first and
#' indicator values are obtained from structural and measurement equations.
#' \item A model-based bootstrap approach using an observed dataset.
#' }
#'
#' Non-normal data can optionally be generated using
#' \code{\linkS4class{SimDataDist}} objects created with \code{\link{bindDist}}.
#'
#' @param paramSet
#' A set of drawn parameters produced by \code{\link{draw}}.
#'
#' @param n
#' Integer specifying the desired sample size.
#'
#' @param indDist
#' A \code{\linkS4class{SimDataDist}} object (or list of such objects)
#' specifying the distribution of indicators. If a single object is
#' supplied, all indicators share the same distribution. Used when
#' \code{sequential = FALSE}.
#'
#' @param sequential
#' Logical. If \code{TRUE}, a sequential data generation method is used
#' in which latent variables are generated first and indicators are
#' obtained from the model equations. If \code{FALSE}, data are generated
#' directly from the model-implied mean vector and covariance matrix.
#'
#' @param facDist
#' A \code{\linkS4class{SimDataDist}} object (or list of such objects)
#' specifying the distribution of latent variables. Used only when
#' \code{sequential = TRUE}.
#'
#' @param errorDist
#' A \code{\linkS4class{SimDataDist}} object (or list of such objects)
#' specifying the distribution of measurement errors.
#'
#' @param saveLatentVar
#' Logical. If \code{TRUE}, latent variable scores, residual latent
#' scores, and measurement error scores are also returned. The
#' \code{sequential} method must be used.
#'
#' @param indLab
#' Optional vector of indicator labels. If not supplied, default names
#' \code{y1, y2, ...} are used.
#'
#' @param facLab
#' Optional vector of factor labels. If not supplied, default names
#' \code{f1, f2, ...} are used.
#'
#' @param modelBoot
#' Logical. If \code{TRUE}, a model-based bootstrap procedure is used
#' for data generation.
#'
#' @param realData
#' A data.frame containing real data used for the model-based bootstrap
#' procedure.
#'
#' @param covData
#' A data.frame containing covariate data. This argument is required
#' when the model template includes \code{GA} or \code{KA} matrices.
#'
#' @param empirical
#' Logical. If \code{TRUE}, generated data are forced to have exactly
#' the specified sample statistics. This option applies only when
#' multivariate normal generation is used.
#'
#' @return
#' A \code{data.frame} containing simulated data. If
#' \code{saveLatentVar = TRUE}, the function returns a list containing
#' the simulated data and additional latent variable information.
#'
#' @details
#' If no distribution object is specified, data are generated using a
#' modified version of \code{mvrnorm} from the \pkg{MASS} package. The
#' modification ensures that simulations with sample sizes \eqn{n} and
#' \eqn{n+k} (for \eqn{k>0}) produce identical first \eqn{n} observations,
#' improving reproducibility in simulation studies.
#'
#' When a \code{\linkS4class{SimDataDist}} object is supplied, non-normal
#' data can be generated using either copula-based methods or the
#' Vale–Maurelli (1983) approach implemented in \pkg{lavaan}.
#'
#' For copula-based generation, if no copula is specified in the
#' distribution object, a Gaussian copula is used. When a copula is
#' specified (e.g., \code{\link[copula]{ellipCopula}} or
#' \code{\link[copula]{archmCopula}}), the transformation approach
#' described by Mair, Satorra, and Bentler (2012) is applied:
#'
#' \deqn{Y = XS^{-1/2}\Sigma_0^{1/2}}
#'
#' where \eqn{X} is data drawn from the copula model, \eqn{S} is the
#' covariance matrix of \eqn{X}, and \eqn{\Sigma_0} is the model-implied
#' covariance matrix.
#'
#' For model-based bootstrapping, the transformation proposed by
#' Yung and Bentler (1996) is used, extending the Bollen–Stine (1992)
#' bootstrap to models including mean structures.
#'
#' @seealso
#' \code{\link{draw}},
#' \code{\link{bindDist}},
#' \code{\linkS4class{SimDataDist}}
#'
#' @references
#' Bollen, K. A., & Stine, R. A. (1992). Bootstrapping goodness-of-fit
#' measures in structural equation models.
#' \emph{Sociological Methods and Research, 21}, 205–229.
#'
#' Mair, P., Satorra, A., & Bentler, P. M. (2012). Generating nonnormal
#' multivariate data using copulas: Applications to SEM.
#' \emph{Multivariate Behavioral Research, 47}, 547–565.
#'
#' Vale, C. D., & Maurelli, V. A. (1983). Simulating multivariate
#' nonnormal distributions. \emph{Psychometrika, 48}, 465–471.
#'
#' Yung, Y.-F., & Bentler, P. M. (1996). Bootstrapping techniques in
#' analysis of mean and covariance structures.
#'
#' @examples
#' loading <- matrix(0, 6, 2)
#' loading[1:3, 1] <- NA
#' loading[4:6, 2] <- NA
#'
#' LY <- bind(loading, 0.7)
#'
#' latent.cor <- matrix(NA, 2, 2)
#' diag(latent.cor) <- 1
#' RPS <- binds(latent.cor, 0.5)
#'
#' RTE <- binds(diag(6))
#'
#' VY <- bind(rep(NA, 6), 2)
#'
#' CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType = "CFA")
#'
#' param <- draw(CFA.Model)
#'
#' dat <- createData(param[[1]], n = 200)
#'
#' @export
createData <- function(
  paramSet, n, indDist = NULL, sequential = FALSE, facDist = NULL,
  errorDist = NULL, saveLatentVar = FALSE, indLab = NULL, facLab = NULL, modelBoot = FALSE, realData = NULL, covData = NULL,
  empirical = FALSE
) {
  # Assume covData is good
  if (modelBoot) {
    if (sequential) {
      stop("The model-based bootstrap and sequential cannot be used at the same time.")
    }
    if (is.null(realData)) {
      stop("If the data are generated by model bootstrap, the real data are needed.")
    }
    if (!is.null(indLab)) {
      realData <- realData[, indLab]
    }
    if (sum(is.na(realData)) > 0) {
      stop("The model-based bootstrap is not available for data with missingness.")
    }
  }

  if (!is.null(realData)) stopifnot(n == nrow(realData))
  if (!is.null(covData)) stopifnot(n == nrow(covData))

  if (!is.null(errorDist)) {
    if (!is.null(paramSet$BE) && is.null(paramSet$LY)) {
      # Model is path analysis
      stop("errorDist is not allowed for path analysis model. The distribution of each indicator should be specified in facDist if sequential=TRUE.")
    }
  }
  if (!sequential & !is.null(facDist)) {
    stop("facDist is not allowed when using model-implied method in data generation")
  }
  if (!sequential & !is.null(errorDist)) {
    stop("errorDist is not allowed when using model-implied method in data generation")
  }
  if (sequential & !is.null(indDist)) {
    stop("indDist is not allowed when using sequential method in data generation")
  }
  # classes <- sapply(list(facDist,indDist,errorDist),class) could be a check
  # soon

  Data <- NULL
  ExtraData <- NULL
  param <- paramSet$param # FIXME: Should this be paramSet[[1]]$param? check last line of /inst/tests/test_drawParam.R
  usedParam <- NULL
  if (!is.null(paramSet$misspec)) {
    usedParam <- paramSet$misspec
  } else {
    usedParam <- param
  }

  if (modelBoot) {
    if (!is.null(covData)) realData <- data.frame(covData, realData)
    covStat <- list(MZ = as.matrix(colMeans(covData)), CZ = cov(covData))
    implied <- createImpliedMACS(usedParam, covStat)
    S <- cov(realData)
    Sigma <- implied$CM
    M <- colMeans(realData)
    M <- matrix(rep(M, n), nrow = n, byrow = TRUE)
    Mu <- implied$M
    Mu <- matrix(rep(Mu, n), nrow = n, byrow = TRUE)
    z <- (scale(realData, scale = FALSE) %*% (solve(lavaan::lav_matrix_symmetric_sqrt(S)) %*%
      lavaan::lav_matrix_symmetric_sqrt(Sigma))) + Mu
    index <- sample(1:n, replace = TRUE)
    Data <- z[index, ]
  } else {
    if (sequential) {
      latentVariableScore <- NULL
      latentResidualScore <- NULL
      measurementErrorScore <- NULL
      if (is.null(usedParam$BE) && !is.null(usedParam$LY)) {
        # CFA
        if (!is.null(facDist)) {
          fac <- dataGen(facDist, n, usedParam$AL, usedParam$PS, empirical = empirical)
        } else {
          fac <- mvrnorm(n, usedParam$AL, usedParam$PS, empirical = empirical)
          if (n == 1) fac <- rbind(fac, deparse.level = 0)
        }
        if (!is.null(covData)) {
          latentResidualScore <- fac
          fac <- fac + (as.matrix(covData) %*% t(usedParam$GA))
        }
        latentVariableScore <- fac
        trueScore <- fac %*% t(usedParam$LY)

        if (!is.null(errorDist)) {
          errorScore <- dataGen(errorDist, n, usedParam$TY, usedParam$TE, empirical = empirical)
        } else {
          errorScore <- mvrnorm(n, usedParam$TY, usedParam$TE, empirical = empirical)
          if (n == 1) errorScore <- rbind(errorScore, deparse.level = 0)
        }

        measurementErrorScore <- errorScore
        Data <- trueScore + errorScore
        if (!is.null(covData)) Data <- Data + (as.matrix(covData) %*% t(usedParam$KA))
      } else {
        usedParam2 <- NULL
        if (!is.null(usedParam$BE)) {
          # SEM or Path
          usedParam2 <- usedParam
        } else {
          stop("Incorrect model type")
        }
        set <- findRecursiveSet(usedParam2$BE)
        iv <- set[[1]]
        if (!is.null(facDist)) {
          fac <- dataGen(extractSimDataDist(facDist, iv), n, usedParam2$AL[iv],
            usedParam2$PS[iv, iv],
            empirical = empirical
          )
        } else {
          fac <- mvrnorm(n, usedParam2$AL[iv],
            usedParam2$PS[iv, iv],
            empirical = empirical
          )
          if (n == 1) fac <- rbind(fac, deparse.level = 0)
        }
        if (!is.null(covData)) {
          latentResidualScore <- fac
          fac <- fac + (as.matrix(covData) %*% t(usedParam2$GA[iv, , drop = FALSE]))
        } else if (length(set) > 1) {
          latentResidualScore <- fac
        }
        if (length(set) > 1) {
          for (i in 2:length(set)) {
            dv <- set[[i]]
            pred <- fac %*% t(usedParam2$BE[dv, iv, drop = FALSE])
            if (!is.null(facDist)) {
              res <- dataGen(extractSimDataDist(facDist, dv), n, usedParam2$AL[dv],
                usedParam2$PS[dv, dv],
                empirical = empirical
              )
            } else {
              res <- mvrnorm(n, usedParam2$AL[dv],
                usedParam2$PS[dv, dv],
                empirical = empirical
              )
              if (n == 1) res <- rbind(res, deparse.level = 0)
            }
            latentResidualScore <- cbind(latentResidualScore, res)
            new <- pred + res
            if (!is.null(covData)) new <- new + (as.matrix(covData) %*% t(usedParam2$GA[dv, , drop = FALSE]))
            fac <- cbind(fac, new)
            iv <- c(iv, set[[i]])
          }
        }
        neworder <- match(1:length(iv), iv)
        fac <- fac[, neworder]
        if (!is.null(latentResidualScore)) latentResidualScore <- latentResidualScore[, neworder]
        latentVariableScore <- fac
        if (is.null(usedParam$LY)) {
          # Path
          Data <- fac
        } else {
          # SEM
          trueScore <- fac %*% t(usedParam2$LY)

          if (!is.null(errorDist)) {
            errorScore <- dataGen(errorDist, n, usedParam2$TY, usedParam2$TE, empirical = empirical)
          } else {
            errorScore <- mvrnorm(n, usedParam2$TY, usedParam2$TE, empirical = empirical)
            if (n == 1) errorScore <- rbind(errorScore, deparse.level = 0)
          }
          measurementErrorScore <- errorScore
          Data <- trueScore + errorScore
          if (!is.null(covData)) Data <- Data + (as.matrix(covData) %*% t(usedParam2$KA))
        }
      }
      if (!is.null(covData)) Data <- data.frame(covData, Data)
      if (saveLatentVar) {
        if (!is.null(usedParam$LY)) {
          if (is.null(facLab)) facLab <- paste0("f", 1:ncol(latentVariableScore))
          colnames(latentVariableScore) <- facLab
          errorName <- indLab
          if (is.null(errorName)) errorName <- paste0("y", 1:ncol(measurementErrorScore))
          colnames(measurementErrorScore) <- paste0("res_", errorName)
          if (!is.null(latentResidualScore)) colnames(latentResidualScore) <- paste0("res_", facLab)
        } else {
          errorName <- indLab
          if (is.null(errorName)) errorName <- paste0("y", 1:ncol(latentVariableScore))
          latentVariableScore <- NULL
          if (!is.null(latentResidualScore)) colnames(latentResidualScore) <- paste0("res_", errorName)
        }
        ExtraData <- data.frame(cbind(latentVariableScore, latentResidualScore, measurementErrorScore))
      }
    } else {
      # Covariance matrix based data generation
      if (is.null(covData)) {
        macs <- createImpliedMACS(usedParam)
        if (!is.null(indDist)) {
          Data <- dataGen(indDist, n, macs$M, macs$CM, empirical = empirical)
        } else {
          Data <- mvrnorm(n, macs$M, macs$CM, empirical = empirical)
          if (n == 1) Data <- rbind(Data, deparse.level = 0)
        }
      } else {
        macs <- createImpliedConditionalMACS(usedParam, covData)
        meanMacs <- macs$M
        names(meanMacs) <- NULL
        covMacs <- macs$CM
        Data <- t(sapply(meanMacs, dataGen, dataDist = indDist, n = 1, cm = covMacs, empirical = empirical))
        Data <- data.frame(covData, Data)
      }
    }
  }
  varnames <- NULL
  if (!is.null(indLab)) {
    varnames <- indLab
  } else {
    ny <- ncol(Data)
    if (!is.null(covData)) ny <- ny - ncol(covData)
    varnames <- paste0("y", 1:ny)
  }
  if (!is.null(covData)) {
    if (is.null(colnames(covData))) colnames(covData) <- paste0("z", 1:ncol(covData))
    varnames <- c(colnames(covData), varnames)
  }
  colnames(Data) <- varnames
  Data <- as.data.frame(Data)
  if (saveLatentVar) Data <- list(Data, ExtraData)
  return(Data)
}

dataGen <- function(dataDist, n, m, cm, empirical = FALSE) {
  Data <- NULL
  # Check dim(M) dim(CM) dim(copula) are equal
  if (!is.null(dataDist)) {
    if (any(is.na(dataDist@skewness))) {
      if (dataDist@p > 1) {
        varNotZeros <- diag(cm) != 0
        dataDist2 <- dataDist
        cm2 <- cm
        if (sum(varNotZeros) < dataDist@p) {
          dataDist2 <- extractSimDataDist(dataDist, which(varNotZeros))
          cm2 <- cm[which(varNotZeros), which(varNotZeros), drop = FALSE]
        }
        for (i in 1:dataDist2@p) {
          if (dataDist2@reverse[i] == TRUE) {
            cm2[i, ] <- -1 * cm2[i, ]
            cm2[, i] <- -1 * cm2[, i]
          }
        }

        if (!inherits(dataDist@copula, "NullCopula")) {
          Mvdc <- copula::mvdc(dataDist@copula, dataDist2@margins, dataDist2@paramMargins)
          Data <- CopSEM(Mvdc, cm2, nw = n * 100, np = n)
        } else {
          r <- cov2cor(as.matrix(cm2))
          listR <- r[lower.tri(diag(dataDist2@p))]
          CopNorm <- copula::ellipCopula(
            family = "normal", dim = dataDist2@p, dispstr = "un",
            param = listR
          )

          Mvdc <- copula::mvdc(CopNorm, dataDist2@margins, dataDist2@paramMargins)
          Data <- copula::rMvdc(n, Mvdc)
        }
        if (sum(varNotZeros) < dataDist@p) {
          varZeros <- diag(cm) == 0
          constant <- matrix(0, n, sum(varZeros))
          Data <- data.frame(Data, constant)
          Data[, c(which(varNotZeros), which(varZeros))] <- Data
        }
      } else if (dataDist@p == 1) {
        if (as.matrix(cm)[1, 1] == 0) {
          Data <- rep(m[1], n)
        } else {
          # Data <- as.matrix(run(dataDist@dist[[1]], n = n))
          temp <- c(
            list(get(paste0("r", dataDist@margins[[1]]))), dataDist@paramMargins[[1]],
            list(n = n)
          )
          Data <- as.matrix(eval(as.call(temp)))
        }
      } else {
        stop("when creating a data distribution object, p cannot equal 0.")
      }
    } else {
      Data <- lavaanValeMaurelli1983(n = n, COR = cov2cor(cm), skewness = dataDist@skewness, kurtosis = dataDist@kurtosis)
    }
    for (i in 1:dataDist@p) {
      if (dataDist@reverse[i] == TRUE) {
        meanOld <- mean(Data[, i])
        anchor <- max(Data[, i])
        datNew <- anchor - Data[, i]
        Data[, i] <- datNew - mean(datNew) + meanOld
      }
    }
    if (!is.matrix(Data)) {
      Data <- as.matrix(Data)
    }
    if (any(dataDist@keepScale)) {
      Data <- scale(Data)
      Data[is.na(Data)] <- 0
      fakeDat <- mvrnorm(n, m, cm, empirical = empirical)
      if (n == 1) fakeDat <- rbind(fakeDat, deparse.level = 0)
      fakeMean <- apply(fakeDat, 2, mean)
      fakeSD <- apply(fakeDat, 2, sd)
      Data <- t(apply(Data, 1, function(y, m, s) {
        y * s + m
      }, m = fakeMean, s = fakeSD))
    }
    if (nrow(Data) == 1) {
      Data <- t(Data)
    }
  } else {
    Data <- mvrnorm(n, m, cm, empirical = empirical)
    if (n == 1) Data <- rbind(Data, deparse.level = 0)
  }
  return(Data)
}

extractSimDataDist <- function(object, pos) {
  copula <- object@copula
  if (!inherits(copula, "NullCopula")) {
    copula@dimension <- 2L
  }
  return(new("SimDataDist",
    margins = object@margins[pos], paramMargins = object@paramMargins[pos],
    p = length(pos), keepScale = object@keepScale[pos], reverse = object@reverse[pos], copula = copula,
    skewness = object@skewness[pos], kurtosis = object@kurtosis[pos]
  ))
}

# The function from Mair et al. (2012)
CopSEM <- function(copmvdc, Sigma, nw = 100000, np = 1000) {
  ## copmvdc ... joint density from mvdc()
  ## Sigma ... model VC-matrix to be approximated
  ## nw ... sample size for warm-up sample
  ## np ... sample size for production sample
  Xw <- copula::rMvdc(nw, copmvdc) ## draw warm-up sample
  Sw <- cov(Xw) ## warm-up VC matrix
  Sigma.eigen <- eigen(Sigma) ## EV decomposition Sigma
  Sigmaroot <- Sigma.eigen$vectors %*% sqrt(diag(Sigma.eigen$values)) %*% t(Sigma.eigen$vectors) ## root Sigma
  Sx.eigen <- eigen(solve(Sw)) ## EV decomposition S
  Sxroot <- Sx.eigen$vectors %*% sqrt(diag(Sx.eigen$values)) %*% t(Sx.eigen$vectors) ## root S
  X <- copula::rMvdc(np, copmvdc) ## draw production sample
  Y <- (X %*% (Sxroot) %*% Sigmaroot) ## linear combination for Y
  Y
}
