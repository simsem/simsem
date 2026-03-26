### Sunthud Pornprasertmanit; with contributions by Terrence D. Jorgensen
### Last updated: 5 March 2026
### Internal validation functions for checking admissibility of simulated parameter sets

# validateObject()
# ------------------------------------------------------------------
# Validate whether the drawn parameters form a valid (identified) model.
#
# Arguments:
#   paramSet  A list containing a set of parameters drawn for simulation.
#
# Value:
#   Returns TRUE if the parameter set is valid.
validateObject <- function(paramSet) {
  if (!validateCovariance(paramSet$VPS, paramSet$RPS, paramSet$VE)) {
    return(FALSE)
  }
  if (!is.null(paramSet$BE)) {
    # Path or SEM
    if (!validatePath(paramSet$BE, paramSet$VE, paramSet$VE)) {
      return(FALSE)
    }
  }
  if (!is.null(paramSet$LY)) {
    # SEM or CFA
    if (!validateCovariance(paramSet$VTE, paramSet$RTE, paramSet$VY)) {
      return(FALSE)
    }
    if (!validatePath(paramSet$LY, paramSet$VE, paramSet$VY)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# validatePath()
# ------------------------------------------------------------------
# Validate whether a regression coefficient matrix (or loading matrix)
# is admissible given the corresponding variances.
#
# Arguments:
#   path    Regression coefficient or loading matrix.
#   var.iv  Variances of independent variables (columns).
#   var.dv  Variances of dependent variables (rows).
#
# Value:
#   Returns TRUE if the matrix is valid.
validatePath <- function(path, var.iv, var.dv) {
  # Need to account for multiple independent variables
  if (isTRUE(all.equal(var.iv, round(var.iv)))) {
    var.iv <- round(var.iv)
  }
  if (isTRUE(all.equal(var.dv, round(var.dv)))) {
    var.dv <- round(var.dv)
  }
  singleIV <- apply(path, 1, function(object) {
    x <- object[!is.na(object)]
    sum(x != 0) == 1
  })
  if (all(singleIV == 0)) {
    return(TRUE)
  } else {
    path <- path[which(singleIV), , drop = FALSE]
    var.dv <- var.dv[singleIV]
    inv.var.iv <- 1 / as.vector(var.iv)
    max.path <- sqrt(var.dv) %o% sqrt(inv.var.iv)
    abs.path <- abs(path)
    if (any(var.dv == 0)) {
      tmp <- abs.path[var.dv == 0, , drop = FALSE]
      if (length(tmp) > 0) max.path[var.dv == 0, ] <- tmp
    }
    ifelse(sum(abs.path > (max.path + sqrt(.Machine$double.eps))) > 0, return(FALSE), return(TRUE))
  }
}

# validateCovariance()
# ------------------------------------------------------------------
# Validate whether a set of variances and correlations produces a valid
# covariance structure.
#
# Arguments:
#   resVar       Vector of residual variances.
#   correlation  Correlation matrix.
#   totalVar     Optional vector of total variances.
#
# Value:
#   Returns TRUE if the covariance structure is valid.
validateCovariance <- function(resVar, correlation, totalVar = NULL) {
  if (!isSymmetric(correlation)) {
    return(FALSE)
  }
  if (any(is.na(resVar)) || any(resVar < 0)) {
    return(FALSE)
  }
  zero.row <- resVar == 0
  if (sum(zero.row) > 0) {
    target.rows <- correlation[which(zero.row), , drop = FALSE]
    for (i in seq_len(nrow(target.rows))) {
      temp <- target.rows[i, -zero.row[i]]
      if (sum(temp != 0) > 0) {
        return(FALSE)
      }
    }
    if (det(correlation) < 0) {
      return(FALSE)
    }
  } else {
    if (det(correlation) <= 0) {
      return(FALSE)
    }
  }
  if (!is.null(totalVar)) {
    if (sum(totalVar < 0) > 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}
