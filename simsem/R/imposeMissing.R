### Sunthud Pornprasertmanit, Terrence D. Jorgensen, & Alexander M. Schoemann
### Last updated: 6 March 2026
### Functions for imposing missing data under different missingness mechanisms

#' Impose Missing Data Mechanisms on a Dataset
#'
#' Applies missing data mechanisms defined in a \code{SimMissing} object
#' to a dataset. Missingness may include planned missing designs,
#' missing completely at random (MCAR), missing at random (MAR),
#' attrition, or logistic regression–based missingness.
#'
#' @param miss A \code{SimMissing} object specifying the missing data design.
#' @param data.mat A data frame, matrix, or list containing a dataset.
#' @param pmMCAR Optional proportion of missing completely at random.
#' @param pmMAR Optional proportion of missing at random.
#'
#' @return A dataset with missing values imposed according to the
#' specified missing-data mechanism.
#'
#' @export
impose <- function(miss, data.mat, pmMCAR = NULL, pmMAR = NULL) {
    if (!is.null(pmMCAR))
        miss@pmMCAR <- pmMCAR
    if (!is.null(pmMAR))
        miss@pmMAR <- pmMAR
    if (is(data.mat, "list")) {
        if (!("data" %in% names(data.mat)))
            stop("The list does not contain any dataset.")
        data.mat$data <- as.data.frame(imposeMissing(data.mat$data, cov = miss@cov,
            pmMCAR = miss@pmMCAR, pmMAR = miss@pmMAR, logit = miss@logit, nforms = miss@nforms, itemGroups = miss@itemGroups,
            twoMethod = miss@twoMethod, prAttr = miss@prAttr, timePoints = miss@timePoints,
            logical = miss@logical, ignoreCols = miss@ignoreCols, threshold = miss@threshold))
    } else {
        if (is.matrix(data.mat))
            data.mat <- as.data.frame(data.mat)
        data.mat <- as.data.frame(imposeMissing(data.mat, cov = miss@cov, pmMCAR = miss@pmMCAR,
            pmMAR = miss@pmMAR, logit = miss@logit, nforms = miss@nforms, itemGroups = miss@itemGroups,
            twoMethod = miss@twoMethod, prAttr = miss@prAttr, timePoints = miss@timePoints,
            logical = miss@logical, ignoreCols = miss@ignoreCols, threshold = miss@threshold))
    }
    return(data.mat)
}


#' Wrapper for Imposing Missing Data
#'
#' Applies various missing-data mechanisms to a dataset including
#' planned missing designs, MCAR, MAR, attrition, and logistic
#' regression–based missingness.
#'
#' @param data.mat Data frame containing the dataset.
#' @param cov Covariate columns used in missingness mechanisms.
#' @param pmMCAR Proportion of missing completely at random.
#' @param pmMAR Proportion of missing at random.
#' @param nforms Number of forms used in planned missing designs.
#' @param itemGroups Optional list defining item groupings.
#' @param twoMethod Two-method planned missing specification.
#' @param prAttr Probability of attrition.
#' @param timePoints Number of time points.
#' @param ignoreCols Columns excluded from missingness.
#' @param threshold Threshold value used in MAR or attrition mechanisms.
#' @param logit Character string defining logistic missingness syntax.
#' @param logical Logical matrix specifying additional missingness.
#'
#' @return A dataset with missing values imposed.
#'
#' @export
imposeMissing <- function(data.mat, cov = 0, pmMCAR = 0, pmMAR = 0,
                          nforms = 0, itemGroups = list(),
                          twoMethod = 0, prAttr = 0, timePoints = 1,
                          ignoreCols = 0, threshold = 0, logit = "",
                          logical = NULL) {
    if (is.character(ignoreCols))
        ignoreCols <- match(ignoreCols, colnames(data.mat), nomatch = 0L)
    if (is.character(cov))
        cov <- match(cov, colnames(data.mat), nomatch = 0L)

	log.all <- matrix(FALSE, nrow(data.mat), ncol(data.mat))
    if (nforms != 0 | !isTRUE(all.equal(twoMethod, 0))) {
        # TRUE values are values to delete
        log.matpl <- plannedMissing(dim(data.mat), cov, nforms = nforms, twoMethod = twoMethod,
            itemGroups = itemGroups, timePoints = timePoints, ignoreCols = ignoreCols)
        log.all <- log.all | log.matpl
    }
    # Impose MAR and MCAR

    if (pmMCAR != 0) {
        log.mat1 <- makeMCAR(dim(data.mat), pmMCAR, cov, ignoreCols)
		log.all <- log.all | log.mat1
    }

    if (pmMAR != 0) {
        log.mat2 <- makeMAR(data.mat, pmMAR, cov, ignoreCols, threshold)
		log.all <- log.all | log.mat2
    }

	if (!is.null(logit) & (nchar(logit) > 0)) {
		log.mat2.1 <- logitMiss(data.mat, logit)
		log.all <- log.all | log.mat2.1
	}

    if (any(prAttr > 0)) {
        log.mat3 <- attrition(data.mat, prob = prAttr, timePoints, cov, threshold,
            ignoreCols)
		log.all <- log.all | log.mat3
    }

    if (!is.null(logical) && !is.null(dim(logical)) && !all(dim(logical) == 1)) {
        if (!(class(logical) %in% c("matrix", "data.frame")))
            stop("The logical argument must be matrix or data frame.")
        usecol <- setdiff(seq_len(ncol(data.mat)), ignoreCols)
        log.all2 <- log.all[, usecol]
        if ((dim(log.all2)[1] != dim(logical)[1]) | (dim(log.all2)[2] != dim(logical)[2]))
            stop("The dimension in the logical argument is not equal to the dimension in the data")
        log.all2 <- log.all2 | logical
        log.all[, usecol] <- log.all2
    }

	data.mat[log.all] <- NA

    return(data.mat)

}

#' Generate MAR Missingness Matrix
#'
#' Creates a logical matrix indicating values that should be deleted
#' according to a missing-at-random (MAR) mechanism based on a covariate.
#'
#' @param data Data frame containing the dataset.
#' @param pm Proportion of MAR missingness.
#' @param cov Column index or name of the covariate.
#' @param ignoreCols Columns excluded from missingness.
#' @param threshold Threshold value defining eligibility for missingness.
#'
#' @return Logical matrix indicating values to be set to missing.
#'
#' @keywords internal
makeMAR <- function(data, pm = NULL, cov = NULL, ignoreCols = NULL, threshold = NULL) {
    nrow <- dim(data)[1]
    ncol <- dim(data)[2]
    colList <- seq_len(ncol)
    ## because Sunthud couldn't decide between zeros and NULL
    if (all(cov == 0)) cov <- NULL
    if (all(ignoreCols == 0)) ignoreCols <- NULL
    excl <- c(cov, ignoreCols)
    misCols <- setdiff(colList, excl)

    # Calculate the probability of missing above the threshold,starting with the
    # mean of the covariate. If this probability is greater than or equal to 1,
    # lower the threshold by choosing thresholds at increasingly lower quantiles of
    # the data.
    if (is.null(threshold)) {
        threshold <- mean(data[, cov])
    }

    pr.missing <- 1
    qlist <- c(seq(0.5, 0, -0.1))
    i <- 0
    while (pr.missing >= 1 && (i < length(qlist))) {
        if (i != 0) {
            threshold <- quantile(cov, qlist[i])
        }
        percent.eligible <- (sum(data[, cov] > threshold) * length(misCols))/length(as.matrix(data[,misCols]))
        pr.missing <- pm/percent.eligible
        i <- i + 1
    }

    # mismat <- matrix(FALSE,ncol=length(colList),nrow=nrow)

    # rows.eligible <- data[,cov] > threshold

    # mismat[,misCols] <- rows.eligible

    # misrand <- runif(length(mismat)) < pr.missing

    # mismat <- matrix(mapply(`&&`,misrand,as.vector(mismat)),nrow=nrow)

    rows.eligible <- data[, cov] > threshold
    total.elig <- rep(rows.eligible, 1, each = ncol)
    misrand <- runif(length(total.elig)) < pr.missing
    mismat <- matrix(mapply(`&&`, misrand, total.elig), nrow = nrow, byrow = TRUE)

    if (length(excl)) mismat[, excl] <- FALSE

    return(mismat)
}


#' Generate MCAR Missingness Matrix
#'
#' Creates a logical matrix indicating values that should be deleted
#' according to a missing completely at random (MCAR) mechanism.
#'
#' @param dims Dimensions of the data matrix.
#' @param pm Proportion of missing completely at random.
#' @param cov Columns treated as covariates.
#' @param ignoreCols Columns excluded from missingness.
#'
#' @return Logical matrix indicating values to be set to missing.
#'
#' @keywords internal
makeMCAR <- function(dims, pm = 0, cov = 0, ignoreCols = 0) {
    nrow <- dims[1]
    ncol <- dims[2]
    colList <- seq_len(ncol)

    ## because Sunthud couldn't decide between zeros and NULL
    if (all(cov == 0)) cov <- NULL
    if (all(ignoreCols == 0)) ignoreCols <- NULL
    excl <- c(cov, ignoreCols)
    misCols <- setdiff(colList, excl)

    R.mis <- matrix(runif(nrow * ncol) <= pm, nrow = nrow)
    if (length(excl)) R.mis[, excl] <- FALSE

    return(R.mis)
}


#' Generate Planned Missingness Matrix
#'
#' Creates a logical matrix representing planned missing data designs
#' such as matrix sampling or multi-form designs.
#'
#' @param dims Dimensions of the dataset.
#' @param nforms Number of forms in the planned missing design.
#' @param itemGroups Optional list defining item groups.
#' @param twoMethod Two-method planned missing specification.
#' @param cov Covariate columns.
#' @param timePoints Number of measurement occasions.
#' @param ignoreCols Columns excluded from missingness.
#'
#' @return Logical matrix indicating values to be set to missing.
#'
#' @keywords internal
plannedMissing <- function(dims = c(0, 0), nforms = NULL,
                           itemGroups = NULL, twoMethod = NULL,
                           cov = NULL, timePoints = 1,
                           ignoreCols = NULL) {
	# TODO:
	# Warnings for illegal groupings
	# Check to see if item groupings are valid?
    if (!is.null(itemGroups) && is.list(itemGroups) && length(itemGroups) == 0)
        itemGroups <- NULL
    if (is.vector(twoMethod) && length(twoMethod) == 1 && twoMethod == 0)
        twoMethod <- NULL
    if (is.vector(nforms) && length(nforms) == 1 && nforms == 0)
        nforms <- NULL

    nitems <- dims[2]
    nobs <- dims[1]
    itemList <- seq_len(nitems)

    ## because Sunthud couldn't decide between zeros and NULL
    if (all(cov == 0)) cov <- NULL
    if (all(ignoreCols == 0)) ignoreCols <- NULL
    excl <- c(cov, ignoreCols)
    itemList <- setdiff(itemList, excl)

    itemsPerTP <- length(itemList)/timePoints

    if ((itemsPerTP - round(itemsPerTP)) != 0)
        stop("Items are not divisible by timepoints. Check the number of items and timepoints.")


    log.mat <- matrix(FALSE, ncol = itemsPerTP, nrow = nobs)

    if (!is.null(nforms) && nforms != 0) {
        if ((nforms + 1) > dims[2])
            stop("The number of forms cannot exceed the number of variables.")

        if (!is.null(itemGroups) && (nforms + 1 != length(itemGroups))) {
            nforms <- length(itemGroups) - 1
            print("Number of forms has been set to the number of groups specified")
        }

        if (((!is.null(itemGroups)) && (class(itemGroups) != "list"))) {
            stop("itemGroups not a list")
        }

        # groups items into sets of column indices (in the 3 form case, shared/a/b/c)

        if (is.null(itemGroups)) {
            itemGroups <- generateIndices(nforms + 1, 1:itemsPerTP)
        }

        # groups observations into sets of row indices. Each set receives a different
        # form - that is, each observation group has one subset of variables marked for
        # deletion. At each time point, each group of observations systematically
        # receives a different form. To do this, we calculate all possible combinations
        # for a given number of forms (for a 3 form design, this is 6) and then repeat
        # this matrix of permuations to cover all timepoints.

        obsGroups <- generateIndices(nforms, 1:nobs)
        formPerms <- matrix(unlist(permn(length(obsGroups))), ncol = nforms, byrow = TRUE)

        if (timePoints > dim(formPerms)[1]) {
            dimMult <- ceiling((timePoints - dim(formPerms)[1])/timePoints) + 1
            formPerms <- matrix(rep(formPerms, dimMult), ncol = nforms)
        }


        for (j in 1:timePoints) {
            if (j == 1) {
                temp.mat <- matrix(FALSE, ncol = itemsPerTP, nrow = nobs)

                for (i in 1:nforms) {
                  temp.mat[obsGroups[[formPerms[j, i]]], itemGroups[[i + 1]]] <- TRUE
                }
                log.mat <- temp.mat
            } else {
                temp.mat <- matrix(FALSE, ncol = itemsPerTP, nrow = nobs)
                obsGroups <- sample(obsGroups)
                for (i in 1:nforms) {
                  temp.mat[obsGroups[[i]], itemGroups[[i + 1]]] <- TRUE
                }
                log.mat <- cbind(log.mat, temp.mat)
            }

        }

        # Create the full missing matrix

        # 1) Repeat the logical matrix for each time point

        # 2) Create a logical matrix of FALSE for each covariate

        # 3) Add the columns of ignored variables to the end of the matrix, and convert
        # to data frame

        # 4) Rename the colums of the data frame

        # 5) Sort the column names



    }
	# 6) Convert back to matrix

	excl <- setdiff(excl, 0)
	if (length(excl) != 0) {
		exclMat <- matrix(rep(FALSE, nobs * length(excl)), ncol = length(excl))
		log.df <- as.data.frame(cbind(log.mat, exclMat))
		colnames(log.df) <- (c(itemList, excl))

		# The column names need to be coerced to integers for the sort to work
		# correctly, and then coerced back to strings for the data frame subsetting to
		# work correctly.
		log.df <- log.df[, paste(sort(as.integer(colnames(log.df))), sep = "")]

		log.mat <- as.matrix(log.df)
		colnames(log.mat) <- NULL

	}
    if (!is.null(twoMethod)) {
		log.mat <- matrix(FALSE, dims[1], dims[2])
        col <- unlist(twoMethod[1])
        percent <- unlist(twoMethod[2])
        toDelete <- 1:((percent) * nobs)
        log.mat[toDelete, col] <- TRUE
    }
    return(log.mat)
}

#' Generate Sequential Index Groups
#'
#' Creates groups of indices used in planned missing designs.
#'
#' @param ngroups Number of groups.
#' @param groupRange Vector of indices to divide into groups.
#' @param excl Optional indices to exclude.
#'
#' @return A list of index vectors.
#'
#' @keywords internal
generateIndices <- function(ngroups, groupRange, excl = NULL) {

    a <- groupRange

    ## because Sunthud couldn't decide whether to pass zeros or NULL
    if (any(excl == 0)) excl <- excl[excl != 0]

    if (length(excl)) {
        anot <- a[-excl]
    } else {
        anot <- a
    }

    ipg <- length(anot)/ngroups

    for (i in 1:ngroups) {
        if (i == 1) {
            index.list <- list(anot[1:ipg])
        } else {
            indices.used <- length(unlist(index.list))
            index.list[[i]] <- anot[(indices.used + 1):(ipg * i)]
        }
    }

    return(index.list)
}

#' Generate All Permutations
#'
#' Internal implementation of permutation generation used in
#' planned missing designs.
#'
#' @param x Vector of elements.
#' @param fun Optional function applied to permutations.
#'
#' @return List of permutations.
#'
#' @keywords internal
permn <- function(x, fun = NULL, ...) {
    # Taken from package combinat. Put here for easy loading.
    if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)
        x <- seq(x)
    n <- length(x)
    nofun <- is.null(fun)
    out <- vector("list", gamma(n + 1))
    p <- ip <- seqn <- 1:n
    d <- rep(-1, n)
    d[1] <- 0
    m <- n + 1
    p <- c(m, p, m)
    i <- 1
    use <- -c(1, n + 2)
    while (m != 1) {
        out[[i]] <- if (nofun)
            x[p[use]] else fun(x[p[use]], ...)
        i <- i + 1
        m <- n
        chk <- (p[ip + d + 1] > seqn)
        m <- max(seqn[!chk])
        if (m < n)
            d[(m + 1):n] <- -d[(m + 1):n]
        index1 <- ip[m] + 1
        index2 <- p[index1] <- p[index1 + d[m]]
        p[index1 + d[m]] <- m
        tmp <- ip[index2]
        ip[index2] <- ip[m]
        ip[m] <- tmp
    }
    out
}

#' Generate Attrition Missingness
#'
#' Simulates attrition across multiple time points based on specified
#' probabilities.
#'
#' @param data Dataset.
#' @param prob Probability of attrition at each time point.
#' @param timePoints Number of time points.
#' @param cov Covariate influencing attrition.
#' @param threshold Threshold used in attrition mechanism.
#' @param ignoreCols Columns excluded from attrition.
#'
#' @return Logical matrix indicating attrition-induced missing values.
#'
#' @keywords internal
attrition <- function(data, prob = NULL, timePoints = 1,
                      cov = NULL, threshold = NULL,
                      ignoreCols = NULL) {
    dims <- dim(data)
    nrow <- dims[1]

    ## because Sunthud couldn't decide between zeros and NULL
    if (all(cov == 0)) cov <- NULL
    if (all(ignoreCols == 0)) ignoreCols <- NULL

    colGroups <- generateIndices(timePoints, seq_len(dims[2]), excl = c(cov, ignoreCols))

    log.mat <- matrix(FALSE, nrow = dims[1], ncol = dims[2])

    if (length(prob) == 1L) prob <- rep(prob, timePoints)
    if (length(prob) != timePoints) {
      warning('The specified number of timepoints (', timePoints,
              ') does not coincide with the specified number of probabilities ',
              'of attrition (', length(prob), '), so only the first ',
              'probability was used (', prob[1], ').')
      prob <- rep(prob[1], timePoints)
    }

    if (is.null(cov)) {
        excl <- NULL
        for (i in seq_len(timePoints)) {
            if (is.null(excl)) {
                attr <- runif(nrow) <= prob[i]
                log.mat[attr, ] <- TRUE
                excl <- 1
            } else {
                # Grab the first column at the ith timepoint
                slice <- log.mat[, colGroups[[i]][1]]

                # Each value that isn't true has a prob likelihood of being marked true
                misrand <- runif(nrow) <= prob[i]
                attr <- mapply(`||`, slice, misrand)
                # For each row in attr marked true, mark true for all columns excluding
                # previous timepoints.
                log.mat[attr, unlist(colGroups[-excl])] <- TRUE
                excl <- c(excl, i)
            }

        }
    } else {
        if (is.null(threshold)) {
            threshold <- mean(data[, cov])
        }
        rows.eligible <- data[, cov] > threshold

        excl <- NULL
        for (i in seq_len(timePoints)) {
            if (is.null(excl)) {
                # attr <- sapply(rows.eligible,function(x) { if(x && runif(dims[1]) <= prob[i])
                # {x <- TRUE} else {x <- FALSE} })
                misrand <- runif(length(rows.eligible)) <= prob[i]
                attr <- mapply(`&&`, rows.eligible, misrand)
                log.mat[attr, unlist(colGroups)] <- TRUE
                excl <- 1
            } else {
                # Grab the first column at the ith timepoint
                prevRmv <- log.mat[, colGroups[[i]][1]]

                # Each value that isn't true has a prob likelihood of being marked true

                # attr <- mapply(function(x,y) { if(x == FALSE && y == TRUE){runif(1) <=
                # prob[i]} else {FALSE}},slice,rows.eligible)
                misrand <- runif(length(prevRmv)) <= prob[i]
                eligible <- mapply("&&", rows.eligible, misrand)
                attr <- mapply("||", eligible, prevRmv)

                # For each row in attr marked true, mark true for all columns excluding
                # previous timepoints.
                log.mat[attr, unlist(colGroups[-excl])] <- TRUE
                excl <- c(excl, i)
            }
        }
    }
    return(log.mat)
}

#' Generate Missingness from Logistic Model
#'
#' Applies a logistic regression model to generate missing values
#' based on user-specified syntax.
#'
#' @param data Dataset.
#' @param script Character string specifying logistic missingness syntax.
#'
#' @return Logical matrix indicating missing values.
#'
#' @keywords internal
logitMiss <- function(data, script) {
	model <- parseSyntaxLogitMiss(script)
	logmat <- matrix(FALSE, nrow(data), ncol(data))
	parsedModel <- lapply(model, strsplit, "~")
	dv <- sapply(parsedModel, function(x) x[[1]][1])
	if(length(dv) != length(unique(dv))) warnings("Some variables' missingnesses are defined more than once. The last expression will be used only")
	iv <- sapply(parsedModel, function(x) x[[1]][2])
	ivsep <- strsplit(iv, "\\+")

	for(i in 1:length(ivsep)) {
		temp <- strsplit(ivsep[[i]], "\\*")
		ivj <- matrix(1, nrow(data), length(temp))
		for(j in 1:length(temp)) {
			if(length(temp[[j]]) > 1) {
				ivj[,j] <- data[,temp[[j]][2]]
			}
		}
		indexp <- which(sapply(temp, function(x) length(grep("p", x[1])) > 0))
		if(length(indexp) > 1) {
			stop(paste("In the following line:\n", model[i], "\nhas the probability specification more than once"))
		} else if (length(indexp) == 1) {
			expectediv <- 0
			if(length(temp) > 1) {
				meaniv <- colMeans(ivj[,-indexp, drop=FALSE], na.rm=TRUE)
				expectediv <- sum(as.numeric(sapply(temp, function(x) x[1])[-indexp]) * meaniv)
			}
			expectedprob <- temp[[indexp]][1]
			expectedprob <- gsub("p\\(", "", expectedprob)
			expectedprob <- gsub("\\)", "", expectedprob)
			expectedprob <- 1 - as.numeric(expectedprob)

			# NOTE

			# 1/(1 + exp(-(intcept + expectslope))) = p

			# (1 - p)/p = exp(-(intcept + expectslope))

			# intcept = -log((1 - p)/p) - expectslope

			temp[[indexp]][1] <- -log(expectedprob/(1 - expectedprob)) - expectediv
		}
		if(all(sapply(temp, length) != 1)) {
			temp <- c(list(0), temp)
		}
		coef <- matrix(rep(as.numeric(sapply(temp, function(x) x[1])), nrow(data)), nrow=nrow(data), byrow=TRUE)
		pred <- apply(coef * ivj, 1, sum)
		predprob <- 1/(1 + exp(-pred))
		logmat[,which(dv[i]== colnames(data))] <- runif(length(predprob)) < predprob
	}
	logmat
}

#' Parse Logistic Missingness Syntax
#'
#' Parses the syntax used to specify logistic missingness models.
#'
#' @param script Character string specifying logistic missingness syntax.
#'
#' @return Parsed model expressions.
#'
#' @keywords internal
parseSyntaxLogitMiss <- function(script) {
# Most of the beginning of this codes are from lavaanify function in lavaan

    # break up in lines
    model <- unlist( strsplit(script, "\n") )

    # remove comments starting with '#' or '!'
    model <- gsub("#.*","", model); model <- gsub("!.*","", model)

    # replace semicolons by newlines and split in lines again
    model <- gsub(";","\n", model); model <- unlist( strsplit(model, "\n") )

    # strip all white space
    model <- gsub("[[:space:]]+", "", model)

    # keep non-empty lines only
    idx <- which(nzchar(model))
    model <- model[idx]

    # check for multi-line formulas: they contain no "~" or "=" character
    # but before we do that, we remove all modifiers
    # to avoid confusion with for example equal("f1=~x1") statements
    model.simple <- gsub("\\(.*\\)\\*", "MODIFIER*", model)

    start.idx <- grep("[~=<>:]", model.simple)
    end.idx <- c( start.idx[-1]-1, length(model) )
    model.orig    <- model
    model <- character( length(start.idx) )
    for(i in 1:length(start.idx)) {
        model[i] <- paste(model.orig[start.idx[i]:end.idx[i]], collapse="")
    }

    # ok, in all remaining lines, we should have a '~' operator
    # OR one of '=', '<' '>' outside the ""
    model.simple <- gsub("\\\".[^\\\"]*\\\"", "LABEL", model)
    idx.wrong <- which(!grepl("~", model.simple))
    if(length(idx.wrong) > 0) {
        cat("Missing ~ operator in formula(s):\n")
        print(model[idx.wrong])
        stop("Syntax error in missing model syntax")
    }
	model
}

#' Visualize Missing Proportion from Logistic Missingness Models
#'
#' Visualize the missing proportion when the logistic regression method
#' is used. The maximum number of independent variables supported for
#' visualization is two. Any additional independent variables will be
#' fixed at a specified value (default = 0).
#'
#' @param script The script used in specifying missing data using the
#' logistic regression model. See further details in the \code{logit}
#' argument of the \code{\link{miss}} function.
#' @param ylim The range of missing proportion to be plotted.
#' @param x1lim The range of the first independent variable to be plotted.
#' @param x2lim The range of the second independent variable to be plotted.
#' @param otherx The value used to fix additional independent variables.
#' @param useContour If two independent variables are present, the function
#' will produce a 3D representation. By default a contour plot is used.
#' If \code{FALSE}, a perspective plot is produced instead.
#'
#' @return
#' This function does not return a value. It produces a plot illustrating
#' the missingness probability implied by the logistic model.
#'
#' * If the number of independent variables is 0, a bar plot is produced.
#' * If the number of independent variables is 1, a logistic curve is plotted.
#' * If the number of independent variables is 2, a contour or perspective plot
#'   is produced.
#'
#' @seealso
#' \code{\link{miss}} to create the missing data template \cr
#' \code{\link{impose}} to impose missing data
#'
#' @examples
#' script <- 'y1 ~ 0.05 + 0.1*y2 + 0.3*y3
#' y4 ~ -2 + 0.1*y4
#' y5 ~ -0.5'
#' plotLogitMiss(script)
#'
#' script2 <- 'y1 ~ 0.05 + 0.5*y3
#' y2 ~ p(0.2)
#' y3 ~ p(0.1) + -1*y1
#' y4 ~ p(0.3) + 0.2*y1 + -0.3*y2
#' y5 ~ -0.5'
#' plotLogitMiss(script2)
#'
#' @export
plotLogitMiss <- function(script, ylim = c(0, 1), x1lim = c(-3, 3),
                          x2lim = c(-3, 3), otherx = 0, useContour = TRUE) {
	warnT <- as.numeric(options("warn"))
    options(warn = -1)
	model <- parseSyntaxLogitMiss(script)

	parsedModel <- lapply(model, strsplit, "~")
	dv <- sapply(parsedModel, function(x) x[[1]][1])
	if(length(dv) != length(unique(dv))) warnings("Some variables' missingnesses are defined more than once. The last expression will be used only")
	iv <- sapply(parsedModel, function(x) x[[1]][2])
	ivsep <- strsplit(iv, "\\+")

    if (length(ivsep) == 2) {
        obj <- par(mfrow = c(1, 2))
    } else if (length(ivsep) == 3) {
        obj <- par(mfrow = c(1, 3))
    } else if (length(ivsep) > 3) {
        obj <- par(mfrow = c(2, ceiling(length(ivsep)/2)))
    } else if (length(ivsep) == 1) {
        # Intentionally leaving as blank
    } else {
        stop("Some errors occur")
    }

	for(i in 1:length(ivsep)) {
		temp <- strsplit(ivsep[[i]], "\\*")

		iv1 <- seq(x1lim[1], x1lim[2], length.out=100)
		iv2 <- seq(x2lim[1], x2lim[2], length.out=100)

		indexp <- which(sapply(temp, function(x) length(grep("p", x[1])) > 0))
		if(length(indexp) > 1) {
			stop(paste("In the following line:\n", model[i], "\nhas the probability specification more than once"))
		} else if (length(indexp) == 1) {
			expectediv <- 0
			if(length(temp) > 1) {
				meaniv <- rep(otherx, length(temp) - 1)
				meaniv[1] <- mean(iv1)
				if(length(meaniv) > 1) meaniv[2] <- mean(iv2)
				expectediv <- sum(as.numeric(sapply(temp, function(x) x[1])[-indexp]) * meaniv)
			}

			expectedprob <- temp[[indexp]][1]
			expectedprob <- gsub("p\\(", "", expectedprob)
			expectedprob <- gsub("\\)", "", expectedprob)
			expectedprob <- 1 - as.numeric(expectedprob)

			# NOTE

			# 1/(1 + exp(-(intcept + expectslope))) = p

			# (1 - p)/p = exp(-(intcept + expectslope))

			# intcept = -log((1 - p)/p) - expectslope

			temp[[indexp]][1] <- -log(expectedprob/(1 - expectedprob)) - expectediv
		}
		if(all(sapply(temp, length) != 1)) {
			temp <- c(list(0), temp)
		}

		coef <- as.numeric(sapply(temp, function(x) x[1]))
		ivname <- sapply(temp, "[", 2)[sapply(temp, length) != 1]
		title <- paste("Missing Proportion of", dv[i])
        if (length(temp) == 1) {
            predVal <- 1/(1 + exp(-coef))
			barplot(c("Missing" = predVal, "Not Missing" = 1-predVal), ylab = "Missing Proportion", main = title, ylim=ylim)
        } else if (length(temp) == 2) {
			pred <- coef[1] + (coef[2] * iv1)
			predVal <- 1/(1 + exp(-pred))
            plot(iv1, predVal, type = "n", xlab = ivname[1], ylab = "Missing Proportion",
                main = title, ylim = ylim)
            lines(iv1, predVal)
        } else if (length(temp) > 2) {
            FUN <- function(x, y) {
                logi <- coef[1] + coef[2] * x + coef[3] * y
				if(length(coef) > 3) logi <- logi + sum(coef[4:length(coef)] * otherx)
                pp <- 1/(1 + exp(-logi))
                return(pp)
            }
            zpred <- outer(iv1, iv2, FUN)
            if (useContour) {
                contour(iv1, iv2, zpred, xlab = ivname[1], ylab = ivname[2],
                  main = title)
            } else {
                persp(iv1, iv2, zpred, zlim = ylim, theta = 30, phi = 30,
                  expand = 0.5, col = "lightblue", ltheta = 120, shade = 0.75, ticktype = "detailed",
                  xlab = ivname[1], ylab = ivname[2], main = title,
                  zlab = "Missing Proportion")
            }
		} else {
            stop("Something is wrong!")
        }
	}
	if (length(ivsep) > 1)
        par(obj)
    options(warn = warnT)
}
