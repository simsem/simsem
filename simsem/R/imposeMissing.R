# imposeMissing: Function to impost planned, MAR and MCAR missing on a data set

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
## setMethod('run', signature = 'SimMissing', definition = function(object,
## data, pmMCAR = NULL, pmMAR = NULL) {

## })


## The wrapper function for the various functions to impose missing values.
## Currently, the function will delete x percent of eligible values for MAR and
## MCAR, if you mark colums to be ignored.
imposeMissing <- function(data.mat, cov = 0, pmMCAR = 0, pmMAR = 0, nforms = 0, itemGroups = list(), 
    twoMethod = 0, prAttr = 0, timePoints = 1, ignoreCols = 0, threshold = 0, logit = "", logical = NULL) {
    if (is.character(ignoreCols)) 
        ignoreCols <- match(ignoreCols, colnames(data.mat))
    if (is.character(cov)) 
        cov <- match(cov, colnames(data.mat))
		
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
	
    if (prAttr != 0) {
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


# Function to make MAR missing based on 1 covariate using the threshold method.

# ToDo: Extend to multiple covariates
makeMAR <- function(data, pm = NULL, cov = NULL, ignoreCols = NULL, threshold = NULL) {
    
    nrow <- dim(data)[1]
    ncol <- dim(data)[2]
    colList <- seq_len(ncol)
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

    mismat[, excl] <- FALSE
    
    return(mismat)
}


# Function to make some MCAR missing

# Input: Data matrix dimensions, desired percent missing, columns of covariates
# to not have missingness on

# Output: Logical matrix of values to be deleted
makeMCAR <- function(dims, pm = 0, cov = 0, ignoreCols = 0) {
    nrow <- dims[1]
    ncol <- dims[2]
    colList <- seq_len(ncol)
    
    excl <- c(cov, ignoreCols)
    misCols <- setdiff(colList, excl)

    R.mis <- matrix(runif(nrow * ncol) <= pm, nrow = nrow)
    R.mis[, excl] <- FALSE
    
    return(R.mis)
}


# Function to poke holes in the data for planned missing designs.

# Input: Data Set

# Output: Boolean matrix of values to delete
# 
# Right now, function defaults to NULL missingness. If number of forms is
# specified, items are divided equally and grouped sequentially. (i.e. columns
# 1-5 are shared, 6-10 are A, 11-15 are B, and 16-20 are C)

# TODO:

# Warnings for illegal groupings

# Check to see if item groupings are valid?
plannedMissing <- function(dims = c(0, 0), nforms = NULL, itemGroups = NULL, twoMethod = NULL, 
    cov = NULL, timePoints = 1, ignoreCols = NULL) {
    
    if (!is.null(itemGroups) && is.list(itemGroups) && length(itemGroups) == 0) 
        itemGroups <- NULL
    if (is.vector(twoMethod) && length(twoMethod) == 1 && twoMethod == 0) 
        twoMethod <- NULL
    if (is.vector(nforms) && length(nforms) == 1 && nforms == 0) 
        nforms <- NULL
    
    nitems <- dims[2]
    nobs <- dims[1]
    excl <- c(cov, ignoreCols)
    numExcl <- length(excl)
    
    itemList <- seq_len(nitems)
    
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


# Default generation method for item groupings and observation groupings.
# Generates sequential groups of lists of column indices based on the desired
# number of groups, and a range of the group column indices. You can also
# exclude specific column indeces.

# EX: generate.indices(3,1:12)

generateIndices <- function(ngroups, groupRange, excl = NULL) {
    
    a <- groupRange
    
    if (!is.null(excl)) {
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

# Implementing attrition using probability of attrition per TP as the
# parameter, and optionally, a covariate.  The probability argument can be a
# vector, allowing you to specify different probabilities for different time
# points.  If there is only one value, this will be the probability of
# attrition at each time time point.  If the length does not equal the number
# of time points, the pattern will repeat to cover the remaining time points.

attrition <- function(data, prob = NULL, timePoints = 1, cov = NULL, threshold = NULL, 
    ignoreCols = NULL) {
    dims <- dim(data)
    nrow <- dims[1]
    
    colGroups <- generateIndices(timePoints, seq_len(dims[2]), excl = c(cov, ignoreCols))
    
    log.mat <- matrix(FALSE, nrow = dims[1], ncol = dims[2])
    
    if (length(prob) != timePoints) {
        prob <- rep(prob, timePoints)
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


# Implementing logistic regression model for missing at random

logitMiss <- function(data, script) {
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
	
	logmat <- matrix(FALSE, nrow(data), ncol(data))
	parsedModel <- lapply(model, strsplit, "~")
	dv <- sapply(parsedModel, function(x) x[[1]][1])
	if(length(dv) != length(unique(dv))) warnings("Some variables' missingnesses are defined more than once. The last expression will be used only")
	iv <- sapply(parsedModel, function(x) x[[1]][2])
	ivsep <- strsplit(iv, "\\+")
	
	for(i in 1:length(ivsep)) {
		temp <- strsplit(ivsep[[i]], "\\*")
		pred <- 0
		for(j in 1:length(temp)) {
			expr <- temp[[j]]
			ivj <- rep(1, nrow(data))
			if(length(expr) > 1) ivj <- data[,expr[2]]
			pred <- pred + (as.numeric(expr[1]) * ivj)
		}
		predprob <- 1/(1 + exp(-pred))
		logmat[,which(dv[i]== colnames(data))] <- runif(length(predprob)) < predprob
	}
	logmat
}


 
