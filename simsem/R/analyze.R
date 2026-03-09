### Sunthud Pornprasertmanit & Terrence D. Jorgensen & Patrick Miller
### Last updated: 5 March 2026
### Functions to fit a model (lavaan, OpenMx, SimSem, or custom function) to data

#' Fit a model to simulated data
#'
#' Fits a model specification to data.
#' Currently supported model objects include \code{SimSem} templates
#' and \code{OpenMx} models.
#'
#' The function dispatches internally to specialized functions such as
#' \code{analyzeSimSem()} or \code{analyzeMx()} depending on the model class.
#'
#' @param model A model specification. Typically a \code{SimSem} object or
#' an \code{OpenMx} model.
#' @param data A data frame or matrix containing observed variables.
#' @param package Character string specifying the analysis engine
#' (default is \code{"lavaan"}).
#' @param miss Optional \code{SimMissing} object specifying missing-data
#' mechanisms.
#' @param aux Optional auxiliary variables used in missing-data estimation.
#' @param group Optional grouping variable.
#' @param mxMixture Logical indicating whether mixture modeling is used
#' with OpenMx.
#' @param ... Additional arguments passed to the underlying estimation
#' function.
#'
#' @return
#' A fitted model object returned by the specified analysis package
#' (e.g., a \code{lavaan} object or an \code{OpenMx} model).
#'
#' @seealso
#' \code{\link{analyzeSimSem}}, \code{\link[lavaan:lavaan]{lavaan}},
#' \code{\link[semTools:auxiliary]{auxiliary}}
#'
#' @export
analyze <- function(model, data, package = "lavaan", miss = NULL,
                    aux = NULL, group = NULL, mxMixture = FALSE, ...) {
	mc <- match.call()
	args <- list(...)
	if (is(model, "SimSem")) {
		if(!("group" %in% names(args)) & "group" %in% names(mc)) args$group <- group
		args <- c(list(model = model, data = data, package = package, miss = miss, aux = aux), args)
		out <- do.call("analyzeSimSem", args)
	} else if (is(model, "MxModel")) {
		out <- analyzeMx(object = model, data = data, groupLab = group, mxMixture = mxMixture, ...)
	} else {
		stop("Please specify an appropriate object for the 'model' argument: ",
		     "simsem model template or OpenMx object. If users wish to analyze ",
		     "the lavaan script, please use the functions in the lavaan package ",
		     "directly (e.g., sem, cfa, growth, or lavaan).")
	}
	return(out)
}

#' Analyze data using a SimSem model template
#'
#' Internal function used by \code{analyze()} to fit models specified by
#' \code{SimSem} objects. The function constructs the appropriate
#' \code{lavaan} call and optionally uses auxiliary variables for
#' missing-data estimation.
#'
#' @param model A \code{SimSem} model object.
#' @param data Data used for estimation.
#' @param package Character string specifying the analysis engine.
#' @param miss Optional \code{SimMissing} object describing missingness.
#' @param aux Optional auxiliary variables for missing-data handling.
#' @param ... Additional arguments passed to \code{lavaan}.
#'
#' @return
#' A fitted \code{lavaan} model object.
#'
#' @seealso
#' \code{\link{analyze}}
#'
#' @keywords internal
analyzeSimSem <- function(model, data, package = "lavaan",
                          miss = NULL, aux = NULL, ...) {
  Output <- NULL
  groupLab <- model@groupLab
  args <- list(...)
  if (is(data, "list")) {
    if ("data" %in% names(data)) {
      data <- data$data
    } else {
      stop("The list does not contain any 'data' slot.")
    }
  }
  if (is.null(colnames(data)))
    colnames(data) <- paste0("x", 1:ncol(data))
  if (is.null(aux)) {
    if (!is.null(miss) && !(length(miss@cov) == 1 && miss@cov == 0) && miss@covAsAux)
      aux <- miss@cov
  }
  if (length(unique(model@pt$group[model@pt$op %in% c("=~", "~~", "~", "~1", "|")])) == 1) {
    args$group <- NULL
    groupLab <- NULL
  }
  ## TDJ 2 June 2016: lavaan >= 0.6-1 requires a ParTable to have "block"
  if (is.null(model@pt$block)) model@pt$block <- model@pt$group

  ##FIXME: without runMI(), no sustainable way to automate imputation
  # if (!is.null(miss) && length(miss@package) != 0 && miss@package %in% c("Amelia", "mice")) {
  #   miArgs <- miss@args
  #   if (miss@package == "Amelia") {
  #     if (model@groupLab %in% colnames(data)) {
  #       if (!is.null(miArgs$idvars)) {
  #         miArgs$idvars <- c(miArgs$idvars, model@groupLab)
  #       } else {
  #         miArgs <- c(miArgs, list(idvars = model@groupLab))
  #       }
  #     }
  #   }
  #   Output <- lavaan.mi::runMI(model@pt, data, fun = "lavaan", ..., m = miss@m,
  #                             miArgs = miArgs, miPackage = miss@package)
  # } else {
    ## If the missing argument is not specified and data have NAs, the default is fiml.
    if (is.null(args$missing)) {
      missing <- "default"
      if (anyNA(data)) missing <- "fiml"
    } else {
      missing <- args$missing
      args$missing <- NULL
    }
    model.type <- if (tolower(model@modelType) == "sem") "sem" else "cfa"
	
	# SP: Remove c() in the list for better speed
	args$model <- model@pt
	args$data <- data
	args$group <- groupLab
	args$model.type <- model.type
	args$missing <- missing

    if (!is.null(aux)) {
      if (is.numeric(aux)) aux <- colnames(data)[aux]
	  args$aux <- aux
	  args$fun <- "lavaan"
      Output <- do.call(semTools::auxiliary, args)
    } else {
      Output <- do.call(lavaan::lavaan, args)
    }
#  }
  return(Output)
}

#' Fit a SimSem model using lavaan
#'
#' Internal helper function used to fit a model specified by a
#' \code{SimSem} object using \code{lavaan}.
#'
#' @param model A \code{SimSem} model object.
#' @param data Data used for estimation.
#' @param package Character string specifying the analysis engine.
#' @param ... Additional arguments passed to \code{lavaan}.
#'
#' @return
#' A fitted \code{lavaan} object.
#'
#' @keywords internal
anal <- function(model, data, package = "lavaan", ...) {
	groupLab <- model@groupLab
	if (length(unique(model@pt$group[model@pt$op %in% c("=~", "~~", "~", "~1", "|")])) == 1L) {
		groupLab <- NULL
	}
	## synchronize model@modelType with lavaan's model.type
	model.type <- if (tolower(model@modelType) == "sem") "sem" else "cfa"

  Output <- lavaan::lavaan(model@pt, data = data, group = groupLab,
                           model.type = model.type, ...)
  return(Output)
}

#' Analyze data using lavaan
#'
#' Internal helper used to call \code{lavaan} or related estimation
#' functions when fitting models. The function optionally incorporates
#' auxiliary variables for missing-data estimation using
#' \code{semTools::auxiliary}.
#'
#' @param args A list of arguments passed to the estimation function.
#' @param fun Character string specifying the lavaan function to call
#' (e.g., \code{"lavaan"}, \code{"sem"}, \code{"cfa"}).
#' @param miss Optional \code{SimMissing} object describing missingness.
#' @param aux Optional auxiliary variables used in estimation.
#'
#' @return
#' A fitted \code{lavaan} model object.
#'
#' @keywords internal
analyzeLavaan <- function(args, fun = "lavaan", miss = NULL, aux = NULL) {
  Output <- NULL
  if (is.null(aux)) {
    if (!is.null(miss) && !(length(miss@cov) == 1 && miss@cov == 0) && miss@covAsAux)
      aux <- miss@cov
  }
  ##FIXME: without runMI(), no sustainable way to automate imputation
  # if (!is.null(miss) && length(miss@package) != 0 && miss@package %in% c("Amelia", "mice")) {
  #   miArgs <- miss@args
  #   if (miss@package == "Amelia") {
  #     if (!is.null(args$group)) {
  #       if (args$group %in% colnames(data)) {
  #         if (!is.null(miArgs$idvars)) {
  #           miArgs$idvars <- c(miArgs$idvars, args$group)
  #         } else {
  #           miArgs <- c(miArgs, list(idvars = args$group))
  #         }
  #       }
  #     }
  #   }
  #   args$fun <- fun
  #   args$m <- miss@m
  #   args$miArgs <- miArgs
  #   args$miPackage <- miss@package
  #   Output <- do.call(semTools::runMI, args)
  # } else {
    ## If the missing argument is not specified and data have NAs, the default is fiml.
    if(is.null(args$missing)) {
      args$missing <- "default"
      if ((!is.null(miss) && (miss@m == 0)) || anyNA(args$data)) {
        args$missing <- "fiml"
      }
    }
    if (!is.null(aux)) {
      if (is.numeric(aux)) aux <- colnames(model$data)[aux]
      args$aux <- aux
      args$fun <- fun
      Output <- do.call(semTools::auxiliary, args)
    } else {
      Output <- do.call(fun, args)
    }
#  }
  return(Output)
}
