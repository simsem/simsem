

analyze <- function(model, data, package = "lavaan", miss = NULL, 
    aux = NULL, ...) {
    Output <- NULL
    DataOut <- NULL
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
	if(length(unique(model@pt$group)) == 1) {
		args$group <- NULL
		groupLab <- NULL
	}
    # if (!(model@groupLab %in% colnames(data))) {
        # data <- data.frame(data, group = 1)
        # colnames(data)[ncol(data)] <- model@groupLab
    # }
    if (!is.null(miss) && length(miss@package) != 0 && miss@package %in% c("Amelia", "mice")) {
		library(semTools)
		miArgs <- miss@args
		if(miss@package == "Amelia") {
			if(model@groupLab %in% colnames(data)) {
				if(!is.null(miArgs$idvars)) {
					miArgs$idvars <- c(miArgs$idvars, model@groupLab)
				} else {
					miArgs <- c(miArgs, list(idvars=model@groupLab))
				}
			}
		}
        Output <- runMI(model@pt, data, m = miss@m, miArgs=miArgs, chi=miss@chi, miPackage=miss@package, fun="lavaan", ...)
    } else {
		# If the missing argument is not specified and data have NAs, the default is fiml.
		if(is.null(args$missing)) {
			missing <- "default"
			if (any(is.na(data))) {
				missing <- "fiml"
			}
		} else {
			missing <- args$missing
			args$missing <- NULL
		}
        if (!is.null(aux)) {
            library(semTools)	
			
			attribute <- list(object=model@pt, aux = aux, data = data, group = groupLab, 
                model.type = model@modelType, missing = missing)
			attribute <- c(attribute, args)
			Output <- do.call(auxiliary, attribute)
			
            #Output <- auxiliary(model@pt, aux = aux, data = data, group = model@groupLab, 
            #    model.type = model@modelType, missing = missing, ...)
        } else {
			attribute <- list(model=model@pt, data = data, group = groupLab, model.type = model@modelType, 
                missing = missing)
			attribute <- c(attribute, args)
			Output <- do.call(lavaan, attribute)
			
            #Output <- lavaan(model@pt, data = data, group = model@groupLab, model.type = model@modelType, 
             #   missing = missing, ...)
        }
    }
    
    return(Output)
}

# To be used internally
anal <- function(model, data, package = "lavaan", ...) {
	groupLab <- model@groupLab
	if(length(unique(model@pt$group)) == 1) {
		groupLab <- NULL
	}
    Output <- lavaan(model@pt, data = data, group = groupLab, model.type = model@modelType, 
        ...)
    return(Output)
} 
