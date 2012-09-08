

analyze <- function(model, data, package="lavaan", miss=NULL,indLab=NULL,aux=NULL,...) {
  Output <- NULL
  DataOut <- NULL
  args <- list(...)
  if (is(data, "list")) {
	if("data" %in% names(data)) {
	  data <- data$data
	} else {
	  stop("The list does not contain any 'data' slot.")
	}
  }
  if (is.null(colnames(data))) 
    colnames(data) <- paste0("x", 1:ncol(data))
  if (is.null(aux)) {
    if (!is.null(miss) && !(length(miss@cov) == 1 && miss@cov == 0) && miss@covAsAux) 
      auxiliy <- miss@cov
  }
  if (is.null(indLab)) {
    if (is.null(aux)) {
      indLab <- colnames(data)
    } else if (is.numeric(aux)) {
      if (max(aux) > ncol(data)) 
        stop("The maximum index in the auxiliary variable set is greater than the number of variables in the data.")
      indLab <- colnames(data)[-aux]
    } else {
      if (length(intersect(colnames(data), aux)) != length(aux)) 
        stop("Some auxiliary variables does not exist in the dataset.")
      indLab <- setdiff(colnames(data), aux)
    }
  }
  ##  if (is.numeric(indLab)) 
  ##         indLab <- colnames(data)[indLab]
  ##     if (is.numeric(auxiliary)) 
  ##         auxiliary <- colnames(data)[auxiliary]
  ##     if (length(intersect(auxiliary, indLab)) != 0) 
  ##         stop("There is common variable between the variables in the model and the auxiliary variables.")
  ##     targetCol <- c(indLab, auxiliary)
  ##     data <- data[, targetCol]
  ##    miss <- sum(is.na(data)) > 0
  
  if(!(model@groupLab %in% colnames(data))) {
	data <- data.frame(data, group=1)
	colnames(data)[ncol(data)] <- model@groupLab
  }
  if (!is.null(miss) && length(miss@package) != 0 && miss@package == "Amelia") {
    Output <- runMI(data, model, miss@args)
  } else {
    missing <- "default"
    if(any(is.na(data))) missing <- "fiml"
	if(!is.null(aux)) {
		library(semTools)
		Output <- auxiliary(model@pt, aux=aux, data=data, group=model@groupLab, model.type=model@modelType,missing=missing,...)
	} else {
		Output <- lavaan(model@pt, data=data, group=model@groupLab, model.type=model@modelType,missing=missing,...)
	}
  }

    return(Output)
}

# To be used internally
anal <- function(model, data, package="lavaan", ...) {
  args <- list(...)
  Output <- lavaan(model@pt, data=data, group="group", model.type=model@modelType,...)    
  return(Output)
}

##     # is.equal(DataOut@param, Output@param) yes --> compute bias
##     if (!is.null(DataOut)) {
##         param <- DataOut@param
##         check <- all.equal(param, Output@param)
##         usedX <- NULL
##         usedY <- NULL
##         if (!(length(check) == 1 && check == TRUE) & !is.null(auxiliary)) {
##             usedY <- which(!(colnames(data) %in% auxiliary))
##             nx <- 0
##             if (modelType == "SEM.exo") 
##                 nx <- nrow(param@LX)
##             if (modelType == "Path.exo") 
##                 nx <- nrow(param@PH)
##             if (nx > 0) 
##                 usedX <- intersect(1:nx, usedY)
##             usedY <- setdiff(usedY, usedX)
##             param <- extract(param, y = usedY, x = usedX)
##         }
##         check <- all.equal(param, Output@param)
##         if (length(check) == 1 && check == TRUE) {
##             paramOut <- DataOut@paramOut
##             if (!is.null(auxiliary)) 
##                 paramOut <- extract(paramOut, y = usedY, x = usedX)
##             Output@paramValue <- paramOut
##         }
##     }
##     Output@n <- nrow(data)
##     if (!is.nully(indLab)) {
##         Output@indLab <- indLab
##     } else {
##         Output@indLab <- colnames(data)
##     }
##     Output@factorLab <- factorLab
##     # Add labels in the SimModelOut --> go to SimModelOut and relabels it
    
##     # Provide a nicer summary --> Groups elements from the same matrix together
##     return(Output)
## })
