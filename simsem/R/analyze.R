

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
