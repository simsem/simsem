

analyze <- function(model, data, package="lavaan", simMissing=NULL,auxiliary=NULL,...) {
    Output <- NULL
    DataOut <- NULL
    args <- list(...)
##  if (class(data) == "SimDataOut") {
##         DataOut <- data
##         data <- DataOut@data
##     }
    if (is.null(colnames(data))) 
        colnames(data) <- paste0("x", 1:ncol(data))
    if (is.null(auxiliary)) {
        if (!is.null(simMissing) && !(length(simMissing@cov) == 1 && simMissing@cov == 0) && simMissing@covAsAux) 
            auxiliary <- simMissing@cov
    }
    if (is.null(indLab)) {
        if (is.null(auxiliary)) {
            indLab <- colnames(data)
        } else if (is.numeric(auxiliary)) {
            if (max(auxiliary) > ncol(data)) 
                stop("The maximum index in the auxiliary variable set is greater than the number of variables in the data.")
            indLab <- colnames(data)[-auxiliary]
        } else {
            if (length(intersect(colnames(data), auxiliary)) != length(auxiliary)) 
                stop("Some auxiliary variables does not exist in the dataset.")
            indLab <- setdiff(colnames(data), auxiliary)
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
    miss <- sum(is.na(data)) > 0
    ## if (is.null(estimator)) 
##         estimator <- estimator
##     estimator <- tolower(estimator)
    ## if (miss && !is.null(simMissing) && simMissing@numImps > 0) {
##         Output <- runMI(data, object, simMissing@numImps, simMissing@impMethod)
##     } else {
        if (package == "OpenMx") {
            Output <- runOpenMx(object, data)
        } else if (package == "lavaan") {
            fit <- lavaan(model@pt, data=data, group="group", model.type=model@modelType,...)
        }

    return(fit)
  }


    # is.equal(DataOut@param, Output@param) yes --> compute bias
    if (!is.null(DataOut)) {
        param <- DataOut@param
        check <- all.equal(param, Output@param)
        usedX <- NULL
        usedY <- NULL
        if (!(length(check) == 1 && check == TRUE) & !is.null(auxiliary)) {
            usedY <- which(!(colnames(data) %in% auxiliary))
            nx <- 0
            if (modelType == "SEM.exo") 
                nx <- nrow(param@LX)
            if (modelType == "Path.exo") 
                nx <- nrow(param@PH)
            if (nx > 0) 
                usedX <- intersect(1:nx, usedY)
            usedY <- setdiff(usedY, usedX)
            param <- extract(param, y = usedY, x = usedX)
        }
        check <- all.equal(param, Output@param)
        if (length(check) == 1 && check == TRUE) {
            paramOut <- DataOut@paramOut
            if (!is.null(auxiliary)) 
                paramOut <- extract(paramOut, y = usedY, x = usedX)
            Output@paramValue <- paramOut
        }
    }
    Output@n <- nrow(data)
    if (!is.nully(indLab)) {
        Output@indLab <- indLab
    } else {
        Output@indLab <- colnames(data)
    }
    Output@factorLab <- factorLab
    # Add labels in the SimModelOut --> go to SimModelOut and relabels it
    
    # Provide a nicer summary --> Groups elements from the same matrix together
    return(Output)
})
