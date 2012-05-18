# collapseExo: Collapse all exogenous variables and put all in endogenous side only.

collapseExo <- function(object, value = 0, label = FALSE) {
    if (!isNullObject(object@GA)) {
        nk <- ncol(object@GA)
        ne <- nrow(object@GA)
        temp.BE <- combinePathExoEndo(object@GA, object@BE, value)
        temp.PS <- combineLatentCorExoEndo(object@PH, object@PS, value)
        temp.AL <- c(object@KA, object@AL)
        temp.LY <- new("NullMatrix")
        temp.TE <- new("NullMatrix")
        temp.TY <- new("NullVector")
        if (object@modelType == "SEM.exo") {
            temp.LY <- combineLoadingExoEndo(object@LX, object@LY, value)
            temp.TE <- combineMeasurementErrorExoEndo(object@TD, object@TE, object@TH)
            temp.TY <- c(object@TX, object@TY)
        }
        if (label) {
            colnames(temp.BE) <- c(colnames(object@GA), colnames(object@BE))
            colnames(temp.PS) <- c(colnames(object@GA), colnames(object@BE))
            rownames(temp.BE) <- c(colnames(object@GA), colnames(object@BE))
            rownames(temp.PS) <- c(colnames(object@GA), colnames(object@BE))
            names(temp.AL) <- c(colnames(object@GA), colnames(object@BE))
            if (object@modelType == "SEM.exo") {
                colnames(temp.LY) <- c(colnames(object@GA), colnames(object@BE))
                colnames(temp.TE) <- c(colnames(object@TD), colnames(object@TE))
                rownames(temp.LY) <- c(colnames(object@TD), colnames(object@TE))
                rownames(temp.TE) <- c(colnames(object@TD), colnames(object@TE))
                names(temp.TY) <- c(colnames(object@TD), colnames(object@TE))
            }
        } else {
            colnames(temp.BE) <- NULL
            colnames(temp.PS) <- NULL
            rownames(temp.BE) <- NULL
            rownames(temp.PS) <- NULL
            names(temp.AL) <- NULL
            if (object@modelType == "SEM.exo") {
                colnames(temp.LY) <- NULL
                colnames(temp.TE) <- NULL
                rownames(temp.LY) <- NULL
                rownames(temp.TE) <- NULL
                names(temp.TY) <- NULL
            }
        }
        return(new(is(object)[1], BE = temp.BE, PS = temp.PS, AL = temp.AL, LY = temp.LY, TE = temp.TE, TY = temp.TY, modelType = object@modelType))
    } else {
        return(object)
    }
} 
