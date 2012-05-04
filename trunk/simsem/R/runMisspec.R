# runMisspec: Run parameters from SimSet object with the parameters from
# SimMisspec to be put on top of it. The final parameters will be with and
# without model misspecification.

runMisspec <- function(object, misspec, SimEqualCon = new("NullSimEqualCon"), 
    conBeforeMis = TRUE) {
    if (conBeforeMis) {
        paramSet <- run(object, SimEqualCon, makeList = TRUE)
    } else {
        paramSet <- run(object, makeList = TRUE)
    }
    Output1 <- paramSet[[1]]
    Mis <- run(misspec)
    param <- combineObject(paramSet[[2]], Mis)
    if (!isNullObject(SimEqualCon) & (conBeforeMis = FALSE)) {
        if (object@modelType != SimEqualCon@modelType) 
            stop("Please provide same tags of SimSet and constraint")
        param <- constrainMatrices(param, SimEqualCon)
    }
    Output2 <- fillParam(param, object@modelType)
    return(list(param = Output1, misspec = Output2))
} 
