# runMisspec: Run parameters from SimSet object with the parameters from
# SimMisspec to be put on top of it. The final parameters will be with and
# without model misspecification.

runMisspec <- function(object, misspec, SimEqualCon = new("NullSimEqualCon")) {
    Output1 <- NULL
    Output2 <- NULL
    Mis <- run(misspec)
    if (isNullObject(SimEqualCon)) {
        if (misspec@misBeforeFill) {
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[2]], Mis)
            Output2 <- fillParam(param, object@modelType)
        } else {
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[1]], Mis)
            Output2 <- fillParam(param, object@modelType)
        }
    } else {
        if (object@modelType != SimEqualCon@modelType) 
            stop("Please provide same tags of SimSet and constraint")
        if (misspec@misBeforeFill & misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 2) Con, AddMis, fill
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[2]], Mis)
            Output2 <- fillParam(param, object@modelType)
        } else if (!misspec@misBeforeFill & misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 1) Con, fillBefore, AddMis
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[1]], Mis)
            Output2 <- fillParam(param, object@modelType)
        } else if (!misspec@misBeforeFill & misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 3) fillBefore, Con, AddMis misspec@misBeforeFill=F & misspec@conBeforeMis=T
            # SimEqualCon@conBeforeFill=F; FTF
            paramSet <- run(object, SimEqualCon, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[2]], Mis)
            Output2 <- fillParam(param, object@modelType)
            
        } else if (!misspec@misBeforeFill & !misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 4) fillBefore, AddMis, Con isBeforeFill=F & misspec@conBeforeMis=F
            # SimEqualCon@conBeforeFill=F; FFF
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[1]], Mis)
            param <- constrainMatrices(param, SimEqualCon)
            Output2 <- fillParam(param, object@modelType)
        } else if (misspec@misBeforeFill & !misspec@conBeforeMis & SimEqualCon@conBeforeFill) {
            # 5) AddMis, Con, fill misspec@misBeforeFill=T & misspec@conBeforeMis=F
            # SimEqualCon@conBeforeFill=T; TFT
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[2]], Mis)
            param <- constrainMatrices(param, SimEqualCon)
            Output2 <- fillParam(param, object@modelType)
            
        } else if (misspec@misBeforeFill & !misspec@conBeforeMis & !SimEqualCon@conBeforeFill) {
            # 6) AddMis, fill, Con misspec@misBeforeFill=T & misspec@conBeforeMis=F
            # SimEqualCon@conBeforeFill=F; TFF ; No FFT and TTF
            paramSet <- run(object, makeList = TRUE)
            Output1 <- paramSet[[1]]
            param <- combineObject(paramSet[[2]], Mis)
            param <- fillParam(param, object@modelType)
            param <- constrainMatrices(param, SimEqualCon)
            Output2 <- fillParam(param, object@modelType)
            
        } else {
            stop("The specifications of 'misBeforeFill' and 'conBeforeMis' in the SimMisspec object and the 'conBeforeFill' in the SimEqualCon are not consistent. Change one of those specifications.")
        }
    }
    return(list(param = Output1, misspec = Output2, misspecAdd = Mis))
} 
