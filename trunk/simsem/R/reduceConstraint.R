# reduceConstraint: Reduce the model constraint based on SimSet to only based
# on SimRSet. Some symbols will be reduced to appropriate position such as
# VTE(3) to TE(3, 3)

reduceConstraint <- function(SimEqualCon) {
    modelType <- SimEqualCon@modelType
    equalCon <- SimEqualCon@con
    Length <- length(equalCon)
    Result <- NULL
    runnum <- 1
    for (i in 1:Length) {
        temp.result <- NULL
        temp.matrix <- equalCon[[i]]
        name <- rownames(temp.matrix)
        if (isMeanConstraint(name)) {
            if (sum(!is.element(name, c("ME", "MX", "MY"))) > 0) 
                temp.result <- temp.matrix
        } else if (isVarianceConstraint(name)) {
            if (sum(is.element(name, c("VE", "VX", "VY"))) > 0) {
                temp.result <- matrix(0, nrow(temp.matrix), 3)
                temp.result[, 1] <- temp.matrix[, 1]
                temp.result[, 2] <- temp.matrix[, 2]
                temp.result[, 3] <- temp.matrix[, 2]
                for (j in 1:length(name)) {
                  if (name[j] == "VTD") 
                    name[j] == "TD"
                  if (name[j] == "RTD") 
                    name[j] == "TD"
                  if (name[j] == "VTE") 
                    name[j] == "TE"
                  if (name[j] == "RTE") 
                    name[j] == "TE"
                  if (name[j] == "VPH") 
                    name[j] == "PH"
                  if (name[j] == "RPH") 
                    name[j] == "PH"
                  if (name[j] == "VPS") 
                    name[j] == "PS"
                  if (name[j] == "RPS") 
                    name[j] == "PS"
                }
            }
        } else {
            temp.result <- temp.matrix
        }
        if (!is.null(temp.result)) {
            Result[[runnum]] <- as.matrix(temp.result)
            runnum <- runnum + 1
        }
    }
    if (is.null(Result)) 
        Result <- list(NULL)
    return(new("SimREqualCon", con = Result, modelType = SimEqualCon@modelType))
} 
