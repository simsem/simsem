# simEqualCon: The constructor of the equality constraints

simEqualCon <- function(..., modelType) {
    List <- list(...)
    Length <- length(List)
    Result <- NULL
    for (i in 1:Length) {
        temp.result <- NULL
        temp.matrix <- List[[i]]
        if (is.matrix(temp.matrix) == FALSE) {
            temp.matrix2 <- as.matrix(temp.matrix)
            rownames(temp.matrix2) <- names(temp.matrix)
            temp.matrix <- temp.matrix2
        }
        rownames(temp.matrix) <- reassignNames(modelType, rownames(temp.matrix))
        if (isMeanConstraint(rownames(temp.matrix)) | isVarianceConstraint(rownames(temp.matrix))) {
            temp.result <- matrix(NA, nrow(temp.matrix), 2)
            colnames(temp.result) <- c("Group", "Element")
            rownames(temp.result) <- rownames(temp.matrix)
            if (ncol(temp.matrix) == 1) {
                temp.result[, 2] <- temp.matrix[, 1]
            } else if (ncol(temp.matrix) == 2) {
                temp.result[, 1:2] <- temp.matrix[, 1:2]
            }
        } else {
            temp.result <- matrix(NA, nrow(temp.matrix), 3)
            colnames(temp.result) <- c("Group", "Row", "Column")
            rownames(temp.result) <- rownames(temp.matrix)
            if (ncol(temp.matrix) == 2) {
                temp.result[, 2:3] <- temp.matrix[, 1:2]
            } else if (ncol(temp.matrix) == 3) {
                temp.result[, 1:3] <- temp.matrix[, 1:3]
            }
        }
        Result[[i]] <- as.matrix(temp.result)
    }
    return(new("SimEqualCon", con = Result, modelType = modelType))
} 
