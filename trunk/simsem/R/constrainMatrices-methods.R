# constrainMatrices: Impose equality constraint in an object

setMethod("constrainMatrices", signature(object = "MatrixSet", SimEqualCon = "SimEqualCon"), 
    definition = function(object, SimEqualCon) {
        label.selection <- NULL
        label.selection <- c("LY", "VTE", "TE", "RTE", "VY", "TY", "MY", "BE", "VPS", 
            "PS", "RPS", "VE", "AL", "ME", "LX", "VTD", "TD", "RTD", "VX", "TX", 
            "MX", "GA", "VPH", "PH", "RPH", "KA", "TH", "RTH")
        matrices <- object
        constraint <- SimEqualCon@con
        n.constraint <- length(constraint)
        for (j in 1:n.constraint) {
            temp.constraint <- constraint[[j]]
            temp.matrix <- rownames(temp.constraint)[1]
            num <- match(temp.matrix, label.selection)
            if (num == 0) 
                stop("Cannot recognize the matrix name in the equality constraint")
            fixedvalue <- NA
            if (isMeanConstraint(rownames(temp.constraint))) {
                fixedvalue <- slot(matrices, temp.matrix)[temp.constraint[1, 2]]
            } else {
                fixedvalue <- slot(matrices, temp.matrix)[temp.constraint[1, 2], 
                  temp.constraint[1, 3]]
            }
            for (i in 2:nrow(temp.constraint)) {
                temp.matrix2 <- rownames(temp.constraint)[i]
                num <- match(temp.matrix2, label.selection)
                if (num == 0) 
                  stop("Cannot recognize the matrix name in the equality constraint")
                if (isMeanConstraint(rownames(temp.constraint))) {
                  slot(matrices, temp.matrix2)[temp.constraint[i, 2]] <- fixedvalue
                } else {
                  slot(matrices, temp.matrix2)[temp.constraint[i, 2], temp.constraint[i, 
                    3]] <- fixedvalue
                  if (isCorMatrix(slot(matrices, temp.matrix2))) 
                    slot(matrices, temp.matrix2)[temp.constraint[i, 2], temp.constraint[i, 
                      1]] <- fixedvalue
                }
            }
        }
        return(matrices)
    }) 
