# constrainMatrices: Impose equality constraint in an object

setMethod("constrainMatrices", signature(object = "MatrixSet", SimEqualCon = "SimEqualCon"), definition = function(object, SimEqualCon) {
    label.selection <- NULL
    label.selection <- c("LY", "VTE", "TE", "RTE", "VY", "TY", "MY", "BE", "VPS", "PS", "RPS", "VE", "AL", "ME", "LX", "VTD", "TD", "RTD", "VX", "TX", "MX", "GA", "VPH", "PH", "RPH", "KA", "TH", 
        "RTH")
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
        if (isMeanConstraint(rownames(temp.constraint)) | isVarianceConstraint(rownames(temp.constraint))) {
            fixedvalue <- slot(matrices, temp.matrix)[temp.constraint[1, 2]]
        } else {
            fixedvalue <- slot(matrices, temp.matrix)[temp.constraint[1, 2], temp.constraint[1, 3]]
        }
        for (i in 2:nrow(temp.constraint)) {
            temp.matrix2 <- rownames(temp.constraint)[i]
            num <- match(temp.matrix2, label.selection)
            if (num == 0) 
                stop("Cannot recognize the matrix name in the equality constraint")
            if (isMeanConstraint(rownames(temp.constraint)) | isVarianceConstraint(rownames(temp.constraint))) {
                slot(matrices, temp.matrix2)[temp.constraint[i, 2]] <- fixedvalue
            } else {
                slot(matrices, temp.matrix2)[temp.constraint[i, 2], temp.constraint[i, 3]] <- fixedvalue
                if (isCorMatrix(slot(matrices, temp.matrix2))) 
                  slot(matrices, temp.matrix2)[temp.constraint[i, 2], temp.constraint[i, 1]] <- fixedvalue
            }
        }
    }
    namesConstraint <- lapply(constraint, rownames)
    namesConstraint <- unique(do.call(c, namesConstraint))
    if (any(c("VTE", "RTE", "VY") %in% namesConstraint)) 
        matrices@TE <- new("NullMatrix")
    if ("TE" %in% namesConstraint) {
        matrices@VTE <- new("NullVector")
        matrices@RTE <- new("NullMatrix")
        matrices@VY <- new("NullVector")
    }
    if ("TY" %in% namesConstraint) 
        matrices@MY <- new("NullVector")
    if ("MY" %in% namesConstraint) 
        matrices@TY <- new("NullVector")
    if (any(c("VPS", "RPS", "VE") %in% namesConstraint)) 
        matrices@PS <- new("NullMatrix")
    if ("PS" %in% namesConstraint) {
        matrices@VPS <- new("NullVector")
        matrices@RPS <- new("NullMatrix")
        matrices@VE <- new("NullVector")
    }
    if ("AL" %in% namesConstraint) 
        matrices@ME <- new("NullVector")
    if ("ME" %in% namesConstraint) 
        matrices@AL <- new("NullVector")
    if (any(c("VTD", "RTD", "VX") %in% namesConstraint)) 
        matrices@TD <- new("NullMatrix")
    if ("TD" %in% namesConstraint) {
        matrices@VTD <- new("NullVector")
        matrices@RTD <- new("NullMatrix")
        matrices@VX <- new("NullVector")
    }
    if ("TX" %in% namesConstraint) 
        matrices@MX <- new("NullVector")
    if ("MX" %in% namesConstraint) 
        matrices@TX <- new("NullVector")
    if (any(c("RPH", "VPH") %in% namesConstraint)) 
        matrices@PH <- new("NullMatrix")
    if ("PH" %in% namesConstraint) {
        matrices@VPH <- new("NullVector")
        matrices@RPH <- new("NullMatrix")
    }
    if ("TH" %in% namesConstraint) 
        matrices@RTH <- new("NullMatrix")
    if ("RTH" %in% namesConstraint) 
        matrices@TH <- new("NullMatrix")
    return(matrices)
}) 
