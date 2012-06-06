# writeLavaanConstraint: Write a lavaan code for a given equality constraint

writeLavaanConstraint <- function(object, constraint) {
    object <- blankParameters(object)
    if (!is.null(constraint)) {
        con <- constraint@con
        for (i in 1:length(con)) {
            current <- con[[i]]
            con.text <- writeLavaanIndividualConstraint(rownames(current)[1], current[1, ], slot(object, rownames(current)[1]))
            for (j in 2:nrow(current)) {
                Matrix <- rownames(current)[j]
                if (Matrix == "PS" | Matrix == "PH" | Matrix == "TE" | Matrix == "TD") {
                  elements <- c(as.numeric(current[j, 2]), as.numeric(current[j, 3]))
                  slot(object, Matrix)[max(elements), min(elements)] <- con.text
				} else if (Matrix == "AL" | Matrix == "TY" | Matrix == "TX" | Matrix == "KA") {
				  slot(object, Matrix)[as.numeric(current[j, 2])] <- con.text
                } else {
                  slot(object, Matrix)[as.numeric(current[j, 2]), as.numeric(current[j, 3])] <- con.text
                }
            }
        }
    }
    return(object)
} 
