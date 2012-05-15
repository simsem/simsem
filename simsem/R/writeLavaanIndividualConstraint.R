# writeLavaanIndividualConstraint: Write a lavaan code for a given equality constraint for each parameter

writeLavaanIndividualConstraint <- function(Matrix, Attribute, Names) {
    result <- "equal('"
    if (!is.na(Attribute[1])) 
        result <- paste(result, Attribute[1], ".", sep = "")
    if (length(Attribute) == 2) {
        result <- paste(result, names(Names)[as.numeric(Attribute[2])], " ~ 1')*", sep = "")
    } else if (length(Attribute) == 3) {
        Row <- as.numeric(Attribute[2])
        Column <- as.numeric(Attribute[3])
        if (Matrix == "LY" | Matrix == "LX") {
            result <- paste(result, colnames(Names)[Column], " =~ ", rownames(Names)[Row], "')*", sep = "")
        } else if (Matrix == "PS" | Matrix == "PH" | Matrix == "TE" | Matrix == "TD" | Matrix == "TH") {
            result <- paste(result, rownames(Names)[Row], " ~~ ", colnames(Names)[Column], "')*", sep = "")
        } else if (Matrix == "GA" | Matrix == "BE") {
            result <- paste(result, rownames(Names)[Row], " ~ ", colnames(Names)[Column], "')*", sep = "")
        }
    }
    return(result)
} 
