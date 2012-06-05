# checkInputValue: Check the values fed in the simMatrix, simVector, and
# symMatrix function and assign a function if the name of distribution object
# is specified

checkInputValueVector <- function(txt) {
    for (i in 1:length(txt)) {
        txt[i] <- checkInputValue(txt[i])
    }
    return(txt)
}

checkInputValue <- function(txt) {
    if (is(txt, "VirtualDist")) 
        txt <- toFunction(txt)
    if (txt == "") {
        stop("Please assign a value for any free parameters")
    }
    temp <- suppressWarnings(as.numeric(txt))
    if (is.na(temp)) {
        temp2 <- NULL
        try(temp2 <- eval(parse(text = txt)))
        if (is.null(temp2)) {
            stop(paste("The specified value", txt, "is not valid."))
        } else {
            if (is(temp2, "VirtualDist")) {
                txt <- toFunction(temp2)
            } else if (is(temp2, "numeric")) {
                # Intend to leave blank
            } else {
                stop(paste("The specified value", txt, "is not valid."))
            }
        }
    }
    return(txt)
}
 
