# matchKeywords: Search for the keywords and check whether the specified text match one in the name vector

matchKeywords <- function(Names, keywords) {
    Length <- length(Names)
    Result <- rep(NA, Length)
    for (i in 1:Length) {
        temp <- which(sapply(keywords, function(listWord, name) {
            sum(toupper(listWord) == toupper(name))
        }, name = Names[i]) > 0)
        if (length(temp) == 0) 
            temp <- 0
        Result[i] <- temp
    }
    return(Result)
} 
