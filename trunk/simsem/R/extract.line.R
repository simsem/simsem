extractLine <- function(text, line=1) {
    Completed.text <- strsplit(text[line], " ")
    Completed.screen <- Completed.text[[1]] == ""
    Completed.split <- Completed.text[[1]][!Completed.screen]
    return(Completed.split)
}
