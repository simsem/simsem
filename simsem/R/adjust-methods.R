# adjust: adjust a particular element in an object

## setMethod("adjust", signature(target = "SimMatrix"), definition = function(target, value, pos, numAsFixed = TRUE) {
##     if (is.vector(pos) && (length(pos) == 2)) 
##         pos <- matrix(pos, ncol = 2)
##     for (i in 1:nrow(pos)) {
##         if (is.numeric(value)) {
##             if (numAsFixed) {
##                 target@value[pos[i, 1], pos[i, 2]] <- ""
##                 target@free[pos[i, 1], pos[i, 2]] <- value
##             } else {
##                 target@value[pos[i, 1], pos[i, 2]] <- as.character(value)
##                 target@free[pos[i, 1], pos[i, 2]] <- NA
##             }
##         } else {
##             target@value[pos[i, 1], pos[i, 2]] <- checkInputValue(value)
##             target@free[pos[i, 1], pos[i, 2]] <- NA
##         }
##     }
##     return(target)  #new('SimMatrix', free=target@free, value=target@value))
## })

## setMethod("adjust", signature(target = "SymMatrix"), definition = function(target, value, pos, numAsFixed = TRUE) {
##     if (is.vector(pos) && (length(pos) == 2)) 
##         pos <- matrix(pos, ncol = 2)
##     for (i in 1:nrow(pos)) {
##         if (is.numeric(value)) {
##             if (numAsFixed) {
##                 target@value[pos[i, 1], pos[i, 2]] <- ""
##                 target@free[pos[i, 1], pos[i, 2]] <- value
##                 target@value[pos[i, 2], pos[i, 1]] <- ""
##                 target@free[pos[i, 2], pos[i, 1]] <- value
##             } else {
##                 target@value[pos[i, 1], pos[i, 2]] <- as.character(value)
##                 target@free[pos[i, 1], pos[i, 2]] <- NA
##                 target@value[pos[i, 2], pos[i, 1]] <- as.character(value)
##                 target@free[pos[i, 2], pos[i, 1]] <- NA
##             }
##         } else {
##             value <- checkInputValue(value)
##             target@value[pos[i, 1], pos[i, 2]] <- value
##             target@free[pos[i, 1], pos[i, 2]] <- NA
##             target@value[pos[i, 2], pos[i, 1]] <- value
##             target@free[pos[i, 2], pos[i, 1]] <- NA
##         }
##     }
##     return(target)  #new('SimMatrix', free=target@free, value=target@value))
## })

## setMethod("adjust", signature(target = "SimVector"), definition = function(target, value, pos, numAsFixed = TRUE) {
##     for (i in 1:length(pos)) {
##         if (is.numeric(value)) {
##             if (numAsFixed) {
##                 target@value[pos[i]] <- ""
##                 target@free[pos[i]] <- value
##             } else {
##                 target@value[pos[i]] <- as.character(value)
##                 target@free[pos[i]] <- NA
##             }
##         } else {
##             target@value[pos[i]] <- checkInputValue(value)
##             target@free[pos[i]] <- NA
##         }
##     }
##     return(target)
## }) 
