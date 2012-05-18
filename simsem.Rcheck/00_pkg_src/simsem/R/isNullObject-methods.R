# isNullObject: Check whether the object is the NULL type of that class

setMethod("isNullObject", signature(target = "vector"), definition = function(target) {
    is(target, "NullVector") || is.nan(target) || (sum(length(target)) == 0)
})

setMethod("isNullObject", signature(target = "matrix"), definition = function(target) {
    is(target, "NullMatrix") || is.nan(target) || (sum(dim(target)) == 0)
})

setMethod("isNullObject", signature(target = "SimMatrix"), definition = function(target) {
    is(target, "NullSimMatrix")
})

setMethod("isNullObject", signature(target = "SymMatrix"), definition = function(target) {
    is(target, "NullSymMatrix")
})

setMethod("isNullObject", signature(target = "SimVector"), definition = function(target) {
    is(target, "NullSimVector")
})

setMethod("isNullObject", signature = "SimSet", definition = function(target) {
    is(target, "NullSimSet")
})

setMethod("isNullObject", signature = "SimEqualCon", definition = function(target) {
    is(target, "NullSimEqualCon")
})

setMethod("isNullObject", signature = "SimREqualCon", definition = function(target) {
    is(target, "NullSimREqualCon")
})

setMethod("isNullObject", signature = "SimMisspec", definition = function(target) {
    is(target, "NullSimMisspec")
})

setMethod("isNullObject", signature = "VirtualRSet", definition = function(target) {
    is(target, "NullRSet")
})

setMethod("isNullObject", signature = "data.frame", definition = function(target) {
    (is(target, "NullDataFrame") || (nrow(target) == 0) || (ncol(target) == 0))
})

setMethod("isNullObject", signature = "SimMissing", definition = function(target) {
    is(target, "NullSimMissing")
})

setMethod("isNullObject", signature = "SimDataDist", definition = function(target) {
    is(target, "NullSimDataDist")
})

setMethod("isNullObject", signature = "SimFunction", definition = function(target) {
    is(target, "NullSimFunction")
}) 
