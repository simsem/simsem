# show: Overwrite the default of the show method that prints everything

setMethod("show", signature(object = "ANY"), function(object) {
    summaryShort(object)
}) 
