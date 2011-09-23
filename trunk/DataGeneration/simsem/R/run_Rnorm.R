setMethod("run",
    signature(object = "Rnorm"),
    function (object) 
    {
        rnorm(1,object@Mean, object@SD)
    }
)
