setMethod("run",
    signature(object = "Rnorm"),
    function (object) 
    {
        rnorm(1,object@Mean, object@SD)
    }
)

setMethod("run",
    signature(object = "Runif"),
    function (object) 
    {
        runif(1,object@Lower, object@Upper)
    }
)
