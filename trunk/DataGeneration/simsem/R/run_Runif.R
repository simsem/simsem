setMethod("run",
    signature(object = "Runif"),
    function (object) 
    {
        runif(1,object@Lower, object@Upper)
    }
)
