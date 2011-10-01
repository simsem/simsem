setMethod("run",
    signature(object = "nullSimMatrix"),
    function (object) 
    {
		return(new("nullMatrix"))
    }
)
