# The dataGen Class

setClass("dataGen",
         representation(
                        pop.model="matrix",
                        analysis.model="matrix",
                        data="matrix",
                        missing.data="matrix",
                        impute.data="list"),
         sealed=TRUE)
