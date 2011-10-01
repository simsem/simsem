setClass("Runif",
	representation(
		Lower="numeric",
		Upper="numeric"
	)
)

setClass("Rnorm",
	representation(
		Mean="numeric",
		SD="numeric"
	)
)

setClassUnion("simDist", c("Runif", "Rnorm"))
