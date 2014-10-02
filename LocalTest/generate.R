test.changeScaleSEM <- function() {
	loading <- matrix(0, 8, 3)
	loading[1:3, 1] <- NA
	loading[4:6, 2] <- NA
	loading[7:8, 3] <- "con1"
	loading.start <- matrix("", 8, 3)
	loading.start[1:3, 1] <- 0.7
	loading.start[4:6, 2] <- 0.7
	loading.start[7:8, 3] <- "rnorm(1,0.6,0.05)"
	LY <- bind(loading, loading.start)

	RTE <- binds(diag(8))

	factor.cor <- diag(3)
	factor.cor[1, 2] <- factor.cor[2, 1] <- NA
	RPS <- binds(factor.cor, 0.5)

	path <- matrix(0, 3, 3)
	path[3, 1:2] <- NA
	path.start <- matrix(0, 3, 3)
	path.start[3, 1] <- "rnorm(1,0.6,0.05)"
	path.start[3, 2] <- "runif(1,0.3,0.5)"
	BE <- bind(path, path.start)

	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)

	# Manifest variable approach



	loading <- matrix(0, 8, 3)
	loading[1:3, 1] <- c(1, "con1", "con1")
	loading[4:6, 2] <- c(1, "con2", "con2")
	loading[7:8, 3] <- c(1, 1)
	loading.start <- matrix("", 8, 3)
	LY <- bind(loading, 0.9)

	RTE <- binds(diag(8))

	factor.cor <- diag(3)
	factor.cor[1, 2] <- factor.cor[2, 1] <- NA
	RPS <- binds(factor.cor, 0.5)

	VPS <- bind(rep(NA, 3), c(1.3, 1.6, 0.9))

	path <- matrix(0, 3, 3)
	path[3, 1:2] <- NA
	path.start <- matrix(0, 3, 3)
	path.start[3, 1] <- "rnorm(1,0.6,0.05)"
	path.start[3, 2] <- "runif(1,0.3,0.5)"
	BE <- bind(path, path.start)

	VTE <- bind(rep(NA, 8), 1)
	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, VPS=VPS, VTE=VTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)

	# Mixed approach

	loading <- matrix(0, 8, 3)
	loading[1:3, 1] <- c(1, "con1", "con1")
	loading[4:6, 2] <- c(1, "con2", "con2")
	loading[7:8, 3] <- c("con3", "con3")
	loading.start <- matrix("", 8, 3)
	LY <- bind(loading, 0.7)

	RTE <- binds(diag(8))

	factor.cor <- diag(3)
	factor.cor[1, 2] <- factor.cor[2, 1] <- NA
	RPS <- binds(factor.cor, 0.5)

	VPS <- bind(c(NA, NA, 0.9), c(1.3, 0.7, ""))

	path <- matrix(0, 3, 3)
	path[3, 1:2] <- NA
	path.start <- matrix(0, 3, 3)
	path.start[3, 1] <- "rnorm(1,0.6,0.05)"
	path.start[3, 2] <- "runif(1,0.3,0.5)"
	BE <- bind(path, path.start)

	VTE <- bind(rep(NA, 8), 1)
	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, VPS=VPS, VTE=VTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)
	
	
	# Constrain


	loading <- matrix(0, 9, 3)
	loading[1:3, 1] <- c("con1", "con2", "con3")
	loading[4:6, 2] <- c("con1", "con2", "con3")
	loading[7:9, 3] <- c("con1", "con2", "con3")
	LY <- bind(loading, 0.7)

	RTE <- binds(diag(9))

	RPS <- binds(diag(3))

	VPS <- bind(c(1, NA, NA), c("", 0.75, 0.75))

	path <- matrix(0, 3, 3)
	path[2, 1] <- NA
	path[3, 2] <- NA
	BE <- bind(path, 0.5)

	VTE <- bind(rep(NA, 9), 1)
	SEM.model <- model(BE=BE, LY=LY, RPS=RPS, RTE=RTE, VPS=VPS, VTE=VTE, modelType="SEM")

	dat <- generate(SEM.model, n=300)

	# Constrain Group

	loading.in <- matrix(0, 9, 3)
	loading.in[1:3, 1] <- paste0("load", 1:3)
	loading.in[4:6, 2] <- paste0("load", 4:6)
	loading.in[7:9, 3] <- paste0("load", 7:9)
	LY.in <- bind(loading.in, 0.7)

	RPS <- binds(diag(3))

	RTE <- binds(diag(9))

	VTE <- bind(rep(NA, 9), 0.51)

	TY.in <- bind(paste0("int", 1:9), 0)

	VPS1 <- bind(rep(1, 3))
	VPS2 <- bind(rep(NA, 3), c(1.1, 1.2, 1.3))

	AL1 <- bind(rep(0, 3))
	AL2 <- bind(rep(NA, 3), c(-0.5, 0.2, 0.3))

	path <- matrix(0, 3, 3)
	path[2, 1] <- NA
	path[3, 2] <- NA
	BE <- bind(path, 0.5)

	strong <- model(LY = LY.in, RPS = RPS, VPS=list(VPS1, VPS2), RTE = RTE, VTE=VTE, TY=TY.in, AL=list(AL1, AL2), BE=list(BE,BE), ngroups=2, modelType = "SEM")

	dat <- generate(strong,200)
}
