testObject <- function(object) {
   exists(as.character(substitute(object)))
}

useTemp <- function(filename) {
	paste0(tempdir(), "/", filename)
}

checkFigure <- function(filename) {
	if (file.exists(filename)) {
		file.remove(filename)
		return(TRUE)
	} else {
		return(FALSE)
	}
}

checkExample <- function(path, label, subFrom = NULL, subTo = NULL, result = NULL) {
	png(useTemp("figure.png"))
	txt <- readLines(path)
	for(i in seq_along(subFrom)) {
		txt <- gsub(subFrom[i], subTo[i], txt)
	}
	error <- try(time <- system.time(capture.output(eval(parse(text = txt)))), silent=TRUE)
	dev.off()
	figure <- checkFigure(useTemp("figure.png"))
	if(is(error, "try-error")) {
		result <- rbind(result, c(label, FALSE, FALSE, NA))
	} else {
		result <- rbind(result, c(label, figure, NA, time[3]))
	}
	return(result)
}

globalTest <- function() {
	# Test
	# 1. Successful run
	# 2. Have desired characteristics
	# 3. Time elapsed
	condition <- NULL
	success <- NULL
	goodrun <- NULL
	timeelapsed <- NULL
	
	### Bind function
	test.bind()
	
	### Model function
	# test.model()
	# test.estmodel()
	# test.model.lavaan()
	# test.covData()
	
	# CFA, SEM, path with fillParam/Not
	# Equality constraints
	# Inequality constraints
	# Multiple groups with three models
	# Covariate
	# Data distribution Normal copula and Hu-bentler
	
	### draw function
	# test.draw()
	# test.rawDraw()
	# test.fillParam()
	# test.createImpliedMACS()
	# test.createImpliedConditionalMACS()
	# test.popMisfitMACS()
	# test.popDiscrepancy()
	
	### createData function
	# test.createData()
	
	### generate function
	# test.generate()
	
	### sim function
	# test.sim()
	# outfun
	# datafun
	
	### Utilities function from result object
	
	
	
	


}


test.overall <- function() {
	# Test
	# 1. Name of testing
	# 2. Successful run?
	# 3. Have expected errors?
	# 4. Time elapsed
	install.packages("simsem_0.5-0.tar.gz", repos=NULL, type="source")
	result <- NULL
	currentDir <- getwd()
	currentDir <- gsub("/simsem/R", "", currentDir)
	
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex1/ex1.R"), "Example 1", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex2/ex2.R"), "Example 2", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex3/ex3.R"), "Example 3", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex4/ex4.R"), "Example 4", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex5/ex5.R"), "Example 5", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex6/ex6.R"), "Example 6", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex7/ex7.R"), "Example 7", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex8/ex8.R"), "Example 8", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex9/ex9.R"), "Example 9", subFrom = "sim\\(1000", subTo = "sim\\(5", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex10/ex10.R"), "Example 10", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex11/ex11.R"), "Example 11", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex12/ex12.R"), "Example 12", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex13/ex13.R"), "Example 13", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex14/ex14.R"), "Example 14", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex15/ex15.R"), "Example 15", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex16/ex16.R"), "Example 16", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex17/ex17.R"), "Example 17", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex18/ex18.R"), "Example 18", subFrom = "50:1000", subTo = "seq\\(50, 1000, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex19/ex19.R"), "Example 19", subFrom = "50:500", subTo = "seq\\(50, 500, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex20/ex20.R"), "Example 20", subFrom = "25:500", subTo = "seq\\(25, 500, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex21/ex21.R"), "Example 21", subFrom = "25:500", subTo = "seq\\(25, 500, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex22/ex22.R"), "Example 22", subFrom = "25:500", subTo = "seq\\(25, 500, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex23/ex23.R"), "Example 23", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex24/ex24.R"), "Example 24", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex25/ex25.R"), "Example 25", subFrom = "50:500", subTo = "seq\\(50, 1000, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex26/ex26.R"), "Example 26", subFrom = "50:500", subTo = "seq\\(50, 1000, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex27/ex27.R"), "Example 27", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version03/ex28/ex28.R"), "Example 28", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	
	# New update 0.5
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/exMultipleFormat/singleGroup.R"), "Multiple format sim (Single Group)", subFrom = "totalRep <- 1000", subTo = "totalRep <- 50", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/exMultipleFormat/multipleGroup.R"), "Multiple format sim (Multiple Group)", subFrom = "totalRep <- 1000", subTo = "totalRep <- 50", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/exMultipleFormat/covData.R"), "Covariate effect", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex2/ex2.R"), "Example 2 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex5/ex5.R"), "Example 5 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex6/ex6.R"), "Example 6 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex7/ex7.R"), "Example 7 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex8/ex8.R"), "Example 8 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex9/ex9.R"), "Example 9 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(10", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex10/ex10.R"), "Example 10 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex11/ex11.R"), "Example 11 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex12/ex12.R"), "Example 12 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex14/ex14.R"), "Example 14 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex15/ex15.R"), "Example 15 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex16/ex16.R"), "Example 16 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex17/ex17.R"), "Example 17 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex18/ex18.R"), "Example 18 (lavaan)", subFrom = "50:1000", subTo = "seq\\(50, 1000, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex19/ex19.R"), "Example 19 (lavaan)", subFrom = "50:500", subTo = "seq\\(50, 500, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex21/ex21.R"), "Example 21 (lavaan)", subFrom = "25:500", subTo = "seq\\(25, 500, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex22/ex22.R"), "Example 22 (lavaan)", subFrom = "25:500", subTo = "seq\\(25, 500, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex24/ex24.R"), "Example 24 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex25/ex25.R"), "Example 25 (lavaan)", subFrom = "50:500", subTo = "seq\\(50, 1000, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex26/ex26.R"), "Example 26 (lavaan)", subFrom = "50:500", subTo = "seq\\(50, 1000, 10\\)", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex27/ex27.R"), "Example 27 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)
	result <- checkExample(paste0(currentDir, "/SupportingDocs/Examples/Version05/ex28/ex28.R"), "Example 28 (lavaan)", subFrom = "sim\\(1000", subTo = "sim\\(20", result = result)

	
	colnames(result) <- c("condition", "success", "expecterror", "time")
	return(result)
}

