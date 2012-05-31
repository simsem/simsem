# Distribution Object

setClass("SimUnif", representation(min = "numeric", max = "numeric"))

setClass("SimNorm", representation(mean = "numeric", sd = "numeric"))

setClass("SimBeta", representation(shape1 = "numeric", shape2 = "numeric", ncp = "numeric"), prototype(ncp = 0))

setClass("SimBinom", representation(size = "numeric", prob = "numeric"))

setClass("SimCauchy", representation(location = "numeric", scale = "numeric"), prototype(location = 0, scale = 1))

setClass("SimChisq", representation(df = "numeric", ncp = "numeric"), prototype(ncp = 0))

setClass("SimExp", representation(rate = "numeric"), prototype(rate = 1))

setClass("SimF", representation(df1 = "numeric", df2 = "numeric", ncp = "numeric"), prototype(ncp = 0))

setClass("SimGamma", representation(shape = "numeric", rate = "numeric"), prototype(rate = 1))

setClass("SimGeom", representation(prob = "numeric"))

setClass("SimHyper", representation(m = "numeric", n = "numeric", k = "numeric"))

setClass("SimLnorm", representation(meanlog = "numeric", sdlog = "numeric"), prototype(meanlog = 0, sdlog = 1))

setClass("SimLogis", representation(location = "numeric", scale = "numeric"), prototype(location = 0, scale = 1))

setClass("SimNbinom", representation(size = "numeric", prob = "numeric"))

setClass("SimPois", representation(lambda = "numeric"))

setClass("SimT", representation(df = "numeric", ncp = "numeric"), prototype(ncp = 0))

setClass("SimWeibull", representation(shape = "numeric", scale = "numeric"), prototype(scale = 1))

setClassUnion("VirtualDist", c("SimUnif", "SimNorm", "SimBeta", "SimBinom", "SimCauchy", "SimChisq", "SimExp", "SimF", "SimGamma", "SimGeom", "SimHyper", "SimLnorm", "SimLogis", "SimNbinom", 
    "SimPois", "SimT", "SimWeibull"))

setClass("NullDataFrame", contains = "data.frame")

setClass("SimMatrix", representation(free = "matrix", popParam = "matrix", misspec = "matrix", prior = "matrix"),
         prototype(free = as.matrix(NaN), value = as.matrix(NaN) misspec = as.matrix(NaN), prior = as.matrix(NaN))

setClass("SymMatrix", contains = "SimMatrix")

setClass("SimVector", representation(free = "vector", value = "vector"), prototype(free = as.vector(NaN), value = as.vector(NaN)))

setClass("NullVector", contains = "vector")

setClass("NullMatrix", contains = "matrix")

setClass("NullSimMatrix", contains = "SimMatrix")

setClass("NullSymMatrix", contains = "SymMatrix")

setClass("NullSimVector", contains = "SimVector")

# RTH: #Delta on rows, epsilon on columns
setClass("SimSet", representation(modelType = "character", LY = "SimMatrix", TE = "SymMatrix", RTE = "SymMatrix", VTE = "SimVector", PS = "SymMatrix", RPS = "SymMatrix", VPS = "SimVector", 
    BE = "SimMatrix", TY = "SimVector", AL = "SimVector", ME = "SimVector", MY = "SimVector", VE = "SimVector", VY = "SimVector", LX = "SimMatrix", TD = "SymMatrix", RTD = "SymMatrix", VTD = "SimVector", 
    PH = "SymMatrix", RPH = "SymMatrix", VPH = "SimVector", GA = "SimMatrix", TX = "SimVector", KA = "SimVector", MX = "SimVector", VX = "SimVector", TH = "SimMatrix", RTH = "SimMatrix"), prototype(LY = new("NullSimMatrix"), 
    TE = new("NullSymMatrix"), RTE = new("NullSymMatrix"), VTE = new("NullSimVector"), PS = new("NullSymMatrix"), RPS = new("NullSymMatrix"), VPS = new("NullSimVector"), BE = new("NullSimMatrix"), 
    TY = new("NullSimVector"), AL = new("NullSimVector"), ME = new("NullSimVector"), MY = new("NullSimVector"), VE = new("NullSimVector"), VY = new("NullSimVector"), LX = new("NullSimMatrix"), TD = new("NullSymMatrix"), 
    RTD = new("NullSymMatrix"), VTD = new("NullSimVector"), PH = new("NullSymMatrix"), RPH = new("NullSymMatrix"), VPH = new("NullSimVector"), GA = new("NullSimMatrix"), TX = new("NullSimVector"), 
    KA = new("NullSimVector"), MX = new("NullSimVector"), VX = new("NullSimVector"), RTH = new("NullSimMatrix")))

setClass("NullSimSet", contains = "SimSet")

setClass("MatrixSet", representation(modelType = "character", LY = "matrix", RTE = "matrix", TE = "matrix", VTE = "vector", PS = "matrix", RPS = "matrix", VPS = "vector", BE = "matrix", 
    TY = "vector", AL = "vector", ME = "vector", MY = "vector", VE = "vector", VY = "vector", LX = "matrix", TD = "matrix", RTD = "matrix", VTD = "vector", PH = "matrix", RPH = "matrix", GA = "matrix", 
    TX = "vector", KA = "vector", MX = "vector", VPH = "vector", VX = "vector", TH = "matrix", RTH = "matrix"), prototype(LY = new("NullMatrix"), TE = new("NullMatrix"), RTE = new("NullMatrix"), 
    VTE = new("NullVector"), PS = new("NullMatrix"), RPS = new("NullMatrix"), VPS = new("NullVector"), BE = new("NullMatrix"), TY = new("NullVector"), AL = new("NullVector"), ME = new("NullVector"), 
    MY = new("NullVector"), VE = new("NullVector"), VY = new("NullVector"), LX = new("NullMatrix"), TD = new("NullMatrix"), RTD = new("NullMatrix"), VTD = new("NullVector"), PH = new("NullMatrix"), 
    RPH = new("NullMatrix"), GA = new("NullMatrix"), TX = new("NullVector"), KA = new("NullVector"), MX = new("NullVector"), VPH = new("NullVector"), VX = new("NullVector"), TH = new("NullMatrix"), 
    RTH = new("NullMatrix")))

setClass("VirtualRSet", representation(modelType = "character", LY = "matrix", TE = "matrix", PS = "matrix", BE = "matrix", TY = "vector", AL = "vector", LX = "matrix", TD = "matrix", 
    PH = "matrix", GA = "matrix", TX = "vector", KA = "vector", TH = "matrix"), prototype(LY = new("NullMatrix"), TE = new("NullMatrix"), PS = new("NullMatrix"), BE = new("NullMatrix"), TY = new("NullVector"), 
    AL = new("NullVector"), LX = new("NullMatrix"), TD = new("NullMatrix"), PH = new("NullMatrix"), GA = new("NullMatrix"), TX = new("NullVector"), KA = new("NullVector"), TH = new("NullMatrix")))

setClass("SimEqualCon", representation(con = "list", modelType = "character", conBeforeFill = "logical"), prototype(conBeforeFill = TRUE))

setClass("NullSimEqualCon", contains = "SimEqualCon", representation(con = "list", modelType = "character"), prototype(con = list(NA), modelType = "NA"))

setClass("SimREqualCon", representation(con = "list", modelType = "character"))

setClass("NullSimREqualCon", contains = "SimREqualCon", representation(con = "list", modelType = "character"), prototype(con = list(NA), modelType = "NA"))

setClass("SimParam", contains = "VirtualRSet")

setClass("SimLabels", contains = "VirtualRSet")

setClass("SimRSet", contains = "VirtualRSet")

setClass("NullRSet", contains = "SimRSet", representation(modelType = "character", LY = "matrix", TE = "matrix", PS = "matrix", BE = "matrix", TY = "vector", AL = "vector", LX = "matrix", 
    TD = "matrix", PH = "matrix", GA = "matrix", TX = "vector", KA = "vector", TH = "matrix"), prototype(modelType = character(0), LY = new("NullMatrix"), TE = new("NullMatrix"), PS = new("NullMatrix"), 
    BE = new("NullMatrix"), TY = new("NullVector"), AL = new("NullVector"), LX = new("NullMatrix"), TD = new("NullMatrix"), PH = new("NullMatrix"), GA = new("NullMatrix"), TX = new("NullVector"), 
    KA = new("NullVector"), TH = new("NullMatrix")))

setClass("SimMisspec", contains = "SimSet", representation(conBeforeMis = "logical", misBeforeFill = "logical", misfitType = "character", misfitBound = "vector", averageNumMisspec = "logical", 
    optMisfit = "character", numIter = "numeric"), prototype(conBeforeMis = TRUE, misBeforeFill = TRUE, misfitType = "rmsea", misfitBound = new("NullVector"), averageNumMisspec = FALSE, optMisfit = "none", 
    numIter = 20))

setClass("NullSimMisspec", contains = "SimMisspec")

setClass("MisspecSet", contains = "MatrixSet")

setClass("SimDataDist", representation(p = "numeric", dist = "list", keepScale = "logical", reverse = "vector"), prototype(keepScale = TRUE, reverse = FALSE))

setClass("NullSimDataDist", contains = "SimDataDist")

setClass("SimData", representation(modelType = "character", n = "numeric", param = "SimSet", misspec = "SimMisspec", equalCon = "SimEqualCon", maxDraw = "numeric", sequential = "logical", 
    facDist = "SimDataDist", errorDist = "SimDataDist", indDist = "SimDataDist", indLab = "vector", modelBoot = "logical", realData = "data.frame"), prototype(misspec = new("NullSimMisspec"), equalCon = new("NullSimEqualCon"), 
    maxDraw = 100, sequential = FALSE, facDist = new("NullSimDataDist"), errorDist = new("NullSimDataDist"), indDist = new("NullSimDataDist"), indLab = new("NullVector"), modelBoot = FALSE, realData = new("NullDataFrame")))

setClass("SimDataOut", representation(modelType = "character", data = "data.frame", param = "SimParam", paramOut = "SimRSet", misspecOut = "SimRSet", equalCon = "SimEqualCon", n = "numeric"), 
    prototype(equalCon = new("NullSimEqualCon"), n = 0))

setClass("SimModel", representation(modelType = "character", param = "SimParam", start = "SimRSet", equalCon = "SimEqualCon", package = "character", estimator = "character", auxiliary = "vector", 
    indLab = "vector", factorLab = "vector"), prototype(equalCon = new("NullSimEqualCon"), package = "lavaan", estimator = "ml", auxiliary = new("NullVector"), indLab = new("NullVector"), factorLab = new("NullVector")))

setClass("SimResult", representation(modelType = "character", nRep = "numeric", coef = "data.frame", se = "data.frame", fit = "data.frame", converged = "vector", paramValue = "data.frame", 
    FMI1 = "data.frame", FMI2 = "data.frame", stdCoef = "data.frame", seed = "numeric", n = "vector", pmMCAR = "vector", pmMAR = "vector"), prototype(stdCoef = new("NullDataFrame"), paramValue = new("NullDataFrame"), 
    FMI1 = new("NullDataFrame"), FMI2 = new("NullDataFrame"), n = 0, pmMCAR = 0, pmMAR = 0))

setClass("SimModelOut", representation(param = "SimParam", start = "SimRSet", equalCon = "SimEqualCon", package = "character", coef = "SimRSet", fit = "vector", se = "SimRSet", converged = "logical", 
    paramValue = "SimRSet", n = "numeric", indLab = "vector", factorLab = "vector"), prototype(equalCon = new("NullSimEqualCon"), converged = FALSE, paramValue = new("NullRSet"), n = 0, indLab = new("NullVector"), 
    factorLab = new("NullVector")))

setClass("SimModelMIOut", contains = "SimModelOut", representation(FMI1 = "SimRSet", FMI2 = "SimRSet"), prototype(FMI1 = new("NullRSet"), FMI2 = new("NullRSet")))

setClass("SimMissing", representation(cov = "vector", pmMCAR = "numeric", pmMAR = "numeric", nforms = "numeric", itemGroups = "list", twoMethod = "vector", prAttr = "vector", impMethod = "vector", 
    numImps = "numeric", timePoints = "numeric", ignoreCols = "numeric", threshold = "numeric", covAsAux = "logical", logical = "matrix"), prototype(cov = 0, pmMCAR = 0, pmMAR = 0, nforms = 0, itemGroups = list(0), 
    twoMethod = 0, prAttr = 0, impMethod = "amelia", numImps = 0, timePoints = 1, ignoreCols = 0, threshold = 0, covAsAux = TRUE, logical = new("NullMatrix")))

setClass("NullSimMissing", contains = "SimMissing")

setClass("SimFunction", representation(fun = "function", attribute = "list", callfun = "call"))

setClass("NullSimFunction", contains = "SimFunction")

setClass("SimDataOut", representation(modelType = "character", data = "data.frame", param = "SimParam", paramOut = "SimRSet", misspecOut = "SimRSet", equalCon = "SimEqualCon", n = "numeric"), 
    prototype(equalCon = new("NullSimEqualCon"), n = 0))

setClass("SimResultParam", representation(modelType = "character", nRep = "numeric", param = "data.frame", misspec = "data.frame", fit = "data.frame", seed = "numeric"), prototype(misspec = new("NullDataFrame"), 
    fit = new("NullDataFrame")))

setClass("SimGenLabels", contains = "MatrixSet") 
