setMethod("find.OpenMx.values", signature(Parameters="vector", Starting.Values="vector"), definition=function(Parameters, Starting.Values) {
	if(is.null.object(Parameters)) {
		return(Starting.Values)
	} else {
		Starting.Values[!is.na(Parameters)] <- Parameters[!is.na(Parameters)]
		return(round(Starting.Values, 3))
	}
})

setMethod("find.OpenMx.values", signature(Parameters="matrix", Starting.Values="matrix"), definition=function(Parameters, Starting.Values) {
	if(is.null.object(Parameters)) {
		return(Starting.Values)
	} else {
		Starting.Values[!is.na(Parameters)] <- Parameters[!is.na(Parameters)]
		return(round(Starting.Values, 3))
	}
})

setMethod("find.OpenMx.values", signature(Parameters="freeParamSet", Starting.Values="reducedMatrixSet"), definition=function(Parameters, Starting.Values) {
	Starting.Values@LY <- find.OpenMx.values(Parameters@LY, Starting.Values@LY)
	Starting.Values@TE <- find.OpenMx.values(Parameters@TE, Starting.Values@TE)
	Starting.Values@PS <- find.OpenMx.values(Parameters@PS, Starting.Values@PS)
	Starting.Values@BE <- find.OpenMx.values(Parameters@BE, Starting.Values@BE)
	Starting.Values@TY <- find.OpenMx.values(Parameters@TY, Starting.Values@TY)
	Starting.Values@AL <- find.OpenMx.values(Parameters@AL, Starting.Values@AL)
	Starting.Values@LX <- find.OpenMx.values(Parameters@LX, Starting.Values@LX)
	Starting.Values@TD <- find.OpenMx.values(Parameters@TD, Starting.Values@TD)
	Starting.Values@PH <- find.OpenMx.values(Parameters@PH, Starting.Values@PH)
	Starting.Values@GA <- find.OpenMx.values(Parameters@GA, Starting.Values@GA)
	Starting.Values@TX <- find.OpenMx.values(Parameters@TX, Starting.Values@TX)
	Starting.Values@KA <- find.OpenMx.values(Parameters@KA, Starting.Values@KA)
	Starting.Values@TH <- find.OpenMx.values(Parameters@TH, Starting.Values@TH)
	return(Starting.Values)
})
