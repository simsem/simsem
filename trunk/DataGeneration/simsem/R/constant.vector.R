constant.vector <-
function(constant, ni) {
	return(new("simVector", Data=rep(constant, ni), Labels=rep("", ni)))
}

