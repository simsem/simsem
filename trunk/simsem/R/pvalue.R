pvalue<-function(x,vector, revDirec =FALSE){
	if(revDirec) {
		return(mean(x<=vector, na.rm = TRUE))
	} else {
		return(mean(x>=vector, na.rm = TRUE))
	}
}
