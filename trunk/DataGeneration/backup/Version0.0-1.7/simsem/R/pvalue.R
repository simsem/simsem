pvalue<-function(x,vector, reverse =FALSE){
	if(reverse) {
		return(mean(x<=vector, na.rm = TRUE))
	} else {
		return(mean(x>=vector, na.rm = TRUE))
	}
}
