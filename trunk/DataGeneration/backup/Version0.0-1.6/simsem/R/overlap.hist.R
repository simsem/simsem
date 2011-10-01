overlap.hist <- function(a, b, colors=c("red","blue","purple"),
                                breaks=NULL, xlim=NULL, ylim=NULL, main=NULL, xlab=NULL, percentile = NULL,
								find.power = FALSE){

	if(!is.matrix(a)) a <- a@Output
	if(!is.matrix(b)) b <- b@Output
	
	ahist=NULL
  bhist=NULL

  if(!(is.null(breaks))){
    ahist=hist(a,breaks=breaks,plot=F)
    bhist=hist(b,breaks=breaks,plot=F)
  } else {
    ahist=hist(a,plot=F)
    bhist=hist(b,plot=F)

    dist = ahist$breaks[2]-ahist$breaks[1]
    breaks = seq(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks),dist)

    ahist=hist(a,breaks=breaks,plot=F)
    bhist=hist(b,breaks=breaks,plot=F)
  }

  if(is.null(xlim)){
    xlim = c(min(ahist$breaks,bhist$breaks),max(ahist$breaks,bhist$breaks))
  }

  if(is.null(ylim)){
    ylim = c(0,max(ahist$counts,bhist$counts))
  }

  overlap = ahist
  for(i in 1:length(overlap$counts)){
    if(ahist$counts[i] > 0 & bhist$counts[i] > 0){
      overlap$counts[i] = min(ahist$counts[i],bhist$counts[i])
    } else {
      overlap$counts[i] = 0
    }
  }

  plot(ahist, xlim=xlim, ylim=ylim, col=colors[1], main=main, xlab=xlab)
  plot(bhist, xlim=xlim, ylim=ylim, col=colors[2], add=T)
  plot(overlap, xlim=xlim, ylim=ylim, col=colors[3], add=T)
  if(!is.null(percentile)) {
	cutoff1 <- quantile(a, percentile, na.rm = TRUE)
	cutoff2 <- quantile(b, 1 - percentile, na.rm = TRUE)
	abline(v = cutoff1)
	if(find.power == FALSE) abline(v = cutoff2)
  }
}
	