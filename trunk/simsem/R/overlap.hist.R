# overlap.hist
# Function -- simsem package
# Plot overlapping histograms
# Function: overlap.hist(a, b, colors=c("red","blue","purple"), breaks=NULL, xlim=NULL, ylim=NULL, main=NULL, xlab=NULL, swap=FALSE)
# Argument:
#	a:	Data for the first histogram
#	b:	Data for the second histogram
#	colors:	Colors for the first histogram, the second histogram, and the overlappling areas.
#	breaks:	How many breaks users used in each histogram (should not be used)
#	xlim:	The range of x-axis
#	ylim:	The range of y-axis
#	main: 	The title of the figure
#	xiab:	The labels of x-axis
#	swap: 	Specify TRUE to change to plot b first and then a. The default is FALSE to plot a first and then b.
# Return: 	NONE. This function will plot only.
# Author: Sunthud Pornprasertmanit (University of Kansas; psunthud@ku.edu)
# Date Modified: October 9, 2011

overlap.hist <- function(a, b, colors=c("red","blue","purple"), breaks=NULL, xlim=NULL, ylim=NULL, main=NULL, xlab=NULL, swap=FALSE){
	ahist=NULL
	bhist=NULL
	if(!(is.null(breaks))){
		ahist=hist(a,breaks=breaks,plot=F)
		bhist=hist(b,breaks=breaks,plot=F)
	} else {
		ahist=hist(a,plot=F)
		bhist=hist(b,plot=F)
		dist = ahist$breaks[2]-ahist$breaks[1]
		mina <- min(ahist$breaks,bhist$breaks)
		maxa <- max(ahist$breaks,bhist$breaks)
		bin <- ceiling((maxa - mina)/dist)
		breaks = seq(mina, maxa,length.out = bin)
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
	if(swap) {
		plot(bhist, xlim=xlim, ylim=ylim, col=colors[2], main=main, xlab=xlab)  
		plot(ahist, xlim=xlim, ylim=ylim, col=colors[1], add=T)
	} else {
		plot(ahist, xlim=xlim, ylim=ylim, col=colors[1], main=main, xlab=xlab)
		plot(bhist, xlim=xlim, ylim=ylim, col=colors[2], add=T)
	}
	plot(overlap, xlim=xlim, ylim=ylim, col=colors[3], add=T)
}
#Examples:
#a <- rnorm(10000, 0, 1)
#b <- rnorm(10000, 1, 1.5)
#overlap.hist(a, b, main="Example")
