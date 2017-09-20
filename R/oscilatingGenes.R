#' Identifies oscilating genes and tries to group them in oscilating clusters using the EMD package.
#'@param cellexalObj A cellexalvr object
#'@param cellidfile file containing cell IDs
#'@keywords heatmap
#' @return Undefined at the moment
#'@export make.cellexalvr.heatmap

oscilatingGenes <- function( cellexalObj, cellidfile ) {
	if ( class(cellexalObj) == 'character' ) {
		if ( file.exists( cellexalObj ) ) {
			load(cellexalObj)
		}
	}
	if ( file.exists(cellidfile)) {
		cellexalObj <- userGrouping(cellexalObj, cellidfile)
	}
	
	loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj@data)[-
							which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
			] )
	loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order'))
	
	info <- groupingInfo( loc )
	
	xt2 <- apply(loc@data, 2, sum)
	tt2 <- info$order
	try <- emd(xt2, tt2, boundary="wave")
	for(i in 1:try$nimf) {
		#try$imf[,i] is the waveform that has been identified 
		png( file= paste("wave_",i,'.png', sep=''), width=1000, height=300 )
		plot(tt2, try$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
				main=paste(i, "-th IMF", sep="")); 
		abline(h=0)
		dev.off()
		## now I want to find the genes, that follow this wave
		
	}
		
	browser()
	
}