
plotGOIsTimeLine <- function( cellexalObj, gInfo, GOIs=NULL, plotType='png' ) {
	if ( is.null( GOIs)) {
		return (c())
	}
	openPlot <- function(fname) {
		if ( plotType == 'pdf' ) {
			grDevices::pdf( file=paste(fname,'pdf', sep="."), width=10, height=10 )
		}else if (plotType == 'png_high_res' ){
			grDevices::png ( file=paste(fname,'highRes','png', sep="."), width=1600, height=1600)
		}else {
			grDevices::png ( file=paste(fname,'png', sep="."), width=800, height=800)
		}
	}

	loc <- reduceTo( cellexalObj, what='row', to= GOIs )
	gnameO =  paste(sep=" ",gInfo$gname , 'order')
	rolled <- FastWilcoxTest::rollSum( loc@data[, as.vector(loc@userGroups[, gnameO ] ) ], 10 )
	rownames(rolled) = rownames(loc@data)

	col= gplots::bluered( ncol(loc@data) )

	for ( gene in rownames(loc@data)) {
		fname = cellexalObj@usedObj$sessionPath
		if ( is.null(fname) ) {
			fname = cellexalObj@outpath
		}
		fname  = file.path( fname, paste( sep=".", gInfo$gname,gene ) )
		openPlot ( fname )
		plot( 1, 0, xlim=c(min(1), ncol(rolled)), ylim=c(0,1), xlab='pseudotime', ylab='smoothed normalized expression', col='white', main=gene)
		Y = rolled[gene,]
		Y = Y - min(Y)
		Y = Y / max(Y)
		lines( X, Y, lwd=1, col= 'black' )
		lapply( names(table( col)) , function(n) {
			if ( n != 'black'){
				lines( X[which(col == n)], Y[which(col==n)], lwd=1, col= n )
			}
		})
		dev.off()

	}

}