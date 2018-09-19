#' @name mdsPlots2D
#' @aliases mdsPlots2D,cellexalvrR-method
#' @rdname mdsPlots2D-methods
#' @docType methods
#' @description create two 2D mds plots for the report
#' @param cellexalObj the cellexal object
#' @param gInfo the return value from cellexalvrR::groupingInfo()
#' @title description of function mdsPlot2D
#' @export 
setGeneric('mdsPlots2D', ## Name
	function ( cellexalObj, gInfo ) { ## Argumente der generischen Funktion
		standardGeneric('mdsPlots2D') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('mdsPlots2D', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gInfo ) {

		cellexalObj = sessionPath(cellexalObj)
		sessionPath= cellexalObj@usedObj$sessionPath
		
		print ( paste( cellexalObj@outpath, sessionPath))
	MDS1 = file.path( sessionPath , 'png', filename( c( cellexalObj@usedObj$lastGroup ,gInfo$mds , "1_2", 'png' ) ))
	gInfo$grouping[ which(is.na(gInfo$grouping))] = 0
	gInfo$grouping = gInfo$grouping +1
	if ( ! file.exists( MDS1 ) ){
		png( file= MDS1, width=1000, height=1000)
		plot(
				cellexalObj@mds[[gInfo$mds]][,1], cellexalObj@mds[[gInfo$mds]][,2], col= c('grey',gInfo$col)[ gInfo$grouping ],
				main = paste( gInfo$mds, 'dim 1+2' ), xlab="dimension 1", ylab= "dimension 2" )
		dev.off()
	}
	MDS2 = file.path( sessionPath , 'png', filename(c( cellexalObj@usedObj$lastGroup ,gInfo$mds, "2_3", 'png' ) ))
	if ( ! file.exists( MDS2 ) ){
		png( file= MDS2, width=1000, height=1000)
		plot(
				cellexalObj@mds[[gInfo$mds]][,2], cellexalObj@mds[[gInfo$mds]][,3],col= c('grey',gInfo$col)[ gInfo$grouping ],
				main = paste( gInfo$mds, 'dim 2+3' ), xlab="dimension 2", ylab= "dimension 3" )
		dev.off()
	}
	c( MDS1, MDS2)
} )
