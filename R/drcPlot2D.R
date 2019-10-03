#' drcPlots2D is a function linked to the log functionallity.
#' 
#' This function will create the 2D DRC images for the log sections.
#' 
#' @name drcPlots2D
#' @aliases drcPlots2D,cellexalvrR-method
#' @rdname drcPlots2D-methods
#' @docType methods
#' @description create two 2D drc plots for the report
#' @param cellexalObj the cellexal object
#' @param gInfo the return value from cellexalvrR::groupingInfo()
#' @title description of function drcPlot2D
#' @export 
setGeneric('drcPlots2D', ## Name
	function ( cellexalObj, gInfo ) { 
		standardGeneric('drcPlots2D')
	}
)

setMethod('drcPlots2D', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gInfo ) {

		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
		sessionPath= cellexalObj@usedObj$sessionPath
		
		print ( paste( cellexalObj@outpath, sessionPath))
	DRC1 = file.path( sessionPath , 'png', filename( c( cellexalObj@usedObj$lastGroup ,gInfo$drc , "1_2", 'png' ) )) #function definition in file 'filename.R'
	gInfo$grouping[ which(is.na(gInfo$grouping))] = 0
	gInfo$grouping = gInfo$grouping +1
	if ( ! gInfo$drc %in% names(cellexalObj@drc) ){
		stop( paste("group info does not match to cellexalObj data content: drc named", gInfo$drc, "not in list", paste( collapse=", ", names(cellexalObj@drc))))
	}
	if ( ! file.exists( DRC1 ) ){
		grDevices::png( file= DRC1, width=1000, height=1000)
		graphics::plot(
				cellexalObj@drc[[gInfo$drc]][,1], cellexalObj@drc[[gInfo$drc]][,2], col= c('grey',gInfo$col)[ gInfo$grouping ],
				main = paste( gInfo$drc, 'dim 1+2' ), xlab="dimension 1", ylab= "dimension 2" )
		grDevices::dev.off()
	}
	DRC1 = file.path('png', filename( c( cellexalObj@usedObj$lastGroup ,gInfo$drc , "1_2", 'png' ) )) #function definition in file 'filename.R'
	
	DRC2 = file.path( sessionPath , 'png', filename(c( cellexalObj@usedObj$lastGroup ,gInfo$drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
	if ( ! file.exists( DRC2 ) ){
		grDevices::png( file= DRC2, width=1000, height=1000)
		graphics::plot(
				cellexalObj@drc[[gInfo$drc]][,2], cellexalObj@drc[[gInfo$drc]][,3],col= c('grey',gInfo$col)[ gInfo$grouping ],
				main = paste( gInfo$drc, 'dim 2+3' ), xlab="dimension 2", ylab= "dimension 3" )
		grDevices::dev.off()
	}
	DRC2 = file.path( 'png', filename(c( cellexalObj@usedObj$lastGroup ,gInfo$drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
	c( DRC1, DRC2)
} )
