#' drcPlots2Dtime is a function linked to the log functionallity.
#' 
#' This function will create the 2D DRC images for the log sections.
#' 
#' @name drcPlots2Dtime
#' @aliases drcPlots2Dtime,cellexalvrR-method
#' @rdname drcPlots2Dtime-methods
#' @docType methods
#' @description create two 2D drc plots for the report
#' @param cellexalObj the cellexal object
#' @param gInfo the return value from cellexalvrR::groupingInfo()
#' @title description of function drcPlot2D
#' @export 
setGeneric('drcPlots2Dtime', ## Name
	function ( cellexalObj, gInfo ) { 
		standardGeneric('drcPlots2Dtime')
	}
)

setMethod('drcPlots2Dtime', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gInfo ) {

		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
		sessionPath= cellexalObj@usedObj$sessionPath
		
		print ( paste( cellexalObj@outpath, sessionPath))
		if ( ! file.exists(file.path( sessionPath , 'png') )){
			dir.create(file.path( sessionPath , 'png')  )
		}
		if ( ! gInfo$drc %in% names(cellexalObj@drc) ){
			stop( paste("group info does not match to cellexalObj data content: drc named", gInfo$drc, "not in list", paste( collapse=", ", names(cellexalObj@drc))))
		}
		
		drc = cellexalObj@drc[[gInfo$drc]]
		gInfo$order[ which(is.na(gInfo$order))] = 0
		if ( any( ! is.numeric(gInfo$order)) ) {
			message("wrong data in gInfo$order")
			browser()
		}
		if ( min(as.vector(gInfo$order)) == 0) {
			gInfo$order = as.numeric(as.vector(gInfo$order)) +1
		}
		gInfo$order = as.vector(gInfo$order)
		#browser()
		#if ( ncol( cellexalObj@data) > 200) { browser()}

		DRC1 = file.path( sessionPath , 'png', filename( c( gInfo$gname ,gInfo$drc , "1_2", 'png' ) )) #function definition in file 'filename.R'
		grDevices::png( file= DRC1, width=1000, height=1000)
		graphics::plot(	drc[,1], drc[,2], col= color(gInfo$time,colnames(cellexalObj@data)), pch=16,
			main = paste( gInfo$drc, 'dim 1+2' ), xlab="dimension 1", ylab= "dimension 2" )
		dev.off()


		timeline = cellexalObj@usedObj$timelines[[paste(gInfo$gname, 'timeline' )]]
		#rgl::plot3d( timeline$x[timeline$time], timeline$y[timeline$time], timeline$z[timeline$time], col=gplots::bluered( length( timeline$x)))
		#rgl::plot3d( timeline$a[timeline$time], timeline$b[timeline$time], timeline$c[timeline$time], col=gplots::bluered( length( timeline$x)) )
		
		DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo$gname ,gInfo$drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
		grDevices::png( file= DRC2, width=1000, height=1000)
		graphics::plot(
				drc[,1], drc[,3], col= color(gInfo$time,colnames(cellexalObj@data)), pch=16,
			main = paste( gInfo$drc, 'dim 1+3' ), xlab="dimension 1", ylab= "dimension 3" )
		dev.off()
		
		c( DRC1, DRC2)
} )
