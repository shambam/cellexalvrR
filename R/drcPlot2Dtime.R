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
		
		#print ( paste( cellexalObj@outpath, sessionPath))
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
		}
		if ( min(as.vector(gInfo$order)) == 0) {
			gInfo$order = as.numeric(as.vector(gInfo$order)) +1
		}
		gInfo$order = as.vector(gInfo$order)
		#browser()
		#if ( ncol( cellexalObj@data) > 200) { browser()}

		DRC1 = file.path( sessionPath , 'png', filename( c( gInfo$gname ,gInfo$drc , "1_2", 'png' ) )) #function definition in file 'filename.R'
		grDevices::png( file= DRC1, width=1000, height=1000)
		#browser()
		timeline = cellexalObj@usedObj$timelines[[paste(gInfo$gname, 'timeline' )]]
		if ( is.null( timeline) ) {
			## if it is already a Time.group we look at we are fine!
			timeline = cellexalObj@usedObj$timelines[[gInfo$gname]]
		}
		if ( is.null(timeline) ) {
			if (cellexalObj@usedObj$timelines[["lastEntry"]]@gname == gInfo$gname ){
				timeline = cellexalObj@usedObj$timelines[["lastEntry"]]
			}
			else {
				return( drcPlots2D( cellexalObj, gInfo ) )
			}
		}
		id = as.numeric(factor(color(timeline,rownames(drc))))
		col = color(timeline, rownames(drc))

   		p= prettyPlot2Dtime( data.frame(id = id, x=	drc[,1], y=	drc[,2], col = col) ) #function definition in file drcPlot2D.R
    	print(p) #write the plot
		dev.off()


		
		#rgl::plot3d( timeline$x[timeline$time], timeline$y[timeline$time], timeline$z[timeline$time], col=gplots::bluered( length( timeline$x)))
		#rgl::plot3d( timeline$a[timeline$time], timeline$b[timeline$time], timeline$c[timeline$time], col=gplots::bluered( length( timeline$x)) )
		DRC2 = NULL
		if ( ! var(cellexalObj@drc[[gInfo$drc]][,3]) == 0 ) {
			DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo$gname ,gInfo$drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
			grDevices::png( file= DRC2, width=1000, height=1000)
			p= prettyPlot2Dtime( data.frame(id = id, x=	drc[,1], y=	drc[,3],col= col) ) #function definition in file drcPlot2D.R
    		print(p) #write the plot
			dev.off()
		}
		
		c( DRC1, DRC2)
} )


prettyPlot2Dtime = function(x ){

	x$id = as.vector(x$id)
	x[,'x'] = as.numeric(x[,'x'])
	x[,'y'] = as.numeric(x[,'y'])

	
	p = ggplot2::ggplot(x, ggplot2::aes(x=x, y=y) ) 
	p = p +   ggplot2::geom_point(color = x$col , show.legend = FALSE)
    p
}  



drcFiles2HTMLtime = function( cellexalObj, gInfo, addOn = NULL ) {
	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles
	drcFiles =sapply( drcPlots2Dtime( cellexalObj, gInfo ), correctPath, cellexalObj )
	str = c(
		paste( "### 2D DRC", gInfo$drc, "dim 1,2","(", gInfo$gname,")", addOn),"\n",
		paste("![](",drcFiles[1],")"),
		'',"")
	if ( ! is.na(drcFiles[2]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo$drc, "dim 2,3","(", gInfo$gname,")", addOn),"\n",
		paste("![](",drcFiles[2],")"),
		"","")
	}
	paste( str, collapse="\n", sep="" )
}