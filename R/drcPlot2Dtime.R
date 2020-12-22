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
		#browser()
		timeline = cellexalObj@usedObj$timelines[[paste(gInfo$gname, 'timeline' )]]
		if ( is.null( timeline) ) {
			## if it is already a Time.group we look at we are fine!
			timeline = cellexalObj@usedObj$timelines[[gInfo$gname]]
		}

		id = as.numeric(factor(color(timeline,colnames(cellexalObj@data))))
		col = color(timeline,colnames(cellexalObj@data))
   		p= prettyPlot2Dtime( data.frame(id = id, x=	drc[,1], y=	drc[,2]), col) #function definition in file drcPlot2D.R
    	print(p)
		dev.off()


		
		#rgl::plot3d( timeline$x[timeline$time], timeline$y[timeline$time], timeline$z[timeline$time], col=gplots::bluered( length( timeline$x)))
		#rgl::plot3d( timeline$a[timeline$time], timeline$b[timeline$time], timeline$c[timeline$time], col=gplots::bluered( length( timeline$x)) )
		DRC2 = NULL
		if ( ! var(cellexalObj@drc[[gInfo$drc]][,3]) == 0 ) {
			DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo$gname ,gInfo$drc, "2_3", 'png' ) )) #function definition in file 'filename.R'
			grDevices::png( file= DRC2, width=1000, height=1000)
			p= prettyPlot2Dtime( data.frame(id = id, x=	drc[,1], y=	drc[,3]), col ) #function definition in file drcPlot2D.R
    		print(p)
			dev.off()
		}
		
		c( DRC1, DRC2)
} )


prettyPlot2Dtime = function(x, col ){

	x$id = as.vector(x$id)
	x[,1] = as.numeric(x[,1])
	x[,2] = as.numeric(x[,2])
	x$col=  factor(col)
	col=unique(x$col)

	
	p = ggplot2::ggplot(x, ggplot2::aes(x=x, y=y) ) 
	p = p +   ggplot2::geom_point(color = x$col , show.legend = FALSE)
	#browser()
	# pos= t(sapply( sort(as.numeric(unique(x$id))), function(id) {
	# 	ok = which(x$id == id); 
	# 	c( median(x[ok,1]), median(x[ok,2]) )
	# } ))
 #    theta <- seq(pi/8, 2*pi, length.out=48)
 #    xo <- diff(range(pos[,1]))/1200
 #    yo <- diff(range(pos[,2]))/1200
 #    C1 = c(grey(.6),col)
 #    C2 = 'black'
 #    for(i in theta) {
 #        p <- p + ggplot2::geom_text( data=data.frame(pos),
 #            ggplot2::aes_q(
 #                x = bquote(pos[,1]+.(cos(i)*xo)),
 #                y = bquote(pos[,2]+.(sin(i)*yo)),
 #                label=sort(as.numeric(unique(x$id)))-1), 
 #                    size=10, colour=C2 )
 #    }
 #    browser()
 #    if ( length(C1) -1 == length(unique(x$id))){
 #    	C1 = C1[-1]
 #    }
 #    p = p + ggplot2::annotate('text', x = pos[,1], y = pos[,2],
 #     label = sort(as.numeric(unique(x$id))) -1, size = 10, col=C1 )  
    p
}  



drcFiles2HTMLtime = function( cellexalObj, gInfo, addOn = NULL ) {
	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles
	drcFiles =sapply( drcPlots2Dtime( cellexalObj, gInfo ), correctPath, cellexalObj )
	str = c(
		paste( "### 2D DRC", gInfo$drc, " dim 1,2", addOn),
		paste("![](",drcFiles[1],")"),
		'')
	if ( ! is.null(drcFiles[2]) ){
		str = c( str, 
		paste( "### 2D DRC", gInfo$drc, " dim 2,3", addOn),
		paste("![](",drcFiles[2],")"),
		"")
	}
	str
}