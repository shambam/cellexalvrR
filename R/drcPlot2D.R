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
	function ( cellexalObj, gInfo, GOIs=NULL ) { 
		standardGeneric('drcPlots2D')
	}
)

setMethod('drcPlots2D', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gInfo, GOIs=NULL ) {

		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
		sessionPath= cellexalObj@usedObj$sessionPath
		
		#print ( paste( cellexalObj@outpath, sessionPath))
		if ( ! file.exists(file.path( sessionPath , 'png') )){
			dir.create(file.path( sessionPath , 'png')  )
		}
		# if ( gInfo$gname == 'Time.group.3') {
		# 	browser()
		# }
	DRC1 = file.path( sessionPath , 'png', filename( c( gInfo$gname ,gInfo$drc , "1_2", 'png' ) )) #function definition in file 'filename.R'

	gInfo$grouping = as.numeric( gInfo$grouping )



	gInfo$grouping[ which(is.na(gInfo$grouping))] = 0
	if ( any( ! is.numeric(gInfo$grouping)) ) {
		message("wrong data in gInfo$grouping")
		browser()
	}
	#gInfo$grouping = as.numeric(as.factor(gInfo$grouping))
	if ( ! gInfo$drc %in% names(cellexalObj@drc) ){
		stop( paste("group info does not match to cellexalObj data content: drc named", gInfo$drc, "not in list", paste( collapse=", ", names(cellexalObj@drc))))
	}

	#x@usedObj$samples[,group] = factor( x@usedObj$samples[,group] )

    #options(repr.plot.width=24, repr.plot.height=24)
    gr = factor(gInfo$grouping+1)

	grDevices::png( file= DRC1, width=1000, height=1000)

	toPlot = data.frame(x=cellexalObj@drc[[gInfo$drc]][,1], y=cellexalObj@drc[[gInfo$drc]][,2], id=gr )
    p= prettyPlot2D( toPlot, gInfo$col )
	print(p)
	
	grDevices::dev.off()
		
	DRC2 = file.path( sessionPath , 'png', filename(c(  gInfo$gname ,gInfo$drc, "2_3", 'png' ) )) #function definition in file 'filename.R'

	grDevices::png( file= DRC2, width=1000, height=1000)
	
    toPlot = data.frame(x=cellexalObj@drc[[gInfo$drc]][,2], y=cellexalObj@drc[[gInfo$drc]][,3], id=gr )
    p= prettyPlot2D( toPlot, gInfo$col )
    print(p)
	
	grDevices::dev.off()
	c( DRC1, DRC2)
} )


prettyPlot2D = function(x, col ){

	x$id = as.vector(x$id)
	x[,1] = as.numeric(x[,1])
	x[,2] = as.numeric(x[,2])

	p = ggplot(x, aes(x=x, y=y), col= c(grey(.6),col)[gr] ) 
	p = p +   geom_point(aes(color = id  ) , show.legend = FALSE)

	pos= t(sapply( unique(x$id), function(id) {
		ok = which(x$id == id); 
		c( median(x[ok,1]), median(x[ok,2]) )
	} ))
    theta <- seq(pi/8, 2*pi, length.out=48)
    xo <- diff(range(pos[,1]))/1200
    yo <- diff(range(pos[,2]))/1200
    for(i in theta) {
        p <- p + ggplot2::geom_text( data=data.frame(pos),
            ggplot2::aes_q(
                x = bquote(pos[,1]+.(cos(i)*xo)),
                y = bquote(pos[,2]+.(sin(i)*yo)),
                label=unique(x$id)), 
                    size=10, colour='black' )
    }
    p = p + ggplot2::annotate('text', x = pos[,1], y = pos[,2], label = unique(x$id), size = 10, col=c(grey(.6),col) )  
    p
}  



    