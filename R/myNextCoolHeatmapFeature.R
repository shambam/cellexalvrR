
#if ( ! isGeneric('renew') ){
setGeneric('myNextCoolHeatmapFeature', ## Name
	function (x, selection, outfile) { 
		standardGeneric('myNextCoolHeatmapFeature')
	}
)
#}

#' This function is linked to a button on the VR heatmaps.
#' In the default cellexalvrR version this function is a blank to allow non VR Bioinformaticians
#' to start a function they miss and want to implement themselves.
#' 
#' @name myNextCoolHeatmapFeature
#' @aliases myNextCoolHeatmapFeature,cellexalTime-method
#' @rdname myNextCoolHeatmapFeature-methods
#' @docType methods
#' @description checks for NA elements in the table and removes them
#' @param cellexalObj the cellexalvrR object
#' @param selection the selection file linked to the heatmap
#' @param outfile the outfile the VR process is waiting for
#' @title description of function check
#' @export 
setMethod('myNextCoolHeatmapFeature', signature = c ('cellexalvrR', 'character', 'character'),
	definition = function (x, selection, outfile) {

	# please implement you new functionality here:
	cellexalObj = loadObject(x)
	x = userGrouping( x, selection )

	# after you finished please create the outfile to message to the VR process that this function has finished
	fileConn<-file( outfile )
	writeLines( "Some text" , fileConn)
	close(fileConn)

	invisible(cellexalObj)
} )