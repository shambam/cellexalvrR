
setGeneric('reset', ## Name
	function ( x ) {
		standardGeneric('reset')
	}
) 


#' @name reset
#' @aliases reset,cellexalvrR-method
#' @rdname reset-methods
#' @docType methods
#' @description  remove all internals originating from previouse analyses
#' @param x the cellexalObj you want to reset
#' @title description of function reset
#' @export 
setMethod('reset', signature = c ('cellexalvrR'), definition = function ( x ) {
	
	x@usedObj$sessionPath = NULL
	x@usedObj$sessionRmdFiles = NULL
	x@usedObj$sessionName = NULL
	x@userGroups=data.frame()
	x@usedObj$lastGroup = NULL
	x@usedObj$SelectionFiles = list()
	x@groupSelectedFrom = list()
	x@usedObj$timelines = list()
	x@outpath= getwd()
	if ( file.exists( file.path(x@outpath, 'mainServer.sessionName')) ){
		unlink( file.path(x@outpath, 'mainServer.sessionName') )
	}
	return (x)
})