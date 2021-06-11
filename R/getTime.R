#' @name getTime
#' @aliases getTime,cellexalTime-method
#' @rdname getTime-methods
#' @docType methods
#' @description Accessor function to get a cellexalTime object from a cellexalvrR object by name.
#' @param cellexalObj the object to get the data from
#' @param name the name for this timeline
#' @title get a timeline or throw an error
#' @export
#if ( ! isGeneric('getTime') ){
setGeneric('getTime', ## Name
	function (  cellexalObj, name ) { 
		standardGeneric('getTime')
	}
)
#}


#' @rdname getTime-methods
setMethod('getTime', signature = c ('cellexalvrR', 'character'),
	definition = function ( cellexalObj, name ) {
	
	if ( is.null( cellexalObj@usedObj$timelines[[name]])){
		stop( paste("The timeline",name,"could not be found in this cellexal object" ) )	
	}
	cellexalObj@usedObj$timelines[[name]]
	})


#' @rdname getTime-methods
setMethod('getTime', signature = c ('cellexalvrR', 'cellexalTime'),
	definition = function ( cellexalObj, name ) {
		name
	})