#' @name groupingInfo
#' @aliases groupingInfo,cellexalvr-method
#' @rdname groupingInfo-methods
#' @docType methods
#' @description  returns the information stored for the last grouping read
#' or the group defined in the gname option
#' @param cellexalObj the cellexalvr object
#' @param gname the group name you get the data for ( default last added group)
#' @title description of function groupingInfo
#' @export 
if ( ! isGeneric('groupingInfo') ){ setGeneric('groupingInfo', ## Name
	function ( cellexalObj, gname=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('groupingInfo') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)
}else {
	print ("Onload warn generic function 'groupingInfo' already defined - no overloading here!")
}

setMethod('groupingInfo', signature = c ('cellexalvr'),
	definition = function ( cellexalObj, gname=NULL ) {
	if ( is.null(gname)){
		gname = cellexalObj$usedObj$lastGroup
	}
	list( 
			grouping = cellexalObj$userGroups[,gname] ,
			order = cellexalObj$userGroups[,paste(gname, 'order')], 
			'mds' = cellexalObj$groupSelectedFrom[[gname]], 
			col = cellexalObj$colors[[gname]] 
	)
} )
