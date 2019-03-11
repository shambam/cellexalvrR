#' @name groupingInfo
#' @aliases groupingInfo,cellexalvrR-method
#' @rdname groupingInfo-methods
#' @docType methods
#' @description  returns the information stored for the last grouping read
#' @param cellexalObj, cellexalvr object
#' @param gname The optional group name to get info on a specific grouping (not the last)
#' @param cellexalObj, TEXT MISSING
#' @param gname  TEXT MISSING default=NULL
#' @title description of function groupingInfo
#' @keywords groupingInfo
#' @export groupingInfo
if ( ! isGeneric('groupingInfo') ){setGeneric('groupingInfo', ## Name
	function ( cellexalObj, gname=NULL ) { 
		standardGeneric('groupingInfo') 
	}
) }

setMethod('groupingInfo', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, gname=NULL ) {
	if ( is.null(gname)){
		gname = cellexalObj@usedObj$lastGroup
	}
	ret <- list( 
			grouping = cellexalObj@userGroups[,gname] ,
			order = 1:ncol(cellexalObj@dat),
			'mds' = cellexalObj@usedObj[[gname]],
			col = cellexalObj@usedObj[[gname]] 
	)
	if ( ! is.na(match(paste(cellexalObj@usedObj$lastGroup, 'order'), colnames(cellexalObj@dat))) ){
		ret[['order']] = cellexalObj@userGroups[,paste(gname, 'order')]
	}
	
	ret
} )
