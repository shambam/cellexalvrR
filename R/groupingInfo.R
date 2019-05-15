#' groupingInfo collects all information about one grouping from the cellexalvrR internals and returns them as list.
#' @name groupingInfo
#' @aliases groupingInfo,cellexalvrR-method
#' @rdname groupingInfo-methods
#' @docType methods
#' @description  returns the information stored for the last grouping read
#' @param cellexalObj, cellexalvr object
#' @param gname The optional group name to get info on a specific grouping (not the last)
#' @title get information on a sample grouping
#' @example groupingInfo(cellexalObj, 'User.group.4')
#' @returns a list with the entries 'grouping', 'order', 'mds' and 'col'
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
			order = 1:ncol(cellexalObj@data),
			'mds' = cellexalObj@groupSelectedFrom[[gname]],
			col = cellexalObj@colors[[gname]] 
	)
	if ( ! is.na(match(paste(cellexalObj@usedObj$lastGroup, 'order'), colnames(cellexalObj@data))) ){
		ret[['order']] = cellexalObj@userGroups[,paste(gname, 'order')]
	}
	
	ret
} )
