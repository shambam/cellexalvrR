#' returns the information stored for the last grouping read
#'@param cellexalObj A cellexalvr object
#'@param gname The optional group name to get info on a specific grouping (not the last)
#'@keywords groupingInfo
#'@export groupingInfo

groupingInfo <- function( cellexalObj, gname=NULL ) {
	if ( is.null(gname)){
		gname = cellexalObj@usedObj$lastGroup
	}
	list( grouping = cellexalObj@userGroups[,gname] , 'mds' = cellexalObj@groupSelectedFrom[[gname]], col = cellexalObj@colors[[gname]] )
}