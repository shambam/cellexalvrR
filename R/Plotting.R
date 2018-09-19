#' @name plotAllMDS
#' @aliases plotAllMDS,cellexalvrR-method
#' @rdname plotAllMDS-methods
#' @docType methods
#' @description  Plots the MDS reduced data for a quick look
#' @param cellexalObj, cellexalvr object
#' @title description of function plotAllMDS
#' @export plotAllMDS
if ( ! isGeneric('plotAllMDS') ){setGeneric('plotAllMDS', ## Name
	function (cellexalObj) { 
		standardGeneric('plotAllMDS') 
	}
) }

setMethod('plotAllMDS', signature = c ('cellexalvrR'),
	definition = function (cellexalObj) {

    for(i in 1:length(cellexalObj@mds)){
        rgl.open()
        rgl.points(cellexalObj@mds[[i]])
        
    }

} )
