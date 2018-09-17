#' @name plotAllMDS
#' @aliases plotAllMDS,cellexalvrR-method
#' @rdname plotAllMDS-methods
#' @docType methods
#' @description  Plots the MDS reduced data for a quick look
#' @param cellexalObj A cellexalvr object
#' @title description of function plotAllMDS
#' @export plotAllMDS
if ( ! isGeneric('plotAllMDS') ){setGeneric('plotAllMDS', ## Name
	function (cellexalObj) { ## Argumente der generischen Funktion
		standardGeneric('plotAllMDS') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
) }

setMethod('plotAllMDS', signature = c ('cellexalvrR'),
	definition = function (cellexalObj) {

    for(i in 1:length(cellexalObj@mds)){
        rgl.open()
        rgl.points(cellexalObj@mds[[i]])
        
    }

} )
