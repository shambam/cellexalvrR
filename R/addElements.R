

#' @name addCellMeta2cellexalvr
#' @aliases addCellMeta2cellexalvr,cellexalvrR-method
#' @rdname addCellMeta2cellexalvr-methods
#' @docType methods
#' @description  Adds per cell metadata to a 'cellexalvrObj'
#' @param cellexalObj, cellexalvr object
#' @param cell.meta A matrix of cell metadata
#' @title description of function 'addCellMeta2cellexalvr'
#' @export addCellMeta2cellexalvr
if ( ! isGeneric('addCellMeta2cellexalvr') ){setGeneric('addCellMeta2cellexalvr', ## Name
	function (cellexalObj, cell.meta) { 
		standardGeneric('addCellMeta2cellexalvr')
	}
) }

setMethod('addCellMeta2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, cell.meta) {

    rownames(cell.meta) <- colnames(cellexalObj@data)
    cellexalObj@meta.cell <- as.matrix(cell.meta)
    cellexalObj
} )
#' @name addFACS2cellexalvr
#' @aliases addFACS2cellexalvr,cellexalvrR-method
#' @rdname addFACS2cellexalvr-methods
#' @docType methods
#' @description  Adds FACS index to a cellexalvrObj
#' @param cellexalObj, cellexalvr object
#' @param facs.data A matrix of surface marker intensities
#' @title description of function 'addFACS2cellexalvr'
#' @export addFACS2cellexalvr
if ( ! isGeneric('addFACS2cellexalvr') ){setGeneric('addFACS2cellexalvr', ## Name
	function (cellexalObj, facs.data) { 
		standardGeneric('addFACS2cellexalvr')
	}
) }

setMethod('addFACS2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, facs.data) {

    rownames(facs.data) <- colnames(cellexalObj@data)
    cellexalObj@index <- as.matrix(facs.data)
    cellexalObj
} )
