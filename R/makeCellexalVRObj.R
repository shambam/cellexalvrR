#' @name MakeCellexaVRObj
#' @aliases MakeCellexaVRObj,cellexalvrR-method
#' @rdname MakeCellexaVRObj-methods
#' @docType methods
#' @description  Creates a cellexalvr objext with the data given
#' @param exdata A matrix of expression values (required). Colnames are cell IDs, rownames are unique gene names
#' @param mds.list A list of 3-column MDS coords (at least one required)
#' @param specie Specify whether data is from mouse or human (required)
#' @param cell.metadata Required meta data for cells
#' @param facs.data Surface marker intensities from index sorted cells
#' @keywords heatmap
#' @title description of function MakeCellexaVRObj
#' @export MakeCellexaVRObj
if ( ! isGeneric('MakeCellexaVRObj') ){setGeneric('MakeCellexaVRObj', ## Name
	function (exdata,mds.list,specie=c("mouse","human"),cell.metadata=NULL,facs.data=NULL) { 
		standardGeneric('MakeCellexaVRObj') 
	}
) }

setMethod('MakeCellexaVRObj', signature = c ('cellexalvrR'),
	definition = function (exdata,mds.list,specie=c("mouse","human"),cell.metadata=NULL,facs.data=NULL) {

    ### add the rownames to the given matricies
    for(i in 1:length(mds.list)){
        rownames(mds.list[[i]]) <- colnames(exdata)
    }
    
    if(!is.null(cell.metadata)){
        rownames(cell.metadata) <- colnames(exdata)
    }

    #cellexalobj <- new("cellexalvr",data=as.matrix(exdata),mds=mds.list,meta.cell=as.matrix(cell.metadata),index=facs.data)
    cellexalobj <- new("cellexalvr",data=as.matrix(exdata),mds=mds.list,meta.cell=as.matrix(cell.metadata))

    if(!is.null(facs.data)){
        cellexalobj  <- addFACS2cellexalvr(cellexalobj,facs.data)
    }

    cellexalobj <- set.specie(cellexalobj,specie)

    cellexalobj
} )
