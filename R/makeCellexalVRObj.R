#'Creates a cellexalvr objext with the data given
#'@param exdata A matrix of expression values (required). Colnames are cell IDs, rownames are unique gene names
#'@param mds.list A list of 3-column MDS coords (at least one required)
#'@param specie Specify whether data is from mouse of human (required)
#'@param cell.metadata Required meta data for cells
#'@param facs Surface marker intensities from index sorted cells
#'@keywords heatmap
#'@export MakeCellexaVRObj

MakeCellexaVRObj <- function(exdata,mds.list,specie=c("mouse","human"),cell.metadata=NULL,facs.data=NULL){

    ### add the rownames to the given matricies

    for(i in 1:length(mds.list)){
        rownames(mds.list[[i]]) <- colnames(exdata)
    }

    if(cell.metdata){
        rownames(cell.metdata) <- colnames(exdata)
    }

    if(facs.data){
        rownames(facs) <- colnames(exdata)
    }

    cellexalobj <- new("cellexalvr",data=as.matrix(exdata),mds=mds.list,meta.cell=as.matrix(cell.metadata),facs=facs.data)
    cellexalobj
}


