#' Converts a seurat class to one of cellexalvr. If the cell-cycle score have been calculated they will be added to the cell.mata table
#' @param seuratObj the suerat object to be converted
#' @export seurat2cellexalvr
seurat2cellexalvr <- function(seuratObj){

    cell.att <- as.vector(seuratObj@ident)
    cell.t <- unique(cell.att)

    cell.met <- to.matrix(cell.att,cell.t)

	colnames(cell.met) <- paste(cell.t,".type",sep="")

	if(exists("Phase",where=seuratObj@meta.data)==T){

		phase.att <- as.vector(seuratObj@meta.data$Phase)
    	phase.t <- unique(phase.att)

    	phase.met <- to.matrix(phase.att,phase.t)
		colnames(phase.met) <- paste(phase.t,".type",sep="")
		cell.met <- cbind(cell.met,phase.met)

	}
    rownames(cell.met) <- seuratObj@cell.names
    proj <- as.matrix(seuratObj@dr$tsne@cell.embeddings)
    colnames(proj) <- c("x","y","z")

    g <- new("cellexalvr",data=as.matrix(seuratObj@data),
			mds=list(tsne=proj),meta.cell=as.matrix(cell.met))
    #export2cellexalvr(g,path)
}


#'Runs DDRtree for a Seurat class
#'@param seuratObj A cellexalvr object
#'@keywords ddrtree
#'@export run.ddrtree

run.ddrtree <- function(seuratObj){

    dat.samp <- as.matrix(seuratObj@data[seuratObj@var.genes,])
    ddr.samp <- DDRTree((dat.samp), dimensions=3)
    ddr.cood <- t(ddr.samp$Z)
    ddr.cood    
}

#'Sets new cell indentities from a given list
#'@param seuratObj A cellexalvr object
#'@param cell A cellexalvr object
#'@keywords cell type
#'@export changeIdent


changeIdent <- function(seuratObj,new.idents){

    new.ids <- rep("",length(seuratObj@cell.names))

    for(i in 1:length(new.idents)){

        new.ids[grep(as.character(-i),seuratObj@cell.names)] <- new.idents[i]

    }
    seuratObj<- SetIdent(seuratObj,ident.use=new.ids)
    seuratObj
}