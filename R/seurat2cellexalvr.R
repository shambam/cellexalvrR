#' @name seurat2cellexalvr
#' @aliases seurat2cellexalvr,cellexalvr-method
#' @rdname seurat2cellexalvr-methods
#' @docType methods
#' @description  Converts a seurat class to one of cellexalvr
#' @param seuratObj the suerat object to be converted
#' @param seuratObj  TEXT MISSING
#' @title description of function seurat2cellexalvr
#' @export seurat2cellexalvr
setGeneric('seurat2cellexalvr', ## Name
	function (seuratObj) { ## Argumente der generischen Funktion
		standardGeneric('seurat2cellexalvr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('seurat2cellexalvr', signature = c ('seurat'),
	definition = function (seuratObj) {

    cell.att <- as.vector(seuratObj@ident)
    cell.t <- unique(cell.att)

    cell.met <- matrix(0,ncol=length(cell.t),nrow=length(cell.att))

    for(i in 1:length(cell.t)){
        ind <- which(cell.att==cell.t[i])
        cell.met[ind,i] <- 1
    }

    colnames(cell.met) <- paste(cell.t,".type",sep="")
    rownames(cell.met) <- seuratObj@cell.names
    proj <- as.matrix(seuratObj@tsne.rot)
    colnames(proj) <- c("x","y","z")

    cellexalvr$new(data=as.matrix(seuratObj@data),
			mds=list('seuratTSNE'=proj),meta.cell=as.matrix(cell.met))
    #export2cellexalvr(g,path)
} )
