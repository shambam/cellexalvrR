#'Creates the base files needed to run the VR environment
#'@param cellexalObj A cellexalvr object
#' @param forceDB re-write the db even if it exisis (default =F)
#'@export export2cellexalvr

export2cellexalvr <- function(cellexalObj,path, forceDB=F){

    save(cellexalObj,file=file.path(path,"cellexalObj.RData"))

    #write.table(cellexalObj@data,paste(path,"expression.expr",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n")
    write.table(cellexalObj@meta.cell,file.path(path,"a.meta.cell"),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n") 
    write.table(cellexalObj@index,file.path(path,"index.facs"),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n")
    write.table(cellexalObj@meta.gene,file.path(path,"c.meta.gene"),row.names=T,col.names=NA,quote=F,sep="\t",eol="\r\n")

    for(i in 1:length(cellexalObj@mds)){
        
        ashape <- ashape3d(cellexalObj@mds[[i]], alpha = 1)
        rq.triang <- ashape$triang[which(ashape$triang[,4]==1),1:3]
        
        write.table(cellexalObj@mds[[i]],file.path(path,paste("graph",i,".mds",sep="")),row.names=T,col.names=F,quote=F,sep="\t",eol="\r\n")
        write.table(rq.triang,file.path(path,paste("graph",i,".hull",sep="")),row.names=T,col.names=F,quote=F,sep="\t",eol="\r\n")
    }

    genes <- tolower(rownames(cellexalObj@data))
	genes <- data.frame( 'id' = 1:length(genes), genes= genes )
	
	cells <- data.frame( 'id'= 1:ncol(cellexalObj@data), sample= colnames(cellexalObj@data) )
	
    cdat <- data.frame(genes=genes$id,cellexalObj@data)
	
	colnames(cdat) <- c( 'genes', cells$id )
	
    md <- melt(cdat, id=('genes') )
	
	#browser()
    mdc <- md[-which(md[,3]==0),]

	colnames(mdc) <- c('gene_id', 'cell_id','value')
	mdc$cell_id <- as.numeric(as.character(mdc$cell_id))
	colnames(genes) <- c('id', 'gname')
	colnames(cells) <- c('id','cname')
	
    #browser()
	
	if ( file.exists( file.path(path,"database.sqlite")) ) {
		if ( forceDB ){
			unlink( file.path(path,"database.sqlite") ) ## always create the database?!
		}
	}
	if ( ! file.exists( file.path(path,"database.sqlite")) ) {
    	con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = file.path(path,"database.sqlite"))
		
    	RSQLite::dbWriteTable(con, "datavalues",mdc)
		RSQLite::dbWriteTable(con, "genes", genes)
		RSQLite::dbWriteTable(con, "cells", cells )
		
		dbGetQuery(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
		
    	RSQLite::dbDisconnect(con)
	}
	

}


#'Makes the base files needed to run the VR environment from a Seurat object
#'@param cellexalObj A cellexalvr object
#'@export seurat2cellexalvr


seurat2cellexalvr <- function(seuratObj){

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

    g <- new("cellexalvr",data=as.matrix(seuratObj@data),mds=list(graph1=proj),meta.cell=as.matrix(cell.met))
    #export2cellexalvr(g,path)
}

#'Adds mds coordinates to a cellexalvrObj
#'@param cellexalObj A cellexalvr object
#'@param mdsmatrix A matrix of coordinates
#'@export addMDS2cellexalvr

addMDS2cellexalvr <- function(cellexalObj,mdsmatrix){

    rq.ind <- (length(cellexalObj@mds)+1)
    rq.nm <- paste("graph",(length(cellexalObj@mds)+1),sep="")
    mp <- mdsmatrix
    colnames(mp) <- c("x","y","z")
    rownames(mp) <- colnames(cellexalObj@data)

    cellexalObj@mds[[rq.ind]] <- mp
    names(cellexalObj@mds)[rq.ind] <- rq.nm
    cellexalObj
}