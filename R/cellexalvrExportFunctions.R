#'Creates the base files needed to run the VR environment
#'@param cellexalObj A cellexalvr object
#' @param forceDB re-write the db even if it exisis (default =F)
#'@export export2cellexalvr
export2cellexalvr <- function(cellexalObj,path, forceDB=F){

	
	ofile = file.path( path, "cellexalObj.RData")
	if ( ! file.exists( ofile) ){
		save(cellexalObj,file=ofile )
	}
    

    #write.table(cellexalObj@data,paste(path,"expression.expr",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	ofile = file.path(path,"a.meta.cell")
	if ( ! file.exists( ofile) ){
		write.table(cellexalObj@meta.cell,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}
	ofile = file.path(path,"index.facs")
	if ( ! file.exists( ofile) ){
		write.table(cellexalObj@index,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}
	ofile = file.path(path,"c.meta.gene")
	if ( ! file.exists( ofile) ){
		write.table(cellexalObj@meta.gene,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}

    for(i in 1:length(cellexalObj@mds)){
        
        #ashape <- ashape3d(as.matrix(cellexalObj@mds[[i]]), alpha = 5)
		#ofile = file.path(path,paste("graph",i,".hull",sep=""))
		ofile = file.path(path,paste(names(cellexalObj@mds)[i],".hull",sep=""))
		if ( ! file.exists( ofile )) {
        	rq.tring <- NULL

        	if(entropy(as.matrix(cellexalObj@mds[[i]]))<0){
	            ashape <- ashape3d(as.matrix(cellexalObj@mds[[i]]), alpha = 5)
            	#rgl.open()
            	#plot(ashape)
            	rq.triang <- ashape$triang[which(ashape$triang[,9]>1),1:3]
        	}

        	if(entropy(as.matrix(cellexalObj@mds[[i]]))>0){
	            ashape <- ashape3d(as.matrix(cellexalObj@mds[[i]]), alpha = 2)
            	#rgl.open()
            	#plot(ashape)
            	rq.triang <- ashape$triang[which(ashape$triang[,9]>1),1:3]
        	}
			write.table(rq.triang,ofile,row.names=T,col.names=F,quote=F,sep="\t",eol="\n")
		}
		ofile = file.path(path,paste(names(cellexalObj@mds)[i],".mds",sep=""))
		if ( ! file.exists( ofile )) {
			write.table(cellexalObj@mds[[i]],ofile,row.names=T,col.names=F,quote=F,sep="\t",eol="\n")
		}
    }
	
	ofile = file.path(path,"database.sqlite")
	if ( file.exists( ofile ) ) {
		if ( forceDB ){
			unlink( ofile ) ## always create the database?!
		}
	}
	if ( ! file.exists( ofile ) ) {
	    #genes <- tolower(rownames(cellexalObj@data))
		genes <- rownames(cellexalObj@data)
		genes <- data.frame( 'id' = 1:length(genes), genes= genes )
	
		cells <- data.frame( 'id'= 1:ncol(cellexalObj@data), sample= colnames(cellexalObj@data) )
	
	    cdat <- data.frame(genes=genes$id,cellexalObj@data)
	
		colnames(cdat) <- c( 'genes', cells$id )
	
	    md <- melt(cdat, id=('genes') )
	
    	mdc <- md[-which(md[,3]==0),]

		colnames(mdc) <- c('gene_id', 'cell_id','value')
		mdc$cell_id <- as.numeric(as.character(mdc$cell_id))
		colnames(genes) <- c('id', 'gname')
		colnames(cells) <- c('id','cname')
	
    	con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
		
    	RSQLite::dbWriteTable(con, "datavalues",mdc)

		dbGetQuery(con,"create table genes ('id' integer,'gname' varchar(20) collate nocase)")

		RSQLite::dbWriteTable(con, "genes", genes,append = TRUE)
		RSQLite::dbWriteTable(con, "cells", cells )
		
		dbGetQuery(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
		
    	RSQLite::dbDisconnect(con)
	}
	invisible(cellexalObj)

}


#' checkVRfiles: Checks the existance of all VR specific files and re-runs the export function if any is missing.
#' @param cellexalObj the cellexal object
#' @param path the outpath to check
#' @export checkVRfiles
checkVRfiles <- function( cellexalvr, path ) {
	export2cellexalvr( cellexalvr, path, forceDB=F )
}

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

#'Adds mds coordinates to a cellexalvrObj
#'@param cellexalObj A cellexalvr object
#'@param mdsmatrix A matrix of coordinates
#' @param name A name for the object (default = graph<n>)
#'@export addMDS2cellexalvr
addMDS2cellexalvr <- function(cellexalObj,mdsmatrix, name=NULL){

    rq.ind <- (length(cellexalObj@mds)+1)
	if ( ! is.null(name) ){
		rq.nm <- name
	}else {
    	rq.nm <- paste("graph",(length(cellexalObj@mds)+1),sep="")
	}
    mp <- mdsmatrix
    colnames(mp) <- c("x","y","z")
    rownames(mp) <- colnames(cellexalObj@data)

    cellexalObj@mds[[rq.ind]] <- mp
    names(cellexalObj@mds)[rq.ind] <- rq.nm
    cellexalObj
}