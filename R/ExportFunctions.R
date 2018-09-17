#' @name export2cellexalvr
#' @aliases export2cellexalvr,cellexalvrR-method
#' @rdname export2cellexalvr-methods
#' @docType methods
#' @description  Creates the base files needed to run the VR environment
#' @param cellexalObj A cellexalvr object
#' @param forceDB re-write the db even if it exisis (default =F)
#' @param path  TEXT MISSING
#' @param forceDB  TEXT MISSING default=F
#' @param VRpath  TEXT MISSING default=NULL
#' @title description of function export2cellexalvr
#' @export export2cellexalvr
if ( ! isGeneric('export2cellexalvr') ){setGeneric('export2cellexalvr', ## Name
	function (cellexalObj,path, forceDB=F, VRpath=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('export2cellexalvr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
) }

setMethod('export2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj,path, forceDB=F, VRpath=NULL ) {

	
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
			write.table(rq.triang,ofile,row.names=F,col.names=F,quote=F,sep="\t",eol="\n")
		}
		ofile = file.path(path,paste(names(cellexalObj@mds)[i],".mds",sep=""))
		if ( ! file.exists( ofile )) {
			write.table(cellexalObj@mds[[i]],ofile,row.names=T,col.names=F,quote=F,sep="\t",eol="\n")
		}
    }
	
	ofile = file.path(path,"database.sqlite")
	#if ( file.exists( ofile ) ) {
	#	if ( forceDB ){
#			unlink( ofile ) ## always create the database?!
#		}
#	}
	if ( ! file.exists(ofile) || forceDB==T ) {
	    #genes <- tolower(rownames(cellexalObj@data))
		genes <- rownames(cellexalObj@data)
		genes <- data.frame( 'id' = 1:length(genes), genes= genes )
	
		cells <- data.frame( 'id'= 1:ncol(cellexalObj@data), sample= colnames(cellexalObj@data) )
	
	    cdat <- data.frame(genes=genes$id,cellexalObj@data)
	
		colnames(cdat) <- c( 'genes', cells$id )
	
	    md <- melt(cdat, id=('genes') )
	
		to.remove <- which(md[,3]==0)

		mdc <- NULL

		if(length(to.remove>0)){
    		mdc <- md[-to.remove,]
		}else{mdc <- md}

		colnames(mdc) <- c('gene_id', 'cell_id','value')
		mdc$cell_id <- as.numeric(as.character(mdc$cell_id))
		colnames(genes) <- c('id', 'gname')
		colnames(cells) <- c('id','cname')
	
    	con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
		
    	RSQLite::dbWriteTable(con, "datavalues",mdc)

		dbSendStatement(con,"create table genes ('id' integer,'gname' varchar(20) collate nocase)")

		RSQLite::dbWriteTable(con, "genes", genes,append = TRUE)
		RSQLite::dbWriteTable(con, "cells", cells )
		
		dbSendStatement(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
		
    	RSQLite::dbDisconnect(con)
	}
	if ( ! is.null( VRpath ) ) {
		exportUserGroups4vr(cellexalObj, VRpath)
	}
	invisible(cellexalObj)

} )
#' @name checkVRfiles
#' @aliases checkVRfiles,cellexalvrR-method
#' @rdname checkVRfiles-methods
#' @docType methods
#' @description  checkVRfiles: Checks the existance of all VR specific files and re-runs the export
#' @description  function if any is missing.
#' @param cellexalObj the cellexal object
#' @param path the outpath to check
#' @param cellexalvr  TEXT MISSING
#' @param path  TEXT MISSING
#' @title description of function checkVRfiles
#' @export checkVRfiles
if ( ! isGeneric('checkVRfiles') ){setGeneric('checkVRfiles', ## Name
	function ( cellexalvr, path ) { ## Argumente der generischen Funktion
		standardGeneric('checkVRfiles') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
) }

setMethod('checkVRfiles', signature = c ('cellexalvrR'),
	definition = function ( cellexalvr, path ) {
	export2cellexalvr( cellexalvr, path, forceDB=F )
} )
