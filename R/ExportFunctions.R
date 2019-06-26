
if ( ! isGeneric('export2cellexalvr') ){setGeneric('export2cellexalvr', ## Name
	function (cellexalObj,path, forceDB=F, VRpath=NULL ) { 
		standardGeneric('export2cellexalvr') 
	}
) }

#' This function creates all files necessary 
#' for the CellexalVR application to show this data.
#'
#' Consult the CellexalVR documentation for further path requrements.
#' @name export2cellexalvr
#' @aliases export2cellexalvr,cellexalvrR-method
#' @rdname export2cellexalvr-methods
#' @docType methods
#' @description  Creates the base files needed to run the VR environment
#' @param cellexalObj A cellexalvr object
#' @param path the oputpath to store the data in
#' @param forceDB re-write the db even if it exisis (default =F)
#' @param VRpath in order to re-color the data in the VR process the 
#' VR process needs the grouping names which will be exported if we get the correct path here
#' @title create the VR data folder necessary for CellexalVR
#' @examples
#' dir.create ('data')
#' export2cellexalvr(cellexalObj, path='data')
#' @export export2cellexalvr
setMethod('export2cellexalvr', signature = c ('cellexalvrR'),
	definition = function (cellexalObj,path, forceDB=F, VRpath=NULL ) {

	
	ofile = file.path( path, "cellexalObj.RData")
	if ( ! file.exists( ofile) ){
		cellexalObj@outpath = ''
		save(cellexalObj,file=ofile )
	}
    

    #write.table(cellexalObj@data,paste(path,"expression.expr",sep=""),row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	ofile = file.path(path,"a.meta.cell")
	if ( ! file.exists( ofile) ){
		utils::write.table(cellexalObj@meta.cell,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}
	ofile = file.path(path,"index.facs")
	if ( ! file.exists( ofile) ){
		if ( nrow(cellexalObj@index) == ncol(cellexalObj@data) ){
			utils::write.table(cellexalObj@index,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
		}
	}
	ofile = file.path(path,"c.meta.gene")
	if ( ! file.exists( ofile) ){
		utils::write.table(cellexalObj@meta.gene,ofile,row.names=T,col.names=NA,quote=F,sep="\t",eol="\n")
	}

    for(i in 1:length(cellexalObj@drc)){
        
        #ashape <- ashape3d(as.matrix(cellexalObj@drc[[i]]), alpha = 5)
		#ofile = file.path(path,paste("graph",i,".hull",sep=""))
		ofile = file.path(path,paste(names(cellexalObj@drc)[i],".hull",sep=""))
		#if ( ! file.exists( ofile )) {
		if ( FALSE ) { # never create them the VR process is taking care of that now.
        	rq.tring <- NULL

        	if(entropy::entropy(as.matrix(cellexalObj@drc[[i]]))<0){
	            ashape <- alphashape3d::ashape3d(as.matrix(cellexalObj@drc[[i]]), alpha = 5)
            	#rgl.open()
            	#plot(ashape)
            	rq.triang <- ashape$triang[which(ashape$triang[,9]>1),1:3]
         	}

        	if(entropy::entropy(as.matrix(cellexalObj@drc[[i]]))>0){
	            ashape <- alphashape3d::ashape3d(as.matrix(cellexalObj@drc[[i]]), alpha = 2)
           	#rgl.open()
            	#plot(ashape)
            	rq.triang <- ashape$triang[which(ashape$triang[,9]>1),1:3]
        	}

			utils::write.table(format(rq.triang,scientific=FALSE),ofile,row.names=F,col.names=F,quote=F,sep="\t",eol="\n")

		}
		ofile = file.path(path,paste(names(cellexalObj@drc)[i],".mds",sep=""))
		if ( ! file.exists( ofile )) {
			utils::write.table(cellexalObj@drc[[i]],ofile,row.names=T,col.names=F,quote=F,sep="\t",eol="\n")
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
		
		## melt the sparse matrix using the toColNums Rcpp function
		mdc = FastWilcoxTest::meltSparseMatrix( cellexalObj@data )
		
		colnames(genes) <- c('id', 'gname')
		colnames(cells) <- c('id','cname')
	
    	con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
		
    	RSQLite::dbWriteTable(con, "datavalues",mdc)
		
		RSQLite::dbSendStatement(con,"create table genes ('id' integer not null unique,'gname' varchar(20) )")
        
		RSQLite::dbSendStatement(con,"create table cells ('id' integer not null unique,'cname' varchar(20) )")

		RSQLite::dbWriteTable(con, "genes", genes, append = TRUE)
		RSQLite::dbWriteTable(con, "cells", cells, append = TRUE)

		RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX gnameIDX on genes ( gname )")
		RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX cnameIDX on cells ( cname )")
		
		RSQLite::dbSendStatement(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
		RSQLite::dbSendStatement(con,"create index cell_id_data ON datavalues ( 'cell_id' )")

		
    	RSQLite::dbDisconnect(con)


	}
	if ( ! is.null( VRpath ) ) {
		exportUserGroups4vr(cellexalObj, VRpath)
	}
	invisible(cellexalObj)

} )

#' SQLite is the database the VR reads the expression values from.
#' 
#' This function creates these databases from the data stored in the data slot.
#' 
#' @name write_as_sqlite3
#' @aliases write_as_sqlite3,cellexalvrR-method
#' @rdname write_as_sqlite3-methods
#' @docType methods
#' @description save the cellexalObj@data object without questions asked.
#' @param cellexalObj the cellexalvrR object
#' @param ofile the database outfile
#' @title write a cellexalvrR objects data to a 'sqlite3' database of name ofile.
#' @export 
setGeneric('write_as_sqlite3', ## Name
		function ( cellexalObj, ofile )  { ## Argumente der generischen Funktion
			standardGeneric('write_as_sqlite3') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('write_as_sqlite3', signature = c ('cellexalvrR'),
		definition = function ( cellexalObj, ofile )  {
			
			genes <- rownames(cellexalObj@data)
			genes <- data.frame( 'id' = 1:length(genes), genes= genes )
			
			cells <- data.frame( 'id'= 1:ncol(cellexalObj@data), sample= colnames(cellexalObj@data) )
			
			## melt the sparse matrix using the toColNums Rcpp function
			mdc = FastWilcoxTest::meltSparseMatrix( cellexalObj@data )
			
			colnames(genes) <- c('id', 'gname')
			colnames(cells) <- c('id','cname')
			
			con <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = ofile )
			
			RSQLite::dbWriteTable(con, "datavalues",mdc)
			
			RSQLite::dbSendStatement(con,"create table genes ('id' integer not null unique,'gname' varchar(20) )")
			
			RSQLite::dbSendStatement(con,"create table cells ('id' integer not null unique,'cname' varchar(20) )")
			
			RSQLite::dbWriteTable(con, "genes", genes, append = TRUE)
			RSQLite::dbWriteTable(con, "cells", cells, append = TRUE)
			
			RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX gnameIDX on genes ( gname )")
			RSQLite::dbSendStatement(con, "CREATE UNIQUE INDEX cnameIDX on cells ( cname )")
			
			RSQLite::dbSendStatement(con,"create index gene_id_data ON datavalues ( 'gene_id' )")
			RSQLite::dbSendStatement(con,"create index cell_id_data ON datavalues ( 'cell_id' )")
			
			
			RSQLite::dbDisconnect(con)
			
		} )

