#' @name exportUserGroups4vr
#' @aliases exportUserGroups4vr,cellexalvrR-method
#' @rdname exportUserGroups4vr-methods
#' @docType methods
#' @description  Creates a summary file for the vr process Creates a file groupings_info.txt in the
#' @description  outfolder that contains the group name (in the R object) the numer of groups in the
#' @description  selection and the number of cells in the whole group.
#' @param cellexalObj A cellexalvr object
#' @param path the outpath
#' @title description of function exportUserGroups4vr
#' @export exportUserGroups4vr
if ( ! isGeneric('exportUserGroups4vr') ){setGeneric('exportUserGroups4vr', ## Name
	function ( cellexalObj, path ) { ## Argumente der generischen Funktion
		standardGeneric('exportUserGroups4vr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
) }

setMethod('branch.point.analysis', signature = c ('character'),
		definition = function (cellexalObj,cellidfile,maxsig,outfile) {
			cellexalObj <- loadObject(cellexalObj)
			branch.point.analysis( cellexalObj,cellidfile,maxsig,outfile )
		}
)


setMethod('exportUserGroups4vr', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, path ) {
	cellexalObj <- loadObject(cellexalObj)
	
	names <- colnames(cellexalObj@userGroups) [grep('order', colnames(cellexalObj@userGroups), invert=T)]
	
	## create the grouping files for the VR process?
	for ( gname in names ) {
		fname = paste(paste( unlist(stringr::str_split( gname, '\\s+')), collapse='_'),'cgr', sep='.' )
		fname = file.path( path,fname )
		if ( ! file.exists( fname ) ) {
			## create the grouping info for the VR process
			# no header
			# HSPC_639    #FF0000    DDRTree    0
			ids =  which( is.na(cellexalObj@userGroups[,gname]) == F)
			if ( is.null(cellexalObj@colors[[gname]])){
				cellexalObj@colors[[gname]] <- rainbow( length(unique( as.integer( cellexalObj@userGroups[ids,gname] ) )))
			}
			t <- data.frame( 
				cellname = colnames(cellexalObj@data)[ids],
				color = cellexalObj@colors[[gname]][  cellexalObj@userGroups[ids,gname]  ],
				'parent.graph' = rep( 'unknown', length(ids) ),
				'gid' = as.integer( cellexalObj@userGroups[ids,gname] ) 
			)
			write.table( t, col.names=F, row.names=F, quote=F, file=fname, sep="\t" )
		}
	}
	
	groupN <- unlist(lapply( names, function(n) { length(cellexalObj@colors[[n]]) } ) )
	groupCount <- unlist(lapply( names, function(n) { length( which( is.na(cellexalObj@userGroups[,n]) == F)) } ) )
	ret <- cbind(  'group name' = names, 'groups [n]' =  groupN, 'cells [n]' = groupCount )
	write.table(ret, file=file.path( path, 'groupings_info.txt'), row.names=F, col.names=T, sep="\t", quote=F )
	
	ret
} )
