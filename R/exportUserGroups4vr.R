#' Creates a summary file for the vr process
#' @param cellexalObj A cellexalvr object
#' @param path the outpath
#' @description Creates a file groupings_info.txt in the outfolder that contains the group name (in the R object)
#' the numer of groups in the selection and the number of cells in the whole group.
#' @export exportUserGroups4vr
exportUserGroups4vr <- function( cellexalObj, path ) {
	names <- colnames(cellexalObj@userGroups) [grep('order', colnames(cellexalObj@userGroups), invert=T)]
	
	## create the grouping files for the VR process?
	for ( gname in names ) {
		fname = paste(paste( unlist(str_split( gname, '\\s+')), collapse='_'),'grp', sep='.' )
		fname = file.path( path,fname )
		if ( ! file.exists( fname ) ) {
			## create the grouping info for the VR process
			# no header
			# HSPC_639    #FF0000    DDRTree    0
			ids =  which( is.na(cellexalObj@userGroups[,gname]) == F)
			browser()
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
}