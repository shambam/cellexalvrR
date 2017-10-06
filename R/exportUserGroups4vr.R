#' @name exportUserGroups4vr
#' @aliases exportUserGroups4vr,cellexalvr-method
#' @rdname exportUserGroups4vr-methods
#' @docType methods
#' @description  Creates a summary file for the vr process Creates a file groupings_info.txt in the
#' @description  outfolder that contains the group name (in the R object) the numer of groups in the
#' @description  selection and the number of cells in the whole group.
#' @param cellexalObj A cellexalvr object
#' @param path the outpath
#' @title description of function exportUserGroups4vr
#' @export exportUserGroups4vr
setGeneric('exportUserGroups4vr', ## Name
	function ( cellexalObj, path ) { ## Argumente der generischen Funktion
		standardGeneric('exportUserGroups4vr') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('exportUserGroups4vr', signature = c ('cellexalvr'),
	definition = function ( cellexalObj, path ) {
	names <- colnames(cellexalObj$userGroups) [-grep('order', colnames(cellexalObj$userGroups) )]

	groupN <- unlist(lapply( names, function(n) { length(cellexalObj$colors[[n]]) } ) )
	groupCount <- unlist(lapply( names, function(n) { length( which( is.na(cellexalObj$userGroups[,n]) == F)) } ) )
	ret <- cbind(  'group name' = names, 'groups [n]' =  groupN, 'cells [n]' = groupCount )
	write.table(ret, file=file.path( path, 'groupings_info.txt'), row.names=F, col.names=T, sep="\t", quote=F )
	ret
} )
