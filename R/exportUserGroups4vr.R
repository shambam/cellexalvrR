#' Creates a summary file for the vr process
#' @param cellexalObj A cellexalvr object
#' @param path the outpath
#' @description Creates a file groupings_info.txt in the outfolder that contains the group name (in the R object)
#' the numer of groups in the selection and the number of cells in the whole group.
#' @export defineGOIs
exportUserGroups4vr <- function( cellexalObj, path ) {
	names <- colnames(cellexalObj@userGroups) [-grep('order', colnames(cellexalObj@userGroups) )]

	groupN <- unlist(lapply( names, function(n) { length(cellexalObj@colors[[n]]) } ) )
	groupCount <- unlist(lapply( names, function(n) { length( which( is.na(cellexalObj@userGroups[,n]) == F)) } ) )
	ret <- cbind(  'group name' = names, 'groups [n]' =  groupN, 'cells [n]' = groupCount )
	write.table(ret, file=file.path( path, 'groupings_info.txt'), row.names=F, col.names=T, sep="\t", quote=F )
	ret
}