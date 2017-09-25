#' An easy function to register the inbuilt (G)enes (O)f (I)nterest lists 'TFs' and 'epigenetic'
#' @param cellexalObj A cellexalvr object
#' @param name the name of the inbuilt list to use ( either 'TFs' or 'epigenetic' for now)
#' @export useInbuiltGOIlists
useInbuiltGOIlists <- function(cellexalObj, name ) {
	
	if ( ! is.na( match(name, colnames(cellexalObj@meta.gene)))) {
		stop( "This GIO list has already been defined" )
	}
	
	if ( name == "TFs" ) {
		## register TFs
		if ( length(cellexalObj@tfs) > 0 ){
			cellexalObj = defineGOIs( cellexalObj, 'TFs', cellexalObj@tfs[!cellexalObj@tfs==""] )
		}
		else {
			## use inbuilt lists
			if ( length(which(is.na(match(rownames(cellexalObj@data),human.tfs))==F)) > 0 ){
				cellexalObj = defineGOIs( cellexalObj, 'TFs', human.tfs )
			}else if ( length(which(is.na(match( rownames(cellexalObj@data), mouse.tfs))==F)) > 0){
				cellexalObj = defineGOIs( cellexalObj, 'TFs', mouse.tfs )
			}else {
				stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(@data) - please double ckech that.")
			}
		}
	}
	else if ( name == 'epigenetic' ) {
		# register 'epigeneic'
		if ( length(which(is.na(match(rownames(cellexalObj@data),Epigenetic$HGNC_symbol))==F)) > 0 ){
			cellexalObj = defineGOIs( cellexalObj, 'TFs', Epigenetic$HGNC_symbol )
		}else if ( length(which(is.na(match( rownames(cellexalObj@data),Epigenetic$MGI_symbol ))==F)) > 0){
			cellexalObj = defineGOIs( cellexalObj, 'TFs', Epigenetic$MGI_symbol )
		}else {
			stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(@data) - please double ckech that.")
		}
	}
	else {
		stop ( paste("Sorry, but the gene list", name, "is not defined" ) )
	}
	cellexalObj
}