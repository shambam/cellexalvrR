#' @name useInbuiltGOIlists
#' @aliases useInbuiltGOIlists,cellexalvr-method
#' @rdname useInbuiltGOIlists-methods
#' @docType methods
#' @description  An easy function to register the inbuilt (G)enes (O)f (I)nterest lists 'TFs' and
#' @description  'epigenetic' are supported at the moment
#' @param cellexalObj A cellexalvr object
#' @param name the name of the inbuilt list to use ( either 'TFs' or 'epigenetic' for now)
#' @param cellexalObj  TEXT MISSING
#' @param name  TEXT MISSING
#' @title description of function useInbuiltGOIlists
#' @export useInbuiltGOIlists
if ( ! isGeneric('useInbuiltGOIlists') ){ setGeneric('useInbuiltGOIlists', ## Name
		function (cellexalObj, name ) { ## Argumente der generischen Funktion
			standardGeneric('useInbuiltGOIlists') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)
}else {
	print ("Onload warn generic function 'useInbuiltGOIlists' already defined - no overloading here!")
}

setMethod('useInbuiltGOIlists', signature = c ('cellexalvr'),
		definition = function (cellexalObj, name ) {
			
			if ( ! is.na( match(name, colnames(cellexalObj$meta.gene)))) {
				stop( "This GIO list has already been defined" )
			}
			
			if ( name == "TFs" ) {
				## register TFs
				if ( length(cellexalObj$tfs) > 0 ){
					cellexalObj = defineGOIs( cellexalObj, 'TFs', cellexalObj$tfs[!cellexalObj$tfs==""] )
				}
				else {
					## use inbuilt lists
					hum_t <- length(which(is.na(match(rownames(cellexalObj$data),human.tfs))==F))
					mouse_t <- length(which(is.na(match( rownames(cellexalObj$data), mouse.tfs))==F))
					if (hum_t > mouse_t ){
						cellexalObj = defineGOIs( cellexalObj, name, human.tfs )
					}else if ( mouse_t > hum_t ){
						cellexalObj = defineGOIs( cellexalObj, name, mouse.tfs )
					}else {
						stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
					}
				}
			}
			else if ( name == 'epigenetic' ) {
				# register 'epigeneic'
				hum_e <- length(which(is.na(match(rownames(cellexalObj$data),Epigenetic$HGNC_symbol))==F))
				mouse_e <- length(which(is.na(match( rownames(cellexalObj$data),Epigenetic$MGI_symbol ))==F))
				if ( hum_e > mouse_e){
					cellexalObj = defineGOIs( cellexalObj, name, Epigenetic$HGNC_symbol, Epigenetic$Target )
				}else if ( mouse_e > hum_e ){
					cellexalObj = defineGOIs( cellexalObj, name, Epigenetic$MGI_symbol, Epigenetic$Target)
				}else {
					stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
				}
			}
			else if ( name =="CellCycle" ) {
				hum_e <- length(which(is.na(match(rownames(cellexalObj$data),CellCycle$Gene.Symbol))==F))
				mouse_e <- length(which(is.na(match( rownames(cellexalObj$data),CellCycle$MouseGene ))==F))
				if ( hum_e > mouse_e){
					cellexalObj = defineGOIs( cellexalObj, name, CellCycle$Gene.Symbol, CellCycle$X )
				}else if ( mouse_e > hum_e ){
					cellexalObj = defineGOIs( cellexalObj, name, CellCycle$MouseGene, CellCycle$X )
				}else {
					stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
				}
			}
			else if ( name =="CellSurface" ) {
				hum_e <- length(which(is.na(match(rownames(cellexalObj$data),human.CellSurface))==F))
				mouse_e <- length(which(is.na(match( rownames(cellexalObj$data),mouse.CellSurface ))==F))
				if ( hum_e > mouse_e){
					cellexalObj = defineGOIs( cellexalObj, name, human.CellSurface)
				}else if ( mouse_e > hum_e ){
					cellexalObj = defineGOIs( cellexalObj, name, mouse.CellSurface )
				}else {
					stop( "Sorry, but neither inbuilt dataset (Gene Symbols from mouse and humans) do match to the rownames(data$data) - please double ckech that.")
				}
			}
			else {
				stop ( paste("Sorry, but the gene list", name, "is not defined" ) )
			}
			cellexalObj
		} 
)
