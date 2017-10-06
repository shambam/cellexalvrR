# TODO: Add comment
# 
# Author: stefan
###############################################################################



#' Converts all sample annoation into 0/1 columns with meaningful names and 
#' adds all inbuilt gene annoations to the meta.gene table for later use.
#'@param cellexalObj A cellexalvr object
#' @param max the maximum complexity of one column to split it into max 0/1 columns (default =10)
#'@keywords export, vr
#' @return the updated cellexalvr object
#'@export vrPrepare

vrPrepare <- function ( x, max=10 ) {
	if ( class(x)[1] != 'cellexalvr'){
		stop( "Not a cellexal object")
	}
	new.meta.cell <- NULL
	for ( i in 1:ncol(x@meta.cell) ) {
		t <- table(x@meta.cell[,i])
		if (  identical(names(t), c('0','1')) ) {
			new.meta.cell <- cbind(new.meta.cell ,x@meta.cell[,i] )
		}else if ( length(t) <= max ) {
			for ( a in 1:length(t)){
				cname = paste( colnames(x@meta.cell)[i], names(t)[a], sep='.' )
				cname = gsub( '\\.\\.*','.',cname)
				new.meta.cell <- cbind(new.meta.cell , as.numeric(x@meta.cell[,i] == names(t)[a]) )
				colnames(new.meta.cell)[ncol(new.meta.cell)] <- cname
			}
		}else {
			print ( paste( "Column",  colnames(x@meta.cell)[i], "did conatin", length(t),"different entries - removed"))
		}
	}
	x@meta.cell <- new.meta.cell
	# now all columns should be OK
	# fix the inbuilt gene annotations
	for ( n in c('TFs', 'epigenetic')){
		if ( is.na( match(n, colnames(x@meta.gene))) ){
			x <- useInbuiltGOIlists(x, n)
		}
	}
	invisible(x)
}