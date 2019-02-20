##' @name print
##' @aliases print,cellexalvr-method
##' @rdname print-methods
##' @docType methods
##' @description  print the cellexalvr
##' @param x the cellexalvr object
##' @return nothing
##' @title description of function print
##' @export print
#if ( ! isGeneric('print') ){setGeneric('print', ## Name
#			function ( x ) { 
#				standardGeneric('print') 
#			}
#	) }
#setMethod('print', signature = c ('cellexalvrR'),
#		definition = function (x) {
#			cat (paste("An x of class", class(x)),"\n" )
#			#cat("named ",x@name,"\n")
#			cat (paste( 'with',nrow(x@dat),'genes and', ncol(x@dat),' cells.'),"\n")
#			cat (paste("Annotation datasets (",paste(dim(x@meta.gene),collapse=','),"): '",paste( colnames(x@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
#			cat (paste("Sample annotation (",paste(dim(x@meta.cell),collapse=','),"): '",paste( colnames(x@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
#			cat ( paste("There are",length(grep('order', colnames(x@userGroups), invert=T)), "user groups stored" ),":\n")
#			if ( ncol(x@userGroups) > 0 ) {
#				cat ( paste( collapse=", ", colnames(x@userGroups)), '\n' )
#			}
#			if ( length(names(x@mds)) > 0 ){
#				cat ( "and", length(names(x@mds)), "mds x(s)\n")
#			}
#			cat (paste("Specie is set to", x@specie),"\n")
#			
#		}
#)
