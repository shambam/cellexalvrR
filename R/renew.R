#' @name renew
#' @aliases renew,cellexalvr-method
#' @rdname renew-methods
#' @docType methods
#' @description  update the class definition by re-creating the instance
#' @param x the object you want to update
#' @param x  TEXT MISSING
#' @title description of function renew
#' @export renew
setGeneric('renew', ## Name
		function ( x ) { ## Argumente der generischen Funktion
			standardGeneric('renew') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('renew', signature = c ('cellexalvr'),
		definition = function ( x ) {
			if( isS4(x)) {
				## shit an old object!
				ret <- cellexalvr$new(dat=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index )
				#browser()
				if( .hasSlot(x,'userGroups') ){
					ret$userGroups = x@userGroups 
				}
				if( .hasSlot(x,'colors') ){
					ret$colors = x@colors
				}
				if( .hasSlot(x,'usedObj') ){
					ret$usedObj = x@usedObj
				}
				x <- ret
			}else {
				ret <- cellexalvr$new(dat=as.matrix(x$data),mds=x$mds,meta.cell=x$meta.cell,meta.gene = x$meta.gene,  index = x$index)
				if( .hasSlot(x,'userGroups') ){
					ret$userGroups = x$userGroups 
				}
				if( .hasSlot(x,'colors') ){
					ret$colors = x$colors
				}
				if( .hasSlot(x,'usedObj') ){
					ret$usedObj = x$usedObj
				}
				x <- ret
			}
			## now lets add the inbuilt groupings...
			for ( name in c('TFs', 'epigenetic', 'CellCycle') ) {
				if ( is.na( match(name, colnames(x$meta.gene))) ) {
					useInbuiltGOIlists ( x, name )
				}
			}
			
			invisible(x)
		}  
)
