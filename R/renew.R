#' @name renew
#' @aliases renew,cellexalvr-method
#' @rdname renew-methods
#' @docType methods
#' @description update the class definition by re-creating the instance
#' This version also makes sure, that the returned object is an S4 object.
#' @param x the object you want to update
#' @title description of function renew
#' @export renew
renew <-  function ( x ) {
			ret <- new("cellexalvr",data=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, tfs= x@tfs)
			
			if( isS4(x)) {
				## OK no R6 then ;-)
				ret <- new("cellexalvr",data=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index )
				#browser()
				if( .hasSlot(x,'userGroups') ){
					ret@userGroups = x@userGroups 
				}
				if( .hasSlot(x,'colors') ){
					ret@colors = x@colors
				}
				if( .hasSlot(x,'usedObj') ){
					ret@usedObj = x@usedObj
				}
			}else {
				ret <- new("cellexalvr",data=as.matrix(x$data),mds=x$mds,meta.cell=x$meta.cell,meta.gene = x$meta.gene,  index = x$index)
				if( .hasSlot(x,'userGroups') ){
					ret$userGroups = x$userGroups 
				}
				if( .hasSlot(x,'colors') ){
					ret$colors = x$colors
				}
				if( .hasSlot(x,'usedObj') ){
					ret$usedObj = x$usedObj
				}
			}
			## now lets add the inbuilt groupings...
			for ( name in c('TFs', 'epigenetic', 'CellCycle', 'CellSurface') ) {
				if ( is.na( match(name, colnames(ret@meta.gene))) ) {
					useInbuiltGOIlists ( ret, name )
				}
			}
			
			invisible(ret)
} 
