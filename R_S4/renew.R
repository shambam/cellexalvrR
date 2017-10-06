#' @name renew
#' @aliases renew,cellexalvr-method
#' @rdname renew-methods
#' @docType methods
#' @description update the class definition by re-creating the instance
#' @param x the object you want to update
#' @title description of function renew
#' @export renew
renew <-  function ( x ) {
			ret <- new("cellexalvr",data=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, tfs= x@tfs)
			if( .hasSlot(x,'userGroups') ){
				ret@userGroups = x@userGroups 
			}
			if( .hasSlot(x,'colors') ){
				ret@colors = x@colors
			}
			if( .hasSlot(x,'usedObj') ){
				ret@usedObj = x@usedObj
			}
			x <- ret
			invisible(x)
} 
