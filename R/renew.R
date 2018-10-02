#' @name renew
#' @aliases renew,cellexalvr-method
#' @rdname renew-methods
#' @docType methods
#' @description  update the class definition by re-creating the instance This version also makes sure,
#' @description  that the returned object is an S4 object.
#' @param x the object you want to update
#' @title description of function renew
#' @export renew
if ( ! isGeneric('renew') ){setGeneric('renew', ## Name
	function ( x ) { 
		standardGeneric('renew') 
	}
) }
setMethod('renew', signature = c ('cellexalvr'), ## old R3 object
    definition = function (x) {
		browser()
		class(x) = 'cellexalvrR'
		renew(x)
	} )

setMethod('renew', signature = c ('cellexalvrR'),
	definition = function ( x ) {
			#ret <- new("cellexalvrR",data=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, tfs= x@tfs)
			ret = NULL
			if( isS4(x)) {
				## OK no R6 then ;-)
				ret = x
				tryCatch({  validObject(x) } ,  error = function(e) {
				
				ret <- new("cellexalvrR",data=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index)
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
				if( .hasSlot(x,'specie') ){
					ret@specie = x@specie
				}
				} )
			}else {
				ret <- new("cellexalvrR",data=as.matrix(x$data),mds=x$mds,meta.cell=x$meta.cell,meta.gene = x$meta.gene,  index = x$index, specie= x$specie)
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
}  )
