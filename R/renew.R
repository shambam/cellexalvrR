
if ( ! isGeneric('renew') ){setGeneric('renew', ## Name
	function ( x ) {
		standardGeneric('renew')
	}
) }


#' @name renew
#' @aliases renew,cellexalvrR-method
#' @rdname renew-methods
#' @docType methods
#' @description  updatae the class definition by re-creating the instance This version also makes sure,
#' @description  that the returned object is an S4 object.
#' @param x the object you want to updatae
#' @title description of function renew
#' @export renew
setMethod('renew', signature = c ('cellexalvrR'),
	definition = function ( x ) {
			#ret <- new("cellexalvrR",data=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index, tfs= x@tfs)
			ret = NULL
			if( isS4(x)) {
				## OK no R6 then ;-)
				ret = x
				tryCatch({  methods::validObject(x) } ,  error = function(e) {

				ret <- new("cellexalvrR",data=as.matrix(x@data),mds=x@mds,meta.cell=x@meta.cell,meta.gene = x@meta.gene,  index = x@index)
				#browser()
				if( methods::.hasSlot(x,'userGroups') ){
					ret@userGroups = x@userGroups
				}
				if( methods::.hasSlot(x,'colors') ){
					ret@colors = x@colors
				}
				if( methods::.hasSlot(x,'usedObj') ){
					ret@usedObj = x@usedObj
				}
				if( methods::.hasSlot(x,'specie') ){
					ret@specie = x@specie
				}
				} )
			}else {
				ret <- methods::new("cellexalvrR",data=as.matrix(x$data),mds=x$mds,meta.cell=x$meta.cell,meta.gene = x$meta.gene,  index = x$index, specie= x$specie)
				if( methods::.hasSlot(x,'userGroups') ){
					ret$userGroups = x$userGroups
				}
				if( methods::.hasSlot(x,'colors') ){
					ret$colors = x$colors
				}
				if( methods::.hasSlot(x,'usedObj') ){

					ret$usedObj = x$usedObj
				}
			}
			## now lets add the inbuilt groupings...
#			for ( name in c('TFs', 'epigenetic', 'CellCycle', 'CellSurface') ) {
#				if ( is.na( match(name, colnames(ret@meta.gene))) ) {
#					useInbuiltGOIlists ( ret, name )
#				}
#			}

			invisible(ret)
}  )

#' @describeIn renew cellexalvrR
#' @docType methods
#' @description renew the OLD cellexalvr objects
#' @param x the old cellexalvr (not cellexalvrR) object
#' @title description of function renew
#' @export renew
setMethod('renew', signature = c ('cellexalvr'), ## old R3 object
		definition = function (x) {
			class(x) = 'cellexalvrR'
			renew(x)
		} )
