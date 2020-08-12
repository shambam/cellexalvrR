#' check  cellexalvrR sanity check.
#' @name check
#' @aliases check,cellexalvrR-method
#' @rdname check-methods
#' @docType methods
#' @description checks cellexalvrR internals
#' @param x the cellexalvrR object
#' @title cellexal internal checks
#' @export 

setGeneric('check', ## Name
	function ( x ) {
		standardGeneric('check')
	}
)

setMethod('check', signature = c ('cellexalvrR'),
	definition = function ( x ) {
	
	cn = colnames(x@data)
	rn = rownames(x@data)

	OK = FALSE
	error = NULL;

	if ( isTRUE( all.equal(rownames(x@meta.cell), cn) ) ){
		OK = TRUE
	}
	
	if ( ! OK ) {
		error = c(error,"the data colnames are not the same as the meta.cell rownames!")
	}

	if ( isFALSE( all.equal( rn, rownames(x@meta.gene)) ) ) {
		error = c( error,"the data rownames are not the same as the meta.gene rownames!")
	}

	if ( ! isTRUE( all.equal(names(table(cellexalObj@meta.cell[] )), c('0','1'))) ){
		error = c( error,"meta.cells is not a 0/1 table")
	}
	
	for( n in names( x@drc ) ){
		OK = TRUE
		if ( ! isTRUE(all.equal(rownames(x@drc[[n]]), cn) ) ){
			if ( length(nrow( x@drc[[n]])) == length(cn) ){
				OK = FALSE
			}
			if ( length(which(is.na(match(rownames(x@drc[[n]]), cn )))) > 0 ) {
				browser()
				OK =FALSE
			}
		}
		if ( ! OK ){
			error = c( error, paste(
				"ERROR:: the data drc",
				n,
				"has NOT the same cells as the data object!") )
		}
	}

	if ( ! is.null(x@usedObj$timelines)){
		for (n in names(x@usedObj$timelines)){
			checkTime(x, x@usedObj$timelines[[n]] )
		}
	}
	
	if ( !is.null(error) ){
		message(paste(collapse=" \n\r",c ( "check cellexalvrR", error) ) )
	}else {
		message("seams OK")
	}
	

	invisible( x )
} )
