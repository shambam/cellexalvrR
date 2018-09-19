#' @name filename
#' @aliases filename,cellexalvrR-method
#' @rdname filename-methods
#' @docType methods
#' @description simple function to join by '.' and remove whitespace.
#' @param str a string vector
#' @title description of function filename
#' @export 
setGeneric('filename', ## Name
	function ( str ) { ## Argumente der generischen Funktion
		standardGeneric('filename') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('filename', signature = c ('character'),
	definition = function ( str ) {
	of = paste( collapse=".", str )
	stringr::str_replace_all(of, "\\s+",'')
} )
