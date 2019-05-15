#' @name filename
#' @aliases filename,character-method
#' @rdname filename-methods
#' @docType methods
#' @description simple function to join by '.' and remove whitespace.
#' @param str a string vector
#' @title create a filename without spaces from a list of strings
#' @export 
setGeneric('filename', ## Name
	function ( str ) { 
		standardGeneric('filename')
	}
)

setMethod('filename', signature = c ('character'),
	definition = function ( str ) {
	of = paste( collapse=".", str )
	of = stringr::str_replace_all(of, "\\s+",'')
	stringr::str_replace_all(of, "[:-]",'_')
} )
