#' @name entrop
#' @aliases entrop,cellexalvrR-method
#' @rdname entrop-methods
#' @docType methods
#' @description replaced by c++ FastWilcoxTest::entropy ( twice as fast )
#' @param d a vector of group names ( class character)
#' @title description of function entrop
#' @export 
setGeneric('entrop', ## Name
	function (d) { ## Argumente der generischen Funktion
		standardGeneric('entrop') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('entrop', signature = c ('character'),
	definition = function (d) {
    if ( length(d) < 5) {
      return( 0 )
    }

  a = table(d)
  ret = 0
   if ( length(a) > 1 ) {
      p = a/sum(a)
      ret = -sum(p*log(p))
   }
 ret
} )
