#' @name forceAbsoluteUniqueSample
#' @aliases forceAbsoluteUniqueSample,cellexalvr-method
#' @rdname forceAbsoluteUniqueSample-methods
#' @docType methods
#' @description A simple function that forces all samplenames to be unique.
#' @param x the vector of sample names
#' @param separator  TEXT MISSING default='_'
#' @title description of function forceAbsoluteUniqueSample
#' @export 
setGeneric('forceAbsoluteUniqueSample', ## Name
	function ( x ,separator='_') { ## Argumente der generischen Funktion
		standardGeneric('forceAbsoluteUniqueSample') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('forceAbsoluteUniqueSample',
		, signature = c ( 'character') ,
		definition = function ( x ,separator='_') {
			last = ''
			ret <- vector(length=length(x))
			for ( i in 1:length(x) ){
				if ( is.null(ret) ){
					last = x[i]
					ret[i] <- last
				}
				else{
					last = x[i]
					if ( ! is.na(match( last, ret )) ){
						last <- paste(last,separator,sum( ! is.na(match( x[1:i], last )))-1, sep = '')
					}
					ret[i] <- last
				}
			}
			ret
		})



setMethod('forceAbsoluteUniqueSample',
		, signature = c ( 'factor') ,
		definition = function ( x ,separator='_') {
			last = ''
			ret <- vector(length=length(x))
			for ( i in 1:length(x) ){
				if ( is.null(ret) ){
					last = x[i]
					ret[i] <- last
				}
				else{
					last = x[i]
					if ( ! is.na(match( last, ret )) ){
						last <- paste(last,separator,sum( ! is.na(match( x[1:i], last )))-1, sep = '')
					}
					ret[i] <- last
				}
			}
			ret
		})


