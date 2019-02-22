#' @name forceAbsoluteUniqueSample
#' @aliases forceAbsoluteUniqueSample,character-method
#' @rdname forceAbsoluteUniqueSample-methods
#' @docType methods
#' @description  force absolute unique names in a vector by adding _<amount of repeats> to each value
#' @description  if there is more than one repeat opf the value @exportMethod
#' @param x the StefansExpressionSet object
#' @param separator '_' or anything you want to set the separator to
#' @title description of function forceAbsoluteUniqueSample
if ( ! isGeneric('forceAbsoluteUniqueSample') ){ setGeneric('forceAbsoluteUniqueSample', ## Name
	function ( x ,separator='_') { ## Argumente der generischen Funktion
		standardGeneric('forceAbsoluteUniqueSample') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)
}else {
	print ("Onload warn generic function 'forceAbsoluteUniqueSample' already defined - no overloading here!")
}

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

