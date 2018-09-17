#' @name cleanUpGroup
#' @aliases cleanUpGroup,BioData-method
#' @rdname cleanUpGroup-methods
#' @docType methods
#' @description 
#' @param x  TEXT MISSING
#' @param group  TEXT MISSING
#' @param otherGroup  TEXT MISSING
#' @title description of function cleanUpGroup
#' @export 
setGeneric('cleanUpGroup', ## Name
	function ( x, group, otherGroup ) { ## Argumente der generischen Funktion
		standardGeneric('cleanUpGroup') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('cleanUpGroup', signature = c ('BioData'),
	definition = function ( x, group, otherGroup ) {
	## get the main otherGroup for each group 
	r = lapply( levels(x$samples[, group]), 
			function(n) { 
				t = table(x$samples[, otherGroup][which(x$samples[, group] == n )])
				mainOther = names(t[which( t == max(t))])[1]
				groupIDs <- which(x$samples[, group] == n )
				OK = groupIDs[ which(x$samples[ groupIDs ,otherGroup ] ==  mainOther )] 
				if ( length(OK) > 10) {
					OK = sample(OK, 10)
				}
				OK = paste(collapse=";", OK )
				c( mainOther, n, OK)  
			} 
	)
	r= t(data.frame(r))
	rownames(r) = NULL
	OK = unlist( str_split( r[,3], ';'))
	OK = as.numeric(OK)
	OK = OK[is.na(OK) == F]
	OK = reduceTo(x, what='col', to=colnames(x$dat)[OK], name="OK" , copy=T)
	RFobj = bestGrouping(OK, group)
	x$samples[, group] = factor( predict( RFobj , t(as.matrix(x$data())) ), levels=levels(x$samples[, group]))
	print( paste("Grouping", group,"cleaned by grouping", otherGroup )) 
	invisible(x)
}  )
