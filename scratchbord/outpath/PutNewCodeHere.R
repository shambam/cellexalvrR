#' @name Split4Geo
#' @aliases Split4Geo,BioData-method
#' @rdname Split4Geo-methods
#' @docType methods
#' @description 
#' @param x  TEXT MISSING
#' @param opath  TEXT MISSING default= x$outpath
#' @title description of function Split4Geo
#' @export 
setGeneric('Split4Geo', ## Name
	function (x, opath = x$outpath ) { ## Argumente der generischen Funktion
		standardGeneric('Split4Geo') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('Split4Geo', signature = c ('BioData'),
	definition = function (x, opath = x$outpath ) {
	if ( ! file.exists( file.path(opath, 'md5sums.txt'))) {
		system(paste( "touch", file.path(opath, 'md5sums.txt') ))
	}
	for ( i in 1:ncol(x$data) ) {
		d <- cbind( rownames(x$data), x$data[,i] )
		colnames(d) <- c( x$rownamescol, colnames(x$data)[i] )
		write.table( d, file= file.path( opath, paste(colnames(x$data)[i], 'xls',sep='.' )), sep="\t",quote=F, row.names=F )
		system( paste("md5sum", file.path( opath, paste(colnames(x$data)[i], 'xls',sep='.' )) , " >>", file.path(opath, 'md5sums.txt') ) )
	}
	invisible(x)
} )
