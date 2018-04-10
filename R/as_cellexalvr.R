#' @name as_cellexalvr
#' @aliases as_cellexalvr,cellexalvr-method
#' @rdname as_cellexalvr-methods
#' @docType methods
#' @description Convert any object into an S4 cellexalvr object
#' @param x the other object
#' @title description of function as_cellexalvr
#' @export 
setGeneric('as_cellexalvr', ## Name
		function ( x, cellInfo, ... ) { ## Argumente der generischen Funktion
			standardGeneric('as_cellexalvr') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)
#' @name as_cellexalvr.BioData
#' @aliases as_cellexalvr.BioData,cellexalvr-method
#' @rdname as_cellexalvr-methods
#' @docType methods
#' @description Convert a R6::BioData object into a R6::cellexalvr object
#' @param x the BioData object
#' @param cellInfo the samples columns containing 0/1 info for the meta.cells matrix
#' @param groups the R calculated groupings to go into the userGroups matrix
#' @param linear linear sample information that is to be stored in the facs matrix
#' @title description of function as_cellexalvr
#' @export 
setMethod('as_cellexalvr', signature = c ('SingleCells'),
		definition = function ( x, cellInfo, groups=NULL, linear=NULL  ) {
			if ( is.null(x$usedObj$MDS) ){
				x$usedObj$MDS <- list()
			}
			lapply( names(x$usedObj$MDS), function(n){	rownames(x$usedObj$MDS[[n]]) = colnames(x$dat) } )
			meta.cell = NULL
			for( name in cellInfo) {
				meta.cell <- rbind(meta.cell, to.matrix(as.vector(x$samples[,name]), levels(x$samples[,name]) ) )
			}
			rownames(meta.cell) = colnames(x$dat)
			
			userGroups = NULL
			if ( ! is.null(groups) ) {
				userGroups = x$samples[,groups]
				rownames(userGroups) = colnames(x$dat)
			}
			
			index = NULL
			if ( !is.null(linear) ) {
				index = as.matrix(x$samples[,linear])
				rownames(index) = colnames(x$dat)
			}
			ret = new( 'cellexalvr',
					data = as.matrix(x$dat),
					meta.cell= meta.cell, 
					userGroups= userGroups,
					meta.gene= as.matrix(x$annotation), 
					mds=x$usedObj$MDS,
					index = index
			)
			ret
		}
)

cellexalvr <- as_cellexalvr(data, cellInfo = c('sname'), groups = c('nUMI_bins', 'RFgrouping GO_all 1 n=25'), linear = c('nUMI') )
