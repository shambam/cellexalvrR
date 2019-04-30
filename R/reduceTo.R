#' @name reduceTo
#' @aliases reduceTo,cellexalvrR-method
#' @rdname reduceTo-methods
#' @docType methods
#' @description  The main reduction function can drop both samples and genes using the colnames /
#' @description  rownames of the data tables
#' @param x the cellexalvrR object
#' @param what reduce to samples or row ids default='row'
#' @param to select these names default=NULL
#' @title description of function reduceTo
#' @export reduceTo
if ( ! isGeneric('reduceTo') ){setGeneric('reduceTo', ## Name
	function ( x, what='row', to=NULL ) { 
		standardGeneric('reduceTo') 
	}
) }

setMethod('reduceTo', signature = c ('cellexalvrR'),
	definition = function ( x, what='row', to=NULL ) {

			if (nrow(x@meta.gene) < nrow(x@dat)) {
				x@meta.gene <- matrix(ncol=2, c(rownames(x@dat), rep( 0, nrow(x@dat)) ) )
				colnames(x@meta.gene) = c('Gene Symbol', 'useless')
				rownames(x@meta.gene) = rownames(x@dat)
			}
			if ( ! is.null(to)) {
				if ( what =="row") {
					if ( length(which(is.na(match(to,rownames(x@dat)))==F)) > 0 ) {
						useOnly <- match(to, rownames(x@dat))
						not.matched <- to[is.na(useOnly)]
						if ( length(not.matched) > 0 ){
							print (paste('Problematic genes:', paste(not.matched,sep=', ')))
							to <- to[ ! is.na(useOnly)]
							useOnly <- useOnly[ ! is.na(useOnly) ]
						}
#						for (n in x@drop){
#							if ( ! is.null(x[[n]]) ) {
#								x[[n]] <- NULL
#							}
#							if ( ! is.null(x@usedObj[[n]]) ) {
#								x@usedObj[[n]] <- NULL
#							}
#						}
						x@dat <- x@dat[useOnly,]
						x@meta.gene <- x@meta.gene[useOnly,]
						
					}else {
						print (paste( "None of the probesets matched the probesets in the cellexalvr object -> keep everything!"))
					}
					
					
				}else if ( what =="col" ) {
					to <- tolower(to)
					if ( length(which(is.na(match(to,tolower(colnames(x@dat))))==F)) > 0 ) {
						useOnly <- match(to, tolower(colnames(x@dat)))
						not.matched <- to[is.na(useOnly)]
						if ( length(not.matched) > 0 ){
							print (paste('Problematic samples:', paste(not.matched,sep=', ')))
							to <- to[ ! is.na(useOnly)]
							useOnly <- useOnly[ ! is.na(useOnly) ]
						}
#						for (n in x@drop){
#							if ( ! is.null(x[[n]]) ) {
#								x[[n]] <- NULL
#							}
#							if ( ! is.null(x@usedObj[[n]]) ) {
#								x@usedObj[[n]] <- NULL
#							}
#						}
						n = ncol(x@dat)
						x@dat <- x@dat[,useOnly]
						if ( nrow(x@meta.cell) == n ){
							x@meta.cell <- x@meta.cell[useOnly,]
						}
						if ( nrow(x@userGroups) == n){
							x@userGroups <- x@userGroups[useOnly,]
						}
												
					}else {
						print (paste( "None of the names (to) matched the sample names in the cellexalvr object -> keep everything!"))
					}
				}else {
					stop(paste( "the option what='",what,"' is not supported!", sep="") )
				}
			}
			invisible(x)
		}  )
