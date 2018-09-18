#' @name reduceTo
#' @aliases reduceTo,cellexalvr-method
#' @rdname reduceTo-methods
#' @docType methods
#' @description The main reduction function can drop both samples and genes using the colnames / rownames of the data tables
#' @param x the NGScollation object
#' @param what reduce to samples or row ids default='row'
#' @param to select these names default=NULL
#' @title description of function reduceTo
#' @export reduceTo

reduceTo <- function ( x, what='row', to=NULL ) {

			if (nrow(x@meta.gene)==0) {
				x@meta.gene <- matrix(ncol=2, c(rownames(x@data), rep( 0, nrow(x@data)) ) )
				colnames(x@meta.gene) = c('Gene Symbol', 'useless')
				rownames(x@meta.gene) = rownames(x@data)
			}
			if ( ! is.null(to)) {
				if ( what =="row") {
					if ( length(which(is.na(match(to,rownames(x@data)))==F)) > 0 ) {
						useOnly <- match(to, rownames(x@data))
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
						x@data <- x@data[useOnly,]
						x@meta.gene <- x@meta.gene[useOnly,]
						
					}else {
						print (paste( "None of the probesets matched the probesets in the cellexalvr object -> keep everything!"))
					}
					
					
				}else if ( what =="col" ) {
					to <- tolower(to)
					if ( length(which(is.na(match(to,tolower(colnames(x@data))))==F)) > 0 ) {
						useOnly <- match(to, tolower(colnames(x@data)))
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
						x@data <- x@data[,useOnly]
						x@meta.cell <- x@meta.cell[useOnly,]
						x@userGroups <- x@userGroups[useOnly,]
												
					}else {
						print (paste( "None of the names (to) matched the sample names in the cellexalvr object -> keep everything!"))
					}
				}else {
					stop(paste( "the option what='",what,"' is not supported!", sep="") )
				}
			}
			invisible(x)
		} 
		