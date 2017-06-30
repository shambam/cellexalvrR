#' @name reduceTo
#' @aliases reduceTo,cellexalvr-method
#' @rdname reduceTo-methods
#' @docType methods
#' @description The main reduction function can drop both meta.cel and genes using the colnames / rownames of the data tables
#' @param x the NGScollation object
#' @param what reduce to meta.cel or row ids default='row'
#' to select these names default=NULL
#' @param name the new name of this object (mainly unused)
#' @title description of function reduceTo
#' @export 
setGeneric('reduceTo', ## Name
    function ( cellexalvr, what='row', to=NULL, ... ) { ## Argumente der generischen Funktion
      standardGeneric('reduceTo') ## der Aufruf von standardGeneric sorgt für das Dispatching
    }
)

setMethod('reduceTo', signature = c ('cellexalvr'),
    definition = function ( cellexalvr, what='row', to=NULL, name='reduced', copy=FALSE ) {
      if ( copy ) { # only neccessary for the R6 class the function came from
		 cellexalvr <- cellexalvr$clone()
      }
      if ( ! is.null(to)) {
        if ( what =="row") {
          if ( length(which(is.na(match(to,rownames(cellexalvr$data)))==F)) > 0 ) {
            useOnly <- match(to, rownames(cellexalvr$data))
            not.matched <- to[is.na(useOnly)]
            if ( length(not.matched) > 0 ){
              print (paste('Problematic genes:', paste(not.matched,sep=', ')))
              to <- to[ ! is.na(useOnly)]
              useOnly <- useOnly[ ! is.na(useOnly) ]
            }
            #for (n in cellexalvr$drop){
            #  if ( ! is.null(x[[n]]) ) {
            #    x[[n]] <- NULL
            #  }
            #  if ( ! is.null(cellexalvr$usedObj[[n]]) ) {
            #    cellexalvr$usedObj[[n]] <- NULL
            #  }
            #}
            cellexalvr$data = cellexalvr$data[useOnly,]
            cellexalvr$meta.gene = cellexalvr$meta.gene[useOnly,]
            
            #if ( ! is.null( cellexalvr$raw) ) {
            #  cellexalvr$raw <- cellexalvr$raw[useOnly,]
            #}
            if ( length(cellexalvr$stats) > 0 ) {
              lapply( names(cellexalvr$stats) , function(name) {
                    cellexalvr$stats[[name]] = cellexalvr$stats[[name]][match(tolower(to) ,tolower(cellexalvr$stats[[name]][,1]) ),]
                  } )
            }
            cellexalvr$name = name
          }else {
            print (paste( "None of the probesets matched the probesets in the object -> keep everything!"))
          }
          
          
        }else if ( what =="col" ) {
          to <- tolower(make.names(to))
          if ( length(which(is.na(match(to,tolower(colnames(cellexalvr$data))))==F)) > 0 ) {
            useOnly <- match(to, tolower(colnames(cellexalvr$data)))
            not.matched <- to[is.na(useOnly)]
            if ( length(not.matched) > 0 ){
              print (paste('Problematic meta.cel:', paste(not.matched,sep=', ')))
              to <- to[ ! is.na(useOnly)]
              useOnly <- useOnly[ ! is.na(useOnly) ]
            }
            #for (n in cellexalvr$drop){
            #  if ( ! is.null(x[[n]]) ) {
            #    x[[n]] <- NULL
            #  }
            #  if ( ! is.null(cellexalvr$usedObj[[n]]) ) {
            #    cellexalvr$usedObj[[n]] <- NULL
            #  }
            #}
            cellexalvr$data = cellexalvr$data[,useOnly]
            cellexalvr$meta.cell = cellexalvr$meta.cell[useOnly,]
            
            #if ( ! is.null( cellexalvr$raw) ) {
            #  cellexalvr$raw <- cellexalvr$raw[,useOnly]
            #}
            if ( length(cellexalvr$stats) > 0 ) {
              cellexalvr$stats = list()
            }
            cellexalvr$name = name
            
          }else {
            print (paste( "None of the names (to) matched the sample names in object -> keep everything!"))
          }
        }else {
          stop(paste( "the option what='",what,"' is not supported!", sep="") )
        }
      }
      invisible(cellexalvr)
    } )
