#'Class defintion of cellexalvr
#' @exportClass cellexalvr
setClass("cellexalvr",slots=c(
    data="matrix",
    meta.cell="matrix",
    meta.gene="matrix",
    mds="list",
    index="matrix",
    tfs="vector"
    )   
)

#' @name show
#' @aliases show,cellexalvr-method
#' @rdname show-methods
#' @docType methods
#' @description  print the cellexalvr
#' @param x the cellexalvr object
#' @return nothing
#' @title description of function show
#' @export 
setMethod('show', signature(object='cellexalvr') ,
		definition = function (object) {
			cat (paste("An object of class", class(object)),"\n" )
			#cat("named ",object@name,"\n")
			cat (paste( 'with',nrow(object@data),'genes and', ncol(object@data),' samples.'),"\n")
			cat (paste("Annotation datasets (",paste(dim(object@meta.gene),collapse=','),"): '",paste( colnames(object@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
			cat (paste("Sample annotation (",paste(dim(object@meta.cell),collapse=','),"): '",paste( colnames(object@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
			if ( length(names(object@mds)) > 0 ){
				cat ( "and ", length(names(object@mds)), " mds object(s)\n")
			}
		}
)
