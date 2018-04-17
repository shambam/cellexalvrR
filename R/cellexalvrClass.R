#'Class defintion of cellexalvr
#' @exportClass cellexalvr
setClass("cellexalvr",slots=c(
    data="matrix",
    meta.cell="matrix", #test commit
    meta.gene="matrix",
	userGroups="data.frame", # whenever a user defined grouping is read from file we add one column
	colors="list", # the color information linked to all user groups
	groupSelectedFrom = 'list', # which mds rep has been used to create the grouping
	usedObj="list", # push all other objects you temporarily need here
    mds="list",
    index=c("matrix"),
    tfs="vector",
	specie="character"
    )   
)

##' @name print
##' @aliases print,cellexalvr-method
##' @rdname print-methods
##' @docType methods
##' @description  print the cellexalvr
##' @param x the cellexalvr object
##' @return nothing
##' @title description of function print
##' @export print
#setMethod('print', signature = c ('cellexalvr'),
#		definition = function (x) {
#			cat (paste("An object of class", class(x)),"\n" )
#			#cat("named ",x@name,"\n")
#			cat (paste( 'with',nrow(x@data),'genes and', ncol(x@data),' cells.'),"\n")
#			cat (paste("Annotation datasets (",paste(dim(x@meta.gene),collapse=','),"): '",paste( colnames(x@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
#			cat (paste("Sample annotation (",paste(dim(x@meta.cell),collapse=','),"): '",paste( colnames(x@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
#			cat ( paste("There are",(ncol(x@userGroups)/2), "user groups stored" ),"\n")
#			if ( length(names(x@mds)) > 0 ){
#				cat ( "and", length(names(x@mds)), "mds object(s)\n")
#			}
#			cat (paste("Specie is set to", x@specie),"\n")
#		}
#)

#' @name show
#' @aliases show,cellexalvr-method
#' @rdname show-methods
#' @docType methods
#' @description  shows the cellexalvr
#' @param x the cellexalvr object
#' @return nothing
#' @title description of function show
#' @export show
setMethod('show', signature = c ('cellexalvr'),
		definition = function (object) {
			cat (paste("An object of class", class(object)),"\n" )
			#cat("named ",x@name,"\n")
			cat (paste( 'with',nrow(object@data),'genes and', ncol(object@data),' cells.'),"\n")
			cat (paste("Annotation datasets (",paste(dim(object@meta.gene),collapse=','),"): '",paste( colnames(object@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
			cat (paste("Sample annotation (",paste(dim(object@meta.cell),collapse=','),"): '",paste( colnames(object@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
			cat ( paste("There are",length(grep('order', colnames(object@userGroups), invert=T)), "user groups stored" ),"\n")
			if ( length(names(object@mds)) > 0 ){
				cat ( "and", length(names(object@mds)), "mds object(s)\n")
			}
			cat (paste("Specie is set to", object@specie),"\n")
			
		}
)