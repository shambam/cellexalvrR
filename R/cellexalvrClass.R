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
#' @export show
setMethod('show', signature(object='cellexalvr') ,
		definition = function (object) {
			cat (paste("An object of class", class(object)),"\n" )
			#cat("named ",object@name,"\n")
			cat (paste( 'with',nrow(object@data),'genes and', ncol(object@data),' samples.'),"\n")
			cat (paste("Annotation datasets (",paste(dim(object@meta.gene),collapse=','),"): '",paste( colnames(object@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
			cat (paste("Sample annotation (",paste(dim(object@meta.cell),collapse=','),"): '",paste( colnames(object@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
			cat ( paste("Up to now I have",(ncol(object@userGroups)/2), "user groups stored" ),"\n")
			if ( length(names(object@mds)) > 0 ){
				cat ( "and ", length(names(object@mds)), " mds object(s)\n")
			}
		}
)
