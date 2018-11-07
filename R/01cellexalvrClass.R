#' @name cellexalvrR
#' @title cellexalvrR class definition
#' @description  The R backend for the CellexalVR 3D application
#' @slot data the expression matrix
#' @slot meta.cell the cell level meta information (matrix)
#' @slot meta.gene the gene level meta information (matrix)
#' @slot userGroups internally used to store the user defined groupings from the 3D process (data.frame)
#' @slot colors a list for each userGroups entry defining the color sheme for this grouping
#' @slot mds a list of all mds objects to be visible in the 3D application
#' @slot index a matrix for FACS or other linear numeric data that should be available for colouring in the 3D application
#' @slot tfs depricated not used any more
#' @slot specie the species this data is from (mouse or human)
#' @slot outpath the path this object will be saved to
#' @exportClass cellexalvrR
setClass(Class="cellexalvrR",
		representation=representation(
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
				specie="character",
				outpath="character"
		),
		prototype(
				data=matrix(),
				meta.cell=matrix(),
				meta.gene=matrix(),
				userGroups= data.frame(),
				colors=list(),
				groupSelectedFrom=list(),
				usedObj=list(),
				mds=list(),
				index=matrix(),
				tfs=NA_character_,
				specie=NA_character_,
				outpath=NA_character_
		)
)
#' @name print
#' @aliases print,cellexalvr-method
#' @rdname print-methods
#' @docType methods
#' @description  print the cellexalvr
#' @param x the cellexalvr object
#' @return nothing
#' @title description of function print
#' @export print
setMethod('print', signature = c ('cellexalvrR'),
		definition = function (x) {
			show(x)
		} )

#' @name show
#' @aliases show,cellexalvrR-method
#' @rdname show-methods
#' @docType methods
#' @description  shows the cellexalvr contents
#' @param x the cellexalvr object
#' @return nothing
#' @title description of function show
#' @export show
setMethod('show', signature = c ('cellexalvrR'),
		definition = function (object) {
			cat (paste("An object of class", class(object)),"\n" )
			#cat("named ",x@name,"\n")
			cat (paste( 'with',nrow(object@data),'genes and', ncol(object@data),' cells.'),"\n")
			cat (paste("Annotation datasets (",paste(dim(object@meta.gene),collapse=','),"): '",paste( colnames(object@meta.gene ), collapse="', '"),"'  ",sep='' ),"\n")
			cat (paste("Sample annotation (",paste(dim(object@meta.cell),collapse=','),"): '",paste( colnames(object@meta.cell ), collapse="', '"),"'  ",sep='' ),"\n")
			cat ( paste("There are",length(grep('order', colnames(object@userGroups), invert=T)), "user groups stored" ),":\n")
			if ( ncol(object@userGroups) > 0 ) {
				cat ( paste( collapse=", ", colnames(object@userGroups)), '\n' )
			}
			if ( length(names(object@mds)) > 0 ){
				cat ( "and", length(names(object@mds)), "mds object(s)\n")
			}
			cat (paste("Specie is set to", object@specie),"\n")
			
		}
)

#' @name cellexalvr
#' @title cellexalvr class definition (old)
#' @exportClass cellexalvr
setClass( 
		Class='cellexalvr', 
		representation = representation ( 
		##	NGS = 'binary'
		),
		contains='cellexalvrR'
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
