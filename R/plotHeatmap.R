#' Plot a simple violin plot using the cellexalVR colors
#'
#' @name plotHeatmap
#' @aliases plotHeatmap,cellexalvrR-method
#' @rdname plotHeatmap-methods
#' @docType methods
#' @description Create a simple violin plot on the expresion of one gene
#' @param genes  the genes to include into the heatmap (correct order!)
#' @param groupings either columns in the userGroups table
#'                  or a list of CellexalVR selection files or a mix of both
#' @param ofile the figure out file (or null)
#' @param width default figure width (9 in)
#' @param height default figure height (9 in)
#' @param main the figure titla (defaults to gene name)
#' @param X11type not important here (default to 'cairo')
#' @param family pdf typing family (defaults to "Helvetica") png is also supported
#' @fileType figure file type (default 'pdf' )
#' @title description of function plotHeatmap
#' @export 
setGeneric('plotHeatmap', ## Name
	function (x, genes, groupings, ofile=NULL, 
	width=9, height=9, main=NULL, X11type= 'cairo', family="Helvetica", fileType='pdf'  ) { ## Argumente der generischen Funktion
		standardGeneric('plotHeatmap') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('plotHeatmap', signature = c ('cellexalvrR'),
	definition <- function(x, genes, groupings, ofile=NULL, width=9, height=9, main=NULL, X11type= 'cairo', family="Helvetica"  ) {

	x <- loadObject(x) #function definition in file 'lockedSave.R'
	x <- userGrouping(x, grouping) #function definition in file 'userGrouping.R'

	dendrogram= 'none'

	x=reduceTo( x, what='row', to=genes )
	data <- as.matrix(x@data)
	m <- min(data)

	brks <- unique(
		as.vector(c(m, 
		stats::quantile(data[which(data!= m)],seq(0,1,by=1/brks)),
		max(data)))
	)
	
	heapmapCols = function(x){ c("black", gplots::bluered(x))}
	
	
	if ( is.null(main) ){
		main = gene
	}
	ok <- which(!is.na(x@userGroups[,x@usedObj$lastGroup]))
	if ( length(ok) > 0) {
		loc <- reduceTo (x, what='col', to=colnames(x@data)[ ok ] ) #function definition in file 'reduceTo.R'
	}else {
		loc <- x
	}
	nam= unique( loc@userGroups[,x@usedObj$lastGroup])
	data = lapply( nam , 
		function( name ){
			loc@data[gene, which(loc@userGroups[,x@usedObj$lastGroup] == name)]
		} )
	names(data)[0] = 'x'
	col = NULL
	for (name in nam) {
		col = c(col, x@colors[[x@usedObj$lastGroup]][name])
	}
	if ( !is.null(ofile) ) {
		if ( fileType == 'pdf'){
			grDevices::pdf( file=paste(ofile ,'pdf',sep='.'), width=width, height=height, family=family)
		}	
	}
	vioplot( data, names=nam, col=col, main=main )
	if ( !is.null(ofile) ){
		grDevices::dev.off()
	}
	message('Done')
	invisible(x)
} 
)

