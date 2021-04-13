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
#' @param fileType figure file type (default 'pdf' )
#' @param brks the amoiunt of colors for expression (default 10)
#' @title description of function plotHeatmap
#' @export 
setGeneric('plotHeatmap', ## Name
	function (x, genes, groupings, ofile=NULL, 
	width=9, height=9, main=NULL, X11type= 'cairo', family="Helvetica", fileType='pdf', brks=10  ) { ## Argumente der generischen Funktion
		standardGeneric('plotHeatmap') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('plotHeatmap', signature = c ('cellexalvrR'),
	definition <- function(x, genes, groupings, ofile=NULL, width=9, height=9, 
		main=NULL, X11type= 'cairo', family="Helvetica", brks=10  ) {

		if (! require('pheatmap')) {
			stop( "the pheatmap library is required for this")
		}
	x <- loadObject(x) #function definition in file 'lockedSave.R'
	x <- userGrouping(x, groupings[1]) #function definition in file 'userGrouping.R'

	dendrogram= 'none'

	x=reduceTo( x, what='row', to=genes )
	x=reduceTo( x, what='col', to=colnames(x@data)[
		which( !is.na(x@userGroups[,x@usedObj$lastGroup] ))
		])
	x = reorder.samples(x, paste(x@usedObj$lastGroup, 'order') )
	data <- as.matrix(x@data)
	m <- min(data)

	brks <- unique(
		as.vector(c(m, 
		stats::quantile(data[which(data!= m)],seq(0,1,by=1/brks)),
		max(data)))
	)
	
	heapmapCols = function(x){ c("black", gplots::bluered(x))}
	
	col = x@colors[[x@usedObj$lastGroup]]
	col= list( col[which(!is.na(col))] )
	names(col) = x@usedObj$lastGroup

	df = data.frame( x@userGroups[,x@usedObj$lastGroup] )
	colnames(df) = x@usedObj$lastGroup
	rownames(df) = colnames(x@data)

	if ( length(groupings) > 1) {
		df = x@userGroups[,c(x@usedObj$lastGroup, groupings[-1])]
		## need to take care of the colors
		browser()
	}

	if ( is.null(main) ){
		main = x@usedObj$lastGroup
	}

	if ( !is.null(ofile) ) {
		if ( fileType == 'pdf'){
			grDevices::pdf( file=paste(ofile ,'pdf',sep='.'), width=width, height=height, family=family)
		}	
	}
	if ( ncol(data) = 1000 ) {
		ncells =ceiling(ncol(data)/1000)
		warning(paste(
			"Data is collapsed into 1000 summary samples",
			"based on selection order using n=", ncells,"per sample"
			) )
		counts = table(df[,1])
		ids = NULL
		for ( id in names(counts) ) {
			total = floor( counts[id] / ncells)
			thisids = rep( 1:total,ncells )
			thisids = c(thisids, sample( 1:total, counts[id] - length(thisids)  ))
			if( !is.null(ids) ) {
				thisids= max(ids) + thisids
			}
			thisids= sort(thisids)
			ids = c(ids, thisids)
		}
		data = collapse( x@data, ids, 2 ) # collapse by mean
		df = 
	}
	pheatmap( mat = x@data, kmeans_k = length(col) *3,
	 annotation_col = df, scale='none', cluster_rows=TRUE,
	 cluster_cols=FALSE, annotation_colors = col )

	if ( !is.null(ofile) ){
		grDevices::dev.off()
	}
	message('Done')
	invisible(x)
} 
)

