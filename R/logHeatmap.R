#' @name logHeatmap
#' @aliases logHeatmap,cellexalvrR-method
#' @rdname logHeatmap-methods
#' @docType methods
#' @description create one Heatmap session report page
#' @param cellexalObj teh cellexalvrR object
#' @param genes the genes displayed on the heatmap
#' @param png the VR generated heatmap (png)
#' @param grouping the grouping file used to create this heatmap
#' @param ... options you want to send to the ontologyLogPage() function
#' @title description of function logHeatmap
#' @export
setGeneric('logHeatmap', ## Name
	function ( cellexalObj, genes, png, grouping, ... ) {
		standardGeneric('logHeatmap')
	}
)

setMethod('logHeatmap', signature = c ('character'),
		definition = function (cellexalObj, genes, png, grouping, ... ) {
			cellexalObj <- loadObject(cellexalObj)
			logHeatmap(cellexalObj, genes, png, grouping, ... )
		}
)

setMethod('logHeatmap', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes = NULL, png, grouping, ...  ) {
	## here I need to create a page of the final log

	if ( !is.null(genes)){
		if ( file.exists(genes)) {
			genes = as.vector(read.delim(genes)[,1])
		}
	}

	cellexalObj = userGrouping( cellexalObj, grouping )

	cellexalObj = sessionPath(cellexalObj )
	sessionPath = cellexalObj@usedObj$sessionPath

	cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
	n = sessionCounter(  cellexalObj, cellexalObj@usedObj$lastGroup )

	if ( ! file.exists( png) ) {
		stop(paste( "logHeatmap the heatmap png file can not be found!", png ) )
	}
	file.copy(png, file.path( sessionPath , 'png', basename( png ) ) )
	figureF = file.path( 'png', basename( png ) )

	## now I need to create the 2D mds plots for the grouping
	#cellexalObj = userGrouping(cellexalObj, grouping )
	gInfo = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup )

	## gInfo is a list with names grouping, mds, col and order
	# create a file containing the grouping info (and thereby color) and the mds info - do not create doubles

	mdsFiles = mdsPlots2D( cellexalObj, gInfo )

	# figureF, mdsFiles[1] and mdsFiles[2] do now need to be integrated into a Rmd file
	#mainOfile = file.path( sessionPath, filename( c( n, "Heatmap.Rmd") ) )
	#file.create(mainOfile)
	mainOfile = cellexalObj@usedObj$sessionRmdFiles[1]

	max = 10
	i = 0
	 while ( ! file.exists(cellexalObj@usedObj$sessionRmdFiles[1])){
	  Sys.sleep( 10)
	  i =  +1
	  if ( max == i )
	    last
	 }

	cat( sep="\n",
		paste( "##", "Heatmap from Saved Selection ", n  ),
		paste("This selection is available in the R object as group",cellexalObj@usedObj$lastGroup ),
		"",
		paste( "### Genes"),
		paste( collapse=" ", unlist( lapply(sort(genes), function(n) { rmdLink(n, "https://www.genecards.org/cgi-bin/carddisp.pl?gene=")  })) ),
		'',
		paste( "### Heatmap (from the cellexalVR process)"),
		paste("![](",figureF,")"),
		'',
		paste( "### 2D MDS", gInfo$mds, " dim 1,2"),
		paste("![](",mdsFiles[1],")"),
		'',
		paste( "### 2D MDS", gInfo$mds, " dim 2,3"),
		paste("![](",mdsFiles[2],")"),
		""
		, file = mainOfile, append = TRUE)

	cellexalObj@usedObj$sessionRmdFiles = c( cellexalObj@usedObj$sessionRmdFiles, mainOfile)

	## an entry in the annotation gene lists and a GO ontology page for this gene list
	#if ( ! is.null(genes)){
	#  if ( file.exists(genes)) {
	#    genes = as.vector(read.delim(genes)[,1])
	#  }
	#  cellexalObj = ontologyLogPage(cellexalObj, genes, ... )
	#}

	lockedSave(cellexalObj, file.path(sessionPath, '..') )

	cellexalObj
} )
