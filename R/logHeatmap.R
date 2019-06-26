
setGeneric('logHeatmap', ## Name
	function ( cellexalObj, genes, png, grouping, ... ) {
		standardGeneric('logHeatmap')
	}
)
#' logHeatmap will create a section in the log document including 
#' (1) the DRC the grouping was selected from (colored 2D)
#' (2) the heatmap itself
#' (3) a GO analysis of the genes displayed in the heatmap (using ontologyLogPage())
#' @name logHeatmap
#' @aliases logHeatmap,cellexalvrR-method
#' @rdname logHeatmap-methods
#' @docType methods
#' @description preload the object before creating one Heatmap session report page
#' @param cellexalObj the cellexalvrR object
#' @param genes the genes displayed on the heatmap
#' @param png the VR generated heatmap (png)
#' @param grouping the grouping file used to create this heatmap
#' @param ... options you want to send to the ontologyLogPage() function
#' @title description of function logHeatmap
#' @export 
setMethod('logHeatmap', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes = NULL, png, grouping, ...  ) {
	## here I need to create a page of the final log

	if ( !is.null(genes)){
		if ( file.exists(genes)) {
			genes = as.vector(utils::read.delim(genes)[,1])
		}
	}

	cellexalObj = userGrouping( cellexalObj, grouping )

	cellexalObj = sessionPath( cellexalObj )
	sessionPath = cellexalObj@usedObj$sessionPath

	cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
	n = sessionCounter( cellexalObj, cellexalObj@usedObj$lastGroup )

	if ( ! file.exists( png) ) {
		stop(paste( "logHeatmap the heatmap png file can not be found!", png ) )
	}
	file.copy(png, file.path( sessionPath , 'png', basename( png ) ) )
	figureF = file.path( 'png', basename( png ) )

	## now I need to create the 2D drc plots for the grouping
	#cellexalObj = userGrouping(cellexalObj, grouping )
	gInfo = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup )

	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles

	drcFiles = drcPlots2D( cellexalObj, gInfo )

	# figureF, drcFiles[1] and drcFiles[2] do now need to be integrated into a Rmd file
	#mainOfile = file.path( sessionPath, filename( c( n, "Heatmap.Rmd") ) )
	#file.create(mainOfile)
	mainOfile = cellexalObj@usedObj$sessionRmdFiles[1]

	max = 10
	i = 0
	 while ( ! file.exists(cellexalObj@usedObj$sessionRmdFiles[1])){
	  Sys.sleep( 10)
	  i =  +1
	  if ( max == i ){
		  message( paste( "An important log file is missing", cellexalObj@usedObj$sessionRmdFiles[1] ))
		  break
	  }
	 }

	cat( sep="\n",
		paste( "##", "Heatmap from Saved Selection ", n  ),
		paste("This selection is available in the R object as group",cellexalObj@usedObj$lastGroup ),
		"",
		paste( "### Genes"),
		paste( collapse=" ", unlist( lapply(sort(genes), function(n) { rmdLink(n, "https://www.genecards.org/cgi-bin/carddisp.pl?gene=")  })) ),
		'',
		paste( "### Heatmap (from CellexalVR)"),
		paste("![](",figureF,")"),
		'',
		paste( "### 2D DRC", gInfo$drc, " dim 1,2"),
		paste("![](",drcFiles[1],")"),
		'',
		paste( "### 2D DRC", gInfo$drc, " dim 2,3"),
		paste("![](",drcFiles[2],")"),
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
	if ( ! file.exists(file.path(sessionPath, '..', "cellexalObj.RData") )){
		lockedSave(cellexalObj, file.path(sessionPath, '..') )
	}else {
		savePart(cellexalObj, 'usedObj' )
	}
	

	cellexalObj
} )

#' @describeIn logHeatmap cellexalvrR
#' @description create one Heatmap session report page
#' @param cellexalObj the cellexalvrR file
#' @param genes the genes displayed on the heatmap
#' @param png the VR generated heatmap (png)
#' @param grouping the grouping file used to create this heatmap
#' @param ... options you want to send to the ontologyLogPage() function
#' @title description of function logHeatmap
#' @export
setMethod('logHeatmap', signature = c ('character'),
		definition = function (cellexalObj, genes, png, grouping, ... ) {
			cellexalObj <- loadObject(cellexalObj)
			logHeatmap(cellexalObj, genes, png, grouping, ... )
		}
)
