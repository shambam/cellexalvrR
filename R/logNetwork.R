#' @name logNetwork
#' @aliases logNetwork,cellexalvrR-method
#' @rdname logNetwork-methods
#' @docType methods
#' @description create one Network page in the session report
#' @param cellexalObj the cellexalvrR object
#' @param genes the genes displayed on the network
#' @param png the VR generated network (png)
#' @param grouping the grouping file used to create this network
#' @param ... options you want to send to the ontologyLogPage() function
#' @title description of function logNetwork
#' @export
setGeneric('logNetwork', ## Name
	function ( cellexalObj, genes = NULL, png, grouping, ...  ) {
		standardGeneric('logNetwork')
	}
)

setMethod('logNetwork', signature = c ('character'),
		definition = function (cellexalObj, genes = NULL, png, grouping, ... ) {
			cellexalObj <- loadObject(cellexalObj)
			logNetwork(cellexalObj, genes, png, grouping, ... )
		}
)

setMethod('logNetwork', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes = NULL, png, grouping, ... ) {
	## almost the same page as in the logHeatmap function - including a GO analyis?
	cellexalObj = sessionPath(cellexalObj)
	sessionPath = cellexalObj@usedObj$sessionPath

	n = length( grep ( "Network.Rmd", list.files(sessionPath) ) )

	if ( ! file.exists( png) ) {
		stop(paste( "logNetwork the network png file can not be found!", 'png') )
	}
	file.copy(png, file.path( sessionPath , 'png', basename( png )) )
	figureF = file.path( 'png', basename( png ) )

	## now I need to create the 2D mds plots for the grouping
	cellexalObj = userGrouping(cellexalObj, grouping )
	gInfo = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup )

	## gInfo is a list with names grouping, mds, col and order
	# create a file containing the grouping info (and thereby color) and the mds info - do not create doubles

	mdsFiles = mdsPlots2D( cellexalObj, gInfo )

	# figureF, mdsFiles[1] and mdsFiles[2] do now need to be integrated into a Rmd file
	mainOfile = file.path(sessionPath, filename( c( n, "Network.Rmd") ) )
	fileConn<-file( mainOfile )

	writeLines(c(
					paste( "##", "Network for grouping", cellexalObj@usedObj$lastGroup  ),

					paste( "### Network map (from the VR process)"),
					paste("![](",figureF,")"),
					paste( "### 2D MDS", gInfo$mds, " dim 1,2"),
					paste("![](",mdsFiles[1],")"),
					paste( "### 2D MDS", gInfo$mds, " dim 2,3"),
					paste("![](",mdsFiles[2],")")
			), fileConn)

	close(fileConn)

	cellexalObj@usedObj$sessionRmdFiles = c( cellexalObj@usedObj$sessionRmdFiles, mainOfile)

	## if you give me a gene list here you will get a GO analysis ;-)
	if ( ! is.null(genes)){
		if ( file.exists(genes)) {
			genes = as.vector(read.delim(genes)[,1])
		}
		cellexalObj = ontologyLogPage(cellexalObj, genes, ... )
	}

	lockedSave(cellexalObj)

	cellexalObj
} )
