setGeneric('logNetwork', ## Name
	function ( cellexalObj, genes = NULL, png, grouping, ...  ) {
		standardGeneric('logNetwork')
	}
)
#' logNetwork is a VR helper funtion that stores one network in the log document.
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
setMethod('logNetwork', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes = NULL, png, grouping, ... ) {
	## almost the same page as in the logHeatmap function - including a GO analyis?

	## now I need to create the 2D drc plots for the grouping
	cellexalObj = userGrouping( cellexalObj, grouping )

	cellexalObj = sessionPath(cellexalObj)
	sessionPath = cellexalObj@usedObj$sessionPath

	if ( ! file.exists( png) ) {
		stop(paste( "logNetwork the network png file can not be found!", 'png') )
	}
	file.copy(png, file.path( sessionPath , 'png', basename( png )) )
	figureF = file.path( 'png', basename( png ) )

	## now I need to create the 2D drc plots for the grouping
	gInfo = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup )

	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles

	drcFiles = drcPlots2D( cellexalObj, gInfo )

	# figureF, drcFiles[1] and drcFiles[2] do now need to be integrated into a Rmd file
	#mainOfile = file.path(sessionPath, filename( c( n, "Network.Rmd") ) )
	#file.create(mainOfile)
	#fileConn<-file( mainOfile )
	mainOfile = cellexalObj@usedObj$sessionRmdFiles[1]

	max = 10
	i = 0
	while ( ! file.exists( mainOfile ) ){
	  Sys.sleep( 10)
	  i =  +1
	  if ( max == i )
	    break
	}

	cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )

	cat(
					paste( "##", "Network from Saved Selection", sessionCounter(  cellexalObj, cellexalObj@usedObj$lastGroup ) ),
					paste("This selection is available in the R object as group",cellexalObj@usedObj$lastGroup ),
					"",
					paste( "### Network map (from CellexalVR)"),
					paste("![](",figureF,")"),
					"",
					paste( "### 2D DRC", gInfo$drc, " dim 1,2"),
					paste("![](",drcFiles[1],")"),
					"",
					paste( "### 2D DRC", gInfo$drc, " dim 2,3"),
					paste("![](",drcFiles[2],")"),
					""
			, sep="\n", file = mainOfile, append= TRUE)

	#close(fileConn)

	cellexalObj@usedObj$sessionRmdFiles = c( cellexalObj@usedObj$sessionRmdFiles, mainOfile)

	## if you give me a gene list here you will get a GO analysis ;-)
	if ( ! is.null(genes)){
		if ( file.exists(genes)) {
			genes = as.vector(utils::read.delim(genes)[,1])
		}
		cellexalObj = ontologyLogPage(cellexalObj, genes, ... )
	}

	if ( ! file.exists(file.path(sessionPath, '..', "cellexalObj.RData") )){
		lockedSave(cellexalObj, file.path(sessionPath, '..') )
	}else {
		savePart(cellexalObj, 'usedObj' )
	}
	
	cellexalObj
} )


#' @describeIn logNetwork cellexalvrR
#' @docType methods
#' @description preload the cellexalObj.RData file
#' @param cellexalObj the cellexalObj.RData file
#' @param genes the genes displayed on the network
#' @param png the VR generated network (png)
#' @param grouping the grouping file used to create this network
#' @param ... options you want to send to the ontologyLogPage() function
#' @title description of function logNetwork
#' @export
setMethod('logNetwork', signature = c ('character'),
		definition = function (cellexalObj, genes = NULL, png, grouping, ... ) {
			cellexalObj <- loadObject(cellexalObj)
			logNetwork(cellexalObj, genes, png, grouping, ... )
		}
)
