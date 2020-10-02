
setGeneric('logHeatmap', ## Name
	function ( cellexalObj, genes, png, grouping, ... ) {
		standardGeneric('logHeatmap')
	}
)
#' logHeatmap will create a section in the log document including 
#' (1) the DRC the grouping was selected from (colored 2D)
#' (2) the heatmap itself
#' (3) a GO analysis of the genes displayed in the heatmap (using ontologyLogPage()) #function definition in file 'ontologyLogPage.R'
#' @name logHeatmap
#' @aliases logHeatmap,cellexalvrR-method
#' @rdname logHeatmap-methods
#' @docType methods
#' @description preload the object before creating one Heatmap session report page
#' @param cellexalObj the cellexalvrR object
#' @param genes the genes displayed on the heatmap
#' @param png the VR generated heatmap (png)
#' @param grouping the grouping file used to create this heatmap
#' @param ... options you want to send to the ontologyLogPage() function #function definition in file 'ontologyLogPage.R'
#' @title description of function logHeatmap
#' @export 
setMethod('logHeatmap', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes = NULL, png, grouping, ...  ) {
	## here I need to create a page of the final log

	for ( group in names( cellexalObj@groupSelectedFrom ) ){
		if( ! is.null(cellexalObj@groupSelectedFrom[[group]][["heatmapBasename"]]) ){
			if (cellexalObj@groupSelectedFrom[[group]][["heatmapBasename"]] == basename(png) ){
				grouping = group
				break;
			}
		}
	}
	if ( !is.null(genes)){
		if ( file.exists(genes)) {
			genes = as.vector(utils::read.delim(genes)[,1])
		}
	}

	if ( ! file.exists( png) ) {
		stop(paste( "logHeatmap the heatmap png file can not be found!", png ) )
	}
	if ( file.exists( grouping)) {
		cellexalObj = userGrouping( cellexalObj, grouping ) #function definition in file 'userGrouping.R'
	}
	cellexalObj = sessionPath( cellexalObj ) #function definition in file 'sessionPath.R'
	sessionPath = cellexalObj@usedObj$sessionPath

	## the grouping needs to be matched with the heatmap
	base = unlist(strsplit( png, '_' ))
	base = paste( paste( collapse="_",base[-length(base)] ), sep=".", 'txt')

	for ( i in 1:length( cellexalObj@groupSelectedFrom ) ) {
		if ( class( cellexalObj@groupSelectedFrom[[i]]) != 'list') {
			next
		}
		if ( ! is.null( cellexalObj@groupSelectedFrom[[i]]$heatmap2selection) ){
			if ( cellexalObj@groupSelectedFrom[[i]]$heatmap2selection == basename(base) ) {
				grouping = cellexalObj@groupSelectedFrom[[i]]$gname
				break
			}
		}else if ( cellexalObj@groupSelectedFrom[[i]]$selectionFile == basename(grouping)){
			cellexalObj@groupSelectedFrom[[i]]$heatmap2selection = basename(base)
			grouping = cellexalObj@groupSelectedFrom[[i]]$gname
			break
		}
	}
	cellexalObj = sessionRegisterGrouping( cellexalObj, grouping ) #function definition in file 'sessionRegisterGrouping.R'
	n = sessionCounter( cellexalObj, grouping ) #function definition in file 'sessionCounter.R'

	figureF = file.path( sessionPath , 'png', basename( png ) )
	file.copy(png, figureF )

	## in order to restore this heatmap in a new VR session I now need to store the database and gene list, too
	
	if ( ! file.exists( file.path( sessionPath , 'Heatmap') ) ) {
		dir.create( file.path( sessionPath , 'Heatmap') )
	}
	file.copy(base, file.path( sessionPath , 'Heatmap', basename( base ) ) )
	file.copy(paste(base, sep=".", 'sqlite3'), file.path( sessionPath , 'Heatmap', basename( base ) ) )

	## now I need to create the 2D drc plots for the grouping
	#cellexalObj = userGrouping(cellexalObj, grouping ) #function definition in file 'userGrouping.R'
	
	gInfo = groupingInfo( cellexalObj, cellexalObj@usedObj$lastGroup ) #function definition in file 'groupingInfo.R'

    correctPath = function(f) { file.path(cellexalObj@usedObj$sessionName, 'png', basename(f)) }
	## gInfo is a list with names grouping, drc, col and order
	# create a file containing the grouping info (and thereby color) and the drc info - do not create doubles
	drcFiles =sapply( drcPlots2D( cellexalObj, gInfo ), correctPath) #function definition in file 'drcPlot2D.R'

	cellCount = table(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup])
	R_IDs = names(cellCount)
	OK = which(!is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
	tab = cellexalObj@userGroups[OK,]
	tab = tab[order( tab[,paste( cellexalObj@usedObj$lastGroup, 'order')]) ,]
	tab = tab[ match( R_IDs,as.vector(tab[,cellexalObj@usedObj$lastGroup] ) ),]
	tab =  tab[order( as.numeric(tab[,paste(cellexalObj@usedObj$lastGroup, 'order')])),]
		tableHTML = paste( sep="\n",
		"### group information table",'',
		'<table>',
		'  <tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th><th>VR ID</th><th>R ID</th></tr>',
		paste(collapse="\n",
			sapply( as.vector(tab[,cellexalObj@usedObj$lastGroup]), function(id){
			paste(sep="",
				'<tr><td style="background-color:', 
				cellexalObj@colors[[cellexalObj@usedObj$lastGroup]][id],'"',
				"></td><td>",
				cellexalObj@colors[[cellexalObj@usedObj$lastGroup]][id],"</td><td>",
				cellCount[match(id, names(cellCount))], "</td><td>",id-1,"</td><td>",id,"</td></tr>"
				)
			}))
		, '</table> '
	)

		figureF = correctPath(figureF)
	content = paste( sep="\n",
		paste( "##", "Heatmap from Saved Selection ", n  ),
		paste("This selection is available in the R object as group",cellexalObj@usedObj$lastGroup ),
		"",
		paste( "### Genes"),
		paste( collapse=" ", unlist( lapply(sort(genes), function(n) { rmdLink(n, "https://www.genecards.org/cgi-bin/carddisp.pl?gene=")  })) ), #function definition in file 'rmdLink.R'
		'',
		paste( "### Heatmap (from CellexalVR)"),
		paste("![](",figureF,")"),
		'',
		tableHTML,'',
		paste( "### 2D DRC", gInfo$drc, " dim 1,2"),
		paste("![](",drcFiles[1],")"),
		'',
		paste( "### 2D DRC", gInfo$drc, " dim 2,3"),
		paste("![](",drcFiles[2],")"),
		"",
		"The heatmap can be restored in a new VR session using the 2D console (F12) and type:",
		"",
		paste("lsf", R.utils::getRelativePath(gInfo$selectionFile,
			relativeTo= file.path(cellexalObj@outpath,'..','..'), caseSensitive=T) ),
		"",
		"confirm", 
		"",
		paste( "rsf", R.utils::getRelativePath( base, 
			relativeTo=file.path(cellexalObj@outpath,'..','..'), caseSensitive=T) )

	)

	cellexalObj = storeLogContents( cellexalObj, content, type='Heatmap')
	id = length(cellexalObj@usedObj$sessionRmdFiles)
	cellexalObj = renderFile( cellexalObj, id, type='Heatmap' )

	if ( ! file.exists(file.path(sessionPath, '..', "cellexalObj.RData") )){
		lockedSave(cellexalObj, file.path(sessionPath, '..') ) #function definition in file 'lockedSave.R'
	}else {
		savePart(cellexalObj, 'usedObj' ) #function definition in file 'integrateParts.R'
	}
	
	cellexalObj
} )

#' @describeIn logHeatmap cellexalvrR
#' @description create one Heatmap session report page
#' @param cellexalObj the cellexalvrR file
#' @param genes the genes displayed on the heatmap
#' @param png the VR generated heatmap (png)
#' @param grouping the grouping file used to create this heatmap
#' @param ... options you want to send to the ontologyLogPage() function #function definition in file 'ontologyLogPage.R'
#' @title description of function logHeatmap
#' @export
setMethod('logHeatmap', signature = c ('character'),
		definition = function (cellexalObj, genes, png, grouping, ... ) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			logHeatmap(cellexalObj, genes, png, grouping, ... ) #function definition in file 'logHeatmap.R'
		}
)
