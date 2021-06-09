
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

	if ( !is.null(genes[1])){
		if ( file.exists(genes[1])) {
			genes = as.vector(utils::read.delim(genes[1])[,1])
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
		if ( class( cellexalObj@groupSelectedFrom[[i]]) != 'cellexalGrouping') {
			next
		}
		if ( length( cellexalObj@groupSelectedFrom[[i]]@heatmapBasename) != 0 ){
			if ( cellexalObj@groupSelectedFrom[[i]]@heatmapBasename == basename(base) ) {
				grouping = cellexalObj@groupSelectedFrom[[i]]@gname
				break
			}
		}else if ( cellexalObj@groupSelectedFrom[[i]]@selectionFile == basename(grouping)){
			cellexalObj@groupSelectedFrom[[i]]@heatmapBasename = basename(base)
			grouping = cellexalObj@groupSelectedFrom[[i]]@gname
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

	cellCount = table(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup])
	R_IDs = names(cellCount)
	OK = which(!is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
	tab = cellexalObj@userGroups[OK,]
	tab = tab[order( tab[,paste( cellexalObj@usedObj$lastGroup, 'order')]) ,]
	tab = tab[ match( R_IDs,as.vector(tab[,cellexalObj@usedObj$lastGroup] ) ),]


	tab =  tab[order( as.numeric(tab[,paste(cellexalObj@usedObj$lastGroup, 'order')])),]

	tableHTML = HTMLtable ( gInfo) 
	

	figureF = correctPath(figureF, cellexalObj)
	content = paste( sep="\n",
		paste( "##", "Heatmap from Saved Selection ", n  ),
		paste("This selection is available in the R object as group",cellexalObj@usedObj$lastGroup ),
		"",
		paste( "### Genes"),
		md_gene_links( sort(genes) ), #function definition in file 'rmdLink.R'
		md_gene_links( rev(genes), label="expand genes in heatmap order" ),
		'',
		paste( "### Heatmap (from CellexalVR)"),
		paste("![](",figureF,")"),
		'',
		tableHTML,'',
		paste(collapse = "\n", sep="\n",drcFiles2HTML(cellexalObj, gInfo )),
		"The heatmap can be restored in a new VR session using the 2D console (F12) and type:",
		"",
		paste("lsf", R.utils::getRelativePath(gInfo@selectionFile,
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
