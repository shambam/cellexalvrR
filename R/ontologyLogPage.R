#' @name ontologyLogPage
#' @aliases ontologyLogPage,cellexalvrR-method
#' @rdname ontologyLogPage-methods
#' @docType methods
#' @description creates the GO analysis for a gene list and puts it into the report.
#' @param cellexalObj the cellexalvrR object
#' @param genes a list of gene symbols (IMPORTANT)
#' @param ... unused
#' @title description of function ontologyLogPage
#' @export 
setGeneric('ontologyLogPage', ## Name
	function ( cellexalObj, genes, ... ) { 
		standardGeneric('ontologyLogPage')
	}
)

setMethod('ontologyLogPage', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes, ... ) {
	## process the ontology for this gene list and add one ontology report page
	error = ""
	
	## for this to work as expected you need an up to date pandoc:
	## https://pandoc.org/installing.html
	
	n = length( grep ( "GOanalyis.Rmd", list.files(cellexalObj@usedObj$sessionPath) ) )
	
	if(cellexalObj@specie =='mouse'){
		if(require(org.Mm.eg.db)){
			x <- org.Mm.eg.db}else{
			stop("Install org.Mm.eg.db package for retrieving gene lists from GO")
		}
	}else if ( cellexalObj@specie=='human'){
		if(require(org.Hs.eg.db)){
			x <- org.Hs.eg.db}else{
			stop("Install org.Hs.eg.db package for retrieving gene lists from GO")
		}
	}else {
		error= paste( "The specie",  cellexalObj@specie,  "is up to now not supported in the GO reports function" )
	}
	if ( is.null( cellexalObj@usedObj$GO2genes)){
		cellexalObj@usedObj$GO2genes = mapIds(x, keys(x,'GO'), 'SYMBOL', 'GO', multiVals = 'list')
	}
	
	all = is.na(match(rownames(cellexalObj@data), genes ))
	names(all) = rownames(cellexalObj@data)
	all = factor(all)
	tryCatch({  library("topGO", quietly = TRUE) } ,  
			error = function(e) {
					stop(paste("topGO needed for this function to work. Please install it.\n", e),
							call. = FALSE)
		})
		

	cellexalObj@usedObj$analysis = new("topGOdata", ontology = "BP", allGenes=all 
		,geneSel =  function(x) {x} ,  annot = topGO::annFUN.GO2genes, GO2genes= cellexalObj@usedObj$GO2genes)

	
	resultFisher <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "classic", statistic = "fisher")
	resultKS <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "classic", statistic = "ks")
	resultKS.elim <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "elim", statistic = "ks")
	
	allRes <- topGO::GenTable(cellexalObj@usedObj$analysis, classicFisher = resultFisher,classicKS = resultKS, elimKS = resultKS.elim,
					orderBy = "elimKS", ranksOf = "classicFisher", topNodes = 10)
			
	for ( i in 1:nrow(allRes) ) {
		allRes[i,1] = paste( sep="", '[',allRes[i,1],'] (http://amigo.geneontology.org/amigo/term/', allRes[i,1],')')
	}
	write.table(allRes, sep='\t', quote=F, row.names=F, file= file.path( cellexalObj@usedObj$sessionPath, 'tables', filename(c( n, "GOanalysis.csv") ) ) )
	## and now put this nice little table into the GEO section ;-)
	## and probably save this damn analysis object....
	
	if ( is.null( cellexalObj@usedObj$sessionPath)) {
		cellexalObj = sessionPath(cellexalObj)
	}
	sessionPath = cellexalObj@usedObj$sessionPath
	mainOfile = file.path(sessionPath, filename( c( n, "GOanalysis.Rmd") ))
	fileConn<-file( mainOfile )

	writeLines(c(
					paste( "##", "GO analysis for grouping", cellexalObj@usedObj$lastGroup  ),
					paste( "### Genes"),
					paste( collapse=" ", genes ),
					"",
					paste( "The R package topGO was used to create this output table:"),
					" ",
					" ",
					knitr::kable(allRes, caption=paste("GO analysis for grouping", cellexalObj@usedObj$lastGroup ))
			), fileConn)
	
	close(fileConn)
	cellexalObj@usedObj$sessionRmdFiles = c( cellexalObj@usedObj$sessionRmdFiles, mainOfile)
	## object is saved in the heatmap function!
	
	cellexalObj
	
} )
