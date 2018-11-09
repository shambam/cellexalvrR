#' @name ontologyLogPage
#' @aliases ontologyLogPage,cellexalvrR-method
#' @rdname ontologyLogPage-methods
#' @docType methods
#' @description creates the GO analysis for a gene list and puts it into the report.
#' @param cellexalObj the cellexalvrR object
#' @param genes a list of gene symbols (IMPORTANT)
#' @param ontology which GO ontology to choose from (default = "BP")
#' @param topNodes how many GO terms to report (default 10)
#' @param ... unused
#' @import org.Mm.eg.db org.Hs.eg.db topGO
#' @title description of function ontologyLogPage
#' @export
setGeneric('ontologyLogPage', ## Name
	function ( cellexalObj, genes, ontology = 'BP', topNodes=10, ... ) {
		standardGeneric('ontologyLogPage')
	}
)

setMethod('ontologyLogPage', signature = c ('cellexalvrR'),
	definition = function ( cellexalObj, genes, grouping, ontology = 'BP', topNodes=10, ... ) {
	## process the ontology for this gene list and add one ontology report page
	if ( file.exists(genes)) {
		genes = as.vector(read.delim(genes)[,1])
	}
	
	cellexalObj = userGrouping( cellexalObj, grouping )
	cellexalObj = sessionRegisterGrouping( cellexalObj, cellexalObj@usedObj$lastGroup )
	
	#error = ""
	# message( paste( sep=" ","ontologyLogPage genes:",  paste( sep=", ",genes) ) )
	## for this to work as expected you need an up to date pandoc:
	## https://pandoc.org/installing.html

	n = length( grep ( "GOanalyis.csv", list.files( file.path(cellexalObj@usedObj$sessionPath, 'tables') ) ) ) +1
	
	if( length(cellexalObj@specie) == 0){
		cellexalObj = useInbuiltGOIlists(cellexalObj, 'TFs' ) ## sets the species if not alread set
	}
	if(cellexalObj@specie =='mouse'){
		x <- org.Mm.eg.db
		#if(require(org.Mm.eg.db)){
		#	x <- org.Mm.eg.db}else{
		#	stop("Install org.Mm.eg.db package for retrieving gene lists from GO")
		#}
	}else if ( cellexalObj@specie=='human'){
		x <- org.Hs.eg.db
		#if(require(org.Hs.eg.db)){
		#	x <- org.Hs.eg.db}else{
		#	stop("Install org.Hs.eg.db package for retrieving gene lists from GO")
		#}
	}else {
		stop( paste( "The specie",  cellexalObj@specie,  "is up to now not supported in the GO reports function" ))
	}
	if ( is.null( cellexalObj@usedObj$GO2genes)){
		cellexalObj@usedObj$GO2genes = mapIds(x, keys(x,'GO'), 'SYMBOL', 'GO', multiVals = 'list')
	}
	
	
	all = is.na(match(rownames(cellexalObj@data), genes ))
	names(all) = rownames(cellexalObj@data)
	all = factor(all)
	if ( length(table(all)) == 1) {
		message( "No genes of the list are in this object - This should not have happened!")
		return ( cellexalObj )
	}
#	tryCatch({  library("topGO", quietly = TRUE) } ,
#			error = function(e) {
#					stop(paste("topGO needed for this function to work. Please install it.\n", e),
#							call. = FALSE)
#		})

	cellexalObj@usedObj$analysis = new("topGOdata", ontology = ontology, allGenes=all
		,geneSel =  function(x) {x} ,  annot = topGO::annFUN.GO2genes, GO2genes= cellexalObj@usedObj$GO2genes)


	resultFisher <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "classic", statistic = "fisher")
	resultKS <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "classic", statistic = "ks")
	resultKS.elim <- topGO::runTest(cellexalObj@usedObj$analysis, algorithm = "elim", statistic = "ks")
	topNodes <- as.numeric(topNodes)
	allRes <- topGO::GenTable(cellexalObj@usedObj$analysis, classicFisher = resultFisher,classicKS = resultKS, elimKS = resultKS.elim,
			orderBy = "elimKS", ranksOf = "classicFisher", topNodes = topNodes)

	GOI_2_genes <- matrix( 1, nrow=topNodes, ncol=3)
	colnames(GOI_2_genes) = c("GO ID", "Rmd Gene list", "Mapping Gene List")
	for( i in 1:nrow(allRes) ) {
		GOI_2_genes[i,1] = allRes[i,1]
		GOI_2_genes[i,2] =  paste( intersect( genes,cellexalObj@usedObj$GO2genes[[allRes[i,1]]]), collapse=" ")
		GOI_2_genes[i,3] = paste(
				unlist( lapply(	intersect( genes,cellexalObj@usedObj$GO2genes[[allRes[i,1]]]),
		            rmdLink, link="https://www.genecards.org/cgi-bin/carddisp.pl?gene=", lineEnd=FALSE ))
			, collapse=" "
	    )
	}
	for ( i in 1:nrow(allRes) ) {
		allRes[i,1] = rmdLink(allRes[i,1],"http://amigo.geneontology.org/amigo/term/", lineEnd=FALSE )
	}

	GOI_2_genes = cbind(GOI_2_genes,  allRes )	
	
	write.table(GOI_2_genes, sep='\t', quote=F, row.names=F, file= 
					file.path( 
							cellexalObj@usedObj$sessionPath, 
							'tables', 
							filename(
								c( 
									n,
									"GOgenes.csv"
								) 
							) 
					) 
	)
	
	GOI_2_genes = GOI_2_genes[,c(1,3)]
	
	allRes = allRes[,-c(4,5)] ## significant and expected columns do not contain info
	write.table(allRes, sep='\t', quote=F, row.names=F, file= file.path( cellexalObj@usedObj$sessionPath, 'tables', 
					filename(c( n , "GOanalysis.csv") ) ) )
	## and now put this nice little table into the GEO section ;-)
	## and probably save this damn analysis object....

	if ( is.null( cellexalObj@usedObj$sessionPath)) {
		cellexalObj = sessionPath(cellexalObj)
	}
	sessionPath = cellexalObj@usedObj$sessionPath


	#mainOfile = file.path(sessionPath, filename( c( n, "GOanalysis.Rmd") ))
	#fileConn<-file( mainOfile )
	#file.create(mainOfile)
	mainOfile = cellexalObj@usedObj$sessionRmdFiles[1]
	
	cat( sep="\n", 
					paste( "##", "GO analysis from Saved Selection", sessionCounter(  cellexalObj, cellexalObj@usedObj$lastGroup ) ),
					paste("This selection is available in the R object as group", cellexalObj@usedObj$lastGroup ),
					"",
					paste( "### Genes"),
					paste( collapse="", unlist( lapply( genes,  rmdLink, link="https://www.genecards.org/cgi-bin/carddisp.pl?gene=" ))),
					"",
					paste( "The R package topGO was used to create this output table:"),
					" ",
					" ",
					knitr::kable(allRes, caption=paste("GO analysis for grouping", cellexalObj@usedObj$lastGroup )),
					" ",
					knitr::kable(GOI_2_genes, caption=paste("The genes mapping to get GO ids" )),
					""
			, file = mainOfile, append = TRUE)

	#close(fileConn)

	cellexalObj@usedObj$sessionRmdFiles = c( cellexalObj@usedObj$sessionRmdFiles, mainOfile)
	## object is saved in the heatmap function!

	invisible(cellexalObj)

} )
