#' @name oscilliatingGenes
#' @aliases oscilliatingGenes,cellexalvr-method
#' @rdname oscilliatingGenes-methods
#' @docType methods
#' @description  Identifies oscilliating genes and tries to group them in oscilliating clusters using
#'  the EMD package. This function should not be used!
#' @param cellexalObj the cellexal object
#' @param cellidfile the grouping or a file to read a grouping from
#' @return the cellexal object
#' @title description of function oscilliatingGenes
#' @export 
setGeneric('oscilliatingGenes', ## Name
	function ( cellexalObj, cellidfile ) { ## Argumente der generischen Funktion
		standardGeneric('oscilliatingGenes') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('oscilliatingGenes', signature = c ('cellexalvr'),
	definition = function ( cellexalObj, cellidfile ) {
	
	stop( "This is pointless - try the code by hand as it is quite time consuming." )
	
	if ( class(cellexalObj) == 'character' ) {
		if ( file.exists( cellexalObj ) ) {
			load(cellexalObj)
		}
	}
	if ( file.exists(cellidfile)) {
		cellexalObj <- userGrouping(cellexalObj, cellidfile)
	}
	
	loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj$data)[-
							which(is.na(cellexalObj$userGroups[,cellexalObj$usedObj$lastGroup]))
			] )
	loc <- reorder.samples ( loc, paste(cellexalObj$usedObj$lastGroup, 'order'))
	
	info <- groupingInfo( loc )
#	
#	xt2 <- apply(loc@data, 2, sum)
#	tt2 <- info$order
#	try <- emd(xt2, tt2, boundary="wave")
#	for(i in 1:try$nimf) {
#		#try$imf[,i] is the waveform that has been identified 
#		png( file= paste("wave_",i,'.png', sep=''), width=1000, height=300 )
#		plot(tt2, try$imf[,i], type="l", xlab="", ylab="", ylim=rangeimf,
#				main=paste(i, "-th IMF", sep="")); 
#		abline(h=0)
#		dev.off()
#		## now I want to find the genes, that follow this wave
#		
#	}
	
	no_cores <- detectCores() - 1
	cl <- makeCluster(no_cores)
	try <- parLapply(1:nrow(loc@data), function( i ) { emd( loc@data[i,], info$grouping ) } )
	d <- NULL
	ids <- NULL
	for ( i in 1:length(try) ){
		if ( try[[i]]$nimf > 1 ) {
			d <- rbind(d, try[[i]]$imf[,1]) 
			ids <- c(ids, i) 
		} 
	}
	rownames(d) <- rownames(loc@data)[ids]
	int_functions <- loc
	int_functions@data <- d
	int_functions@meta.gene <- loc@meta.gene[ids,]
	
	data <- as_BioData(int_functions)
	clusters_gene(data, clusterby = "DDRTree", groups.n = 15)
	collapse(data,group=data$usedObj$auto_clusters_gene, what='row' )
	## now I can try to find summary trends in the data - I assume I could have done that easier...
	force.numeric(data)
	data$raw = data$data
	for ( i in 1:nrow(data$data)) {
		data$raw[i,] <- smooth.spline(data$data[i,])$y
	}
	cellexalObj$usedObj$trends = data
	print ("return@usedObj$trends$raw contains trends in the selected data")
	
	## this whole analysis is totally pointless :-((
	
	cellexalObj
} )
