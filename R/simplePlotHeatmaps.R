#' The log logics sometimes would benefit froma usable descriptions of the expression patterns.
#' Here we use the most simple heatmap functionallity in R (image) to show the expression values.
#' The returned genes list has the genes in the order of display in the heatmaps.
#'
#' In order to add usability the hetmap is split into an optimal number of sub heatmaps to show the trends.
#'
#' @name simplePlotHeatmaps
#' @aliases simplePlotHeatmaps,cellexalvrR-method
#' @rdname simplePlotHeatmaps-methods
#' @docType methods
#' @description plot an extremely simple heatmap ans slices of that
#' @param x, cellexalvrR object
#' @param mat the expression matrix (col genes; row cells)
#' @param fname the outfile base (.png for main .<i>.png for the slices)
#' @title description of function simplePlotHeatmaps
#' @export 
if ( ! isGeneric('simplePlotHeatmaps') ){setGeneric('simplePlotHeatmaps', ## Name
	function (x, mat, fname ) { 
	#function ( mat, fname ) { 	
		standardGeneric('simplePlotHeatmaps')
	}
) }

setMethod('simplePlotHeatmaps', signature = c ('cellexalvrR'),
	definition = function ( x, mat, fname ) {
	#definition = function ( mat, fname ) {
	genes = colnames(mat)
	if ( is.null(genes)){
		stop("ERROR: the rownames of the matrix are not set")
	}
	path = dirname(fname)
	if ( ! file.exists(path) ) {
		dir.create( path, recursive=TRUE)
	}
	hc = hclust( as.dist( 1- stats::cor(mat, method='pearson') ) )
	deg.genes = hc$labels[hc$order]

	## for the usability of the log file the genes need to be ordered.
	## The heatmap needs to show these clusters of genes. And to identify the right number of clusters I need and elbow analysis.
	## https://www.icsi.berkeley.edu/icsi/node/4806
	## Finding a Kneedle in a Haystack: Detecting Knee Points in System Behavior
	## Satopaa, V.., Albrecht J., Irwin D., & Raghavan B., 2010
	points = unlist(lapply( 1:20, function(k) {  #total within-cluster sum of square (WSS)
		gr = cutree(hc, k);  
		mean( unlist( lapply( 1:k, function(id) {
			ret = 0
			if ( length( which(gr == id) ) > 1) {
				dat = mat[,which(gr == id)]
				ret = (nrow(dat)-1)*sum(apply(dat,2,var) )
			}
			ret
			} ) ), na.rm=TRUE)
		} ) )

	
	
	## create a linear function between start: 1;points[1] and end: length(points);points[length(points)]
	slope <- diff(c(points[1], points[length(points)] ))/diff(c(1,length(points)))
	intercept <- points[1]-slope
	f = function(x) { x * slope + intercept }
	der = unlist(lapply( 1:length(points) ,function(x) { points[x] - f(x) }))
	der = der- min(der)
	## And find the max length of this value
	## here more groups is likely better than less
	optimum <- max ( which(der < max(der) / 1e+10) )
	## now we lack the heatmap here... But I would need one - crap!
	## add a simple one - the most simple one ever, but use a subcluster of genes, too!!
	gr = cutree(hc, optimum); 
	i = 1
	## now I need to cellexalTime object:
	time = x@usedObj$timelines[[basename(fname)]]

	clusterC = rainbow( max(gr) )
	toPlot = time@dat[,c('time', 'col') ]

	pngs = NULL
	#create the separate simple Heatmap PNGs:
	ofile = paste( fname,'png', sep=".")

	ma = -1000
	mi = 1000
	for( genes in split( names(gr), gr) ) {
		gn = paste('gene.group',i, sep=".")
		pred1 = loess(  apply (mat[,genes], 1, mean) ~ time@dat$time, span=.1)
		toPlot[,gn] = predict(pred1)
		ma = max( ma, toPlot[,gn])
		mi = min( mi, toPlot[,gn])
		of = paste(fname, i,'png', sep=".")
		h = round(1000 * length(genes) / ncol(mat) )
		if ( h < 200)
			h = 200
		message( paste("I have", length(genes), "genes for this heatmap and am using the height =",h) )
		png( file=of, width=1000, height = h )
		pngs = c(pngs, of)
		image( mat[,genes], col=gplots::bluered(40))
		dev.off()
		i = i+1
	}

	png( file=ofile, width=1000, height = 1000)
	#pngs = c(pngs, ofile)
	plot( c(min(time@dat$time),max(time@dat$time) ), c(mi,ma), 
		col='white', xlab='pseudotime', 
		ylab="smoothed mean rolling sum expression of gene sets"  )

	#browser()
	for ( a in 1:(i-1) ){
		n = paste(sep=".", 'gene', 'group', a)
		points( toPlot$time, toPlot[,n], col=clusterC[a])
		lines( toPlot$time, toPlot[,n], col=clusterC[a])
	}

	dev.off()


	list( genes = split( names(gr), gr), ofile = ofile, pngs = pngs )
} )
