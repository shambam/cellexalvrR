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
#' @param mat the expression matrix (col genes; row cells)
#' @param fname the outfile base (.png for main .<i>.png for the slices)
#' @title description of function simplePlotHeatmaps
#' @export 
if ( ! isGeneric('simplePlotHeatmaps') ){setGeneric('simplePlotHeatmaps', ## Name
	function (mat, fname ) { 
		standardGeneric('simplePlotHeatmaps')
	}
) }

setMethod('simplePlotHeatmaps', signature = c ('matrix'),
	definition = function (mat, fname ) {
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

	pngs = character( optimum )

	ofile = paste( fname,'png', sep=".")
	for( genes in  split( names(gr), gr) ) {
		of = paste(fname, i,'png', sep=".")
		h = round(1000 * length(genes) / ncol(mat) )
		if ( h < 200)
			h = 200
		message( paste("I have", length(genes), "genes for this heatmap and am using the height =",h) )
		png( file=of, width=1000, height = h )
		image( mat[,genes], col=gplots::bluered(40))
		dev.off()
		pngs[i] = of
		i = i+1
	} 
	png( file=ofile, width=1000, height = 1000)
	image( mat[,deg.genes], col=gplots::bluered(40))
	dev.off()

	list( genes = split( names(gr), gr), ofile = ofile, pngs = pngs )
} )
