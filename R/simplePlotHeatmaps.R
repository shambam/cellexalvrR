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
#' @param x, cellexalvrR object that has the correct order and only the genes of interest!
#' @param info the grouping info for this plot
#' @param fname the outfile base (.png for main .<i>.png for the slices)
#' @title description of function simplePlotHeatmaps
#' @export
#' @returns a list with the keys 
#'   png - all outfiles created; first summary others heatmaps),
#'   genes - the genes split into the displayed groups
#'   ofile - the main outfile
#'   error - any error occuring - should be included in the report.

if ( ! isGeneric('simplePlotHeatmaps') ){setGeneric('simplePlotHeatmaps', ## Name
	function (x, info, fname ) { 
	#function ( mat, fname ) { 	
		standardGeneric('simplePlotHeatmaps')
	}
) }

setMethod('simplePlotHeatmaps', signature = c ('cellexalvrR', 'list', 'character'),
	definition = function ( x, info, fname ) {
	#definition = function ( mat, fname ) {

	path = dirname(fname)
	if ( ! file.exists(path) ) {
		dir.create( path, recursive=TRUE)
	}
	if ( ! class(info) == 'list'){
		stop("I need an info list as obtained from calling groupingInfo")
	}

	## now I need to cellexalTime object:
	ti = x@usedObj$timelines[[info$gname]]
	
	if ( is.null( ti ) ){
		## oops - we got a parentSelection?
		## best guess
		if ( x@usedObj$timelines[["lastEntry"]]@parentSelection == info$gname){
			ti = x@usedObj$timelines[["lastEntry"]]
			info = groupingInfo( ti@gname, x)
		}
	}
	if ( is.null( ti ) ){
		stop(paste( "The time for the selection", info$gname, "could not be found") )
	}

	if ( ! is.null(x@usedObj$deg.genes) ) {
		toPlot = compactTimeZscore( ti, x@usedObj$deg.genes, info, x )
	}else {
		toPlot = compactTimeZscore( ti, rownames(x@data), info, x )
	}

	error = NULL
	gr = clusterGenes( t(toPlot[, -c(1,2) ]), info = info ) 
	clusterC = rainbow( max(gr) )

	pngs = NULL
	#create the separate simple Heatmap PNGs:
	ofile = paste( fname,'png', sep=".")

	ma = -1000
	mi = 1000
	i = 1


	for( genes in split( names(gr), gr) ) {
		gn = paste('gene.group',i, sep=".")
		## what if we would use sum? no time is broken...
		if ( length(genes) > 1){
			pred1 = loess( apply (toPlot[,genes], 1, mean) ~ toPlot[,'time'], span=.1)
		}
		else {
			pred1 = loess( toPlot[,genes] ~ toPlot[,'time'], span=.1)
		}
		toPlot[,gn] = predict(pred1)

		ma = max( ma, toPlot[,gn])
		mi = min( mi, toPlot[,gn])

		of = paste(fname, i, sep=".")
		of = plotTimeHeatmap( t(toPlot[,genes]), of,  col=clusterC[i], circleF = paste(sep=".", ofile,i,'svg' ) )
		#of = correctPath( of, x )
		pngs = c(pngs, of)
		i = i+1
	}

	dat = as.list(toPlot[, grep('gene.group', colnames(toPlot))])
	for (n in names(dat) ) {
		names(dat[[n]]) = rownames(toPlot)
	}
	plotDataOnTime ( data.frame(toPlot[,c('time', 'col')]), dat=dat, ofile=ofile )

	list( genes = split( names(gr), gr), ofile = ofile, pngs = pngs, error= error, mat=toPlot[,sort(names(gr))] )
} )





#' Tries to answer the question: how do these genes differ over the timeline.
#' It answers this in a graphical, not a statistical way.
#' Hence you can feed whichever genelist you like into this function.
#'  
#' @name clusterGenes
#' @aliases clusterGenes,cellexalTime-method
#' @rdname clusterGenes-methods
#' @docType methods
#' @description get a - hopefully - optimal grouing of a list of genes 
#' @param x either the z-scored matrix or a cellexalTime object
#' @param deg.genes a list of genes 
#' @param info the group to cluster the genes for (list)
#' @param cellexalObj if x is a cellexalTime object this is necessary to create the zscored matrix.
#' @title description of function plot
#' @export 
if ( ! isGeneric('clusterGenes') ){setGeneric('clusterGenes', ## Name
	function ( x, deg.genes=NULL, info=NULL, ... ) { 
		standardGeneric('clusterGenes')
	}
) }

setMethod('clusterGenes', signature = c ('cellexalTime'),
	definition = function ( x, deg.genes=NULL, info=NULL, cellexalObj ) {

		if ( ! is.null(deg.genes)){
			cellexalObj = reduceTo( cellexalObj, what='rwo', to = deg.genes )
		}
		if ( ! is.null(info) ) {
			cellexalObj= reduceTo( cellexalObj, what='col', 
				colnames(cellexalObj@data)[which(! is.na( cellexalObj@userGroups[, info$gname]))] )
			cellexalObj = reorder.samples( cellexalObj, info$gname)
		}
		mat = FastWilcoxTest::ZScoreAll( x@data, display_progress=FALSE ) 
		colnames(mat) = colnames(x@data)
		rownames(mat) = rownames(x@data)

		clusterGenes( mat, deg.genes, info )
	}
)

setMethod('clusterGenes', signature = c ('matrix'),
	definition = function ( x, deg.genes=NULL, info=NULL ) {

		pca = irlba::prcomp_irlba ( x, center=T, n=3 )$x

		#hc = hclust( as.dist( 1- stats::cor(mat, method='pearson') ) )
		#deg.genes = hc$labels[hc$order]

		## for the usability of the log file the genes need to be ordered.
		## The heatmap needs to show these clusters of genes. And to identify the right number of clusters I need and elbow analysis.
		## https://www.icsi.berkeley.edu/icsi/node/4806
		## Finding a Kneedle in a Haystack: Detecting Knee Points in System Behavior
		## Satopaa, V.., Albrecht J., Irwin D., & Raghavan B., 2010
	
		points = unlist(lapply( 2:20, function(k) {  #total within-cluster sum of square (WSS)
			gr = stats::kmeans(pca,centers= k)$cluster
			names(gr) = rownames(x)
			#gr = cutree(hc, k);  
			mean( unlist( lapply( 1:k, function(id) {
				ret = 0
				if ( length( which(gr == id) ) > 1) {
					dat = x[which(gr == id), ]
					ret = (ncol(dat)-1)*sum(apply(dat,1,var) )
				}
				ret
				} ) ), na.rm=TRUE
			)
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
		## plot( points, der)
		## abline( v= points[optimum], col='red')
		## this should not be standard, but lets just get a little more than that. Better more than too little info.
		optimum = optimum + 1
		## now we lack the heatmap here... But I would need one - crap!
		## add a simple one - the most simple one ever, but use a subcluster of genes, too!!

		gn = stats::kmeans(pca,centers= optimum)$cluster
		names(gn) = rownames(x)
		geneTrajectories = NULL
		i = 1
		if ( ! is.null(info$time ) ) {
			cT = collapseTime( info$time ) 
			for( genes in split( names(gn), gn) ) {
				browser()
				pred1 = loess( apply (x[,genes], 1, mean) ~ toPlot[,'time'], span=.1)
				gn = paste('gene.group',i, sep=".")
				## what if we would use sum? no time is broken...
			if ( length(genes) > 1){
				geneTrajectories[[i]] = loess( apply (x[,genes], 1, mean) ~ toPlot[,'time'], span=.1)
				#pred1 = loess( apply (toPlot[,genes], 1, mean) ~ toPlot[,'time'], span=.1)
			}
			else {
				#pred1 = loess( toPlot[,genes] ~ toPlot[,'time'], span=.1)
			}
			#toPlot[,gn] = predict(pred1)	
		}
	}


		gn
	}
)
