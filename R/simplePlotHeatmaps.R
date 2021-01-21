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

setMethod('simplePlotHeatmaps', signature = c ('cellexalvrR'),
	definition = function ( x, info, fname ) {
	#definition = function ( mat, fname ) {

	path = dirname(fname)
	if ( ! file.exists(path) ) {
		dir.create( path, recursive=TRUE)
	}
	info = groupingInfo(x, info$gname) ## make sure we only have the info we need.
	if ( length(which(!is.na(info$grouping))) > 0 ){
		x=reduceTo(x, what='col', to=colnames(x@data)[which(!is.na( x@userGroups[,info$gname]))] )
		x= reorder.samples( x, paste(info$gname, 'order'))
		info = groupingInfo(x, info$gname)
	}

	mat = FastWilcoxTest::ZScoreAll( x@data ) 
	colnames(mat) = colnames(x@data)
	rownames(mat) = rownames(x@data)
	

	gr = clusterGenes( mat  ) 

	## now I need to cellexalTime object:
	time = x@usedObj$timelines[[basename(fname)]]
	if ( is.null( time ) ){
		time = x@usedObj$timelines[[paste(basename(fname), 'timeline')]]
	}
	
	clusterC = rainbow( max(gr) )
	toPlot = time@dat[,c('time', 'col') ]

	pngs = NULL
	#create the separate simple Heatmap PNGs:
	ofile = paste( fname,'png', sep=".")

	mat = t(mat)
	ma = -1000
	mi = 1000
	error= NULL
	if ( ncol(mat) != nrow(time@dat)) {
		error = "The timeline function has failed to asigne a time to each selected cell - fix the selection to avoid this problem!"
		message( error )
		m= match( rownames(time@dat), rownames(mat))
		mat = mat[m,]
	}

	## we only have a width of 1000 pixels - I think we should sum up the expression so that we fit into 1000 cells!
	sum =NULL
	sumTime = NULL
	sumCol = NULL
	if ( ncol(x@data) > 1000 ){
		message( paste("To plot the timeline analysis", ncol(x@data), "cells are merged into 1000 data points using mean" ))
		n = ncol(x@data)
		ids = rep( 1:1000, floor(n/1000))
		ids = c( ids, sample( 1:1000, n%%1000))
		ids = sort(ids)
		B = FastWilcoxTest::collapse( x@data, ids, 2 )
		B <- Matrix::Matrix(B, sparse = TRUE)
		mat = FastWilcoxTest::ZScoreAll( B ) 
		colnames(mat) = colnames(1:1000)
		rownames(mat) = rownames(x@data)
		mat =t(mat)
		toPlot = data.frame(
		 time=unlist(lapply( split( time@dat[,'time'], ids ), mean)), 
		 col= unlist(lapply( split( as.vector(time@dat[,'col']), ids ), 
		 	function(x) { t=table(x); names(t)[which(t==max(t))]})) 
		 )
	}

	i = 1
	for( genes in split( names(gr), gr) ) {
		gn = paste('gene.group',i, sep=".")
		## what if we would use sum? no time is broken...
		pred1 = loess(  apply (mat[,genes], 1, mean) ~ toPlot[,'time'], span=.1)
		toPlot[,gn] = predict(pred1)
		#toPlot[,gn] = apply (mat[,genes], 1, mean)
		# plot( as.numeric(toPlot[,'time']), apply (mat[,genes], 1, mean))
		# points(toPlot[,'time'], toPlot[,gn], col='green' )
		ma = max( ma, toPlot[,gn])
		mi = min( mi, toPlot[,gn])
		of = paste(fname, i,'png', sep=".")
		h = length(genes)
		if ( h < 200)
			h = 200
		message( paste("I have", length(genes), "genes for this heatmap and am using the height =",h) )
		png( file=of, width=1000, height = h )
		pngs = c(pngs, of)
		image( mat[,genes], col=gplots::bluered(40), main = gn)
		box("outer", col=clusterC[i], lwd = 10)
		dev.off()
		i = i+1
	}

	## now I need to get the background info into a table
	xstarts = as.vector(toPlot$time[match( unique(toPlot$col), toPlot$col)])
	xstarts[1] = -Inf
	col = as.vector(toPlot$col[match( unique(toPlot$col), toPlot$col)])
	xends =as.vector(c(xstarts[-1],toPlot$time[nrow(toPlot)] )) 
	xends[length(xends)] = Inf
	rects = data.frame( xstarts, xends, col)
	rects$col = as.vector(rects$col)
	rects$col = factor( rects$col, levels=rects$col)
	#browser()
	png( file=ofile, width=1000, height = 1000)
	#pngs = c(pngs, ofile)
	toPlot2 = reshape2::melt( toPlot, id=c('time', 'col'))
	wes = function(n) {wesanderson::wes_palette("Zissou1", n,type = "continuous")[1:n] }
	pl = ggplot2::ggplot(toPlot, ggplot2::aes( xmin = min(time), xmax= max(time), ymin= mi, ymax=ma) )
	pl = pl +
	  ggplot2::geom_rect(data=rects,mapping = ggplot2::aes(
			xmin = xstarts, 
			xmax = xends, 
			#ymin = mi, 
			#ymax = mi+ (ma -mi)/10 
			ymin = -Inf,
			ymax = Inf
			),
	  	fill= wesanderson::wes_palette("Zissou1",10, type = "continuous")[1:10],
			alpha = .2) + 
	  ggplot2::scale_fill_manual( 
	  	palette = wes,
	  	values = wesanderson::wes_palette("Zissou1", 10,type = "continuous")[1:10],
	  	aesthetics = c("colour", "fill")
	  ) +  
	  ggplot2::guides(fill=FALSE)
	#plot( c(min(time@dat$time),max(time@dat$time) ), c(mi,ma), 
	#	col='white', xlab='pseudotime', 
	#	ylab="smoothed mean rolling sum expression of gene sets"  )
	#for ( a in 1:(i-1) ){
	#	points( toPlot$time, toPlot[,n], col=clusterC[a])
	#	lines( toPlot$time, toPlot[,n], col=clusterC[a])
	#}
	for ( a in 1:(i-1) ){
		n = paste(sep=".", 'gene', 'group', a)
		pl = pl + #ggplot2::geom_line( ggplot2::aes_string( y= n ), color=clusterC[a] ) +
		  ggplot2::geom_point(data=toPlot, 
		  	mapping=ggplot2::aes_string(x='time', y= n ), color=clusterC[a] ) +
		  ggplot2::geom_smooth(data=toPlot, 
		  	mapping=ggplot2::aes_string(x='time', y= n, alpha=.6 ), color=clusterC[a], 
		  	method=loess, fill=clusterC[a], alpha=.2, span=.1)
	}
	pl = pl + ggplot2::theme(panel.background = ggplot2::element_blank())
	pl = pl + ggplot2::ggtitle('Gene sets expression changes over the selected pseudotime')
	pl = pl + ggplot2::ylab( "Smoothed mean expression of gene sets" )
	pl = pl + ggplot2::ylab( "pseudotime" )
	print(pl)
	dev.off()

	list( genes = split( names(gr), gr), ofile = ofile, pngs = pngs, error= error, mat=mat )
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
#' @param x the cellexalvrR object to get the data from
#' @param deg.genes a list of genes 
#' @param info the group to cluster the genes for (list)
#' @param 
#' @title description of function plot
#' @export 
if ( ! isGeneric('clusterGenes') ){setGeneric('clusterGenes', ## Name
	function ( x, deg.genes=NULL, info=NULL ) { 
		standardGeneric('clusterGenes')
	}
) }

setMethod('clusterGenes', signature = c ('cellexalTime'),
	definition = function ( x, deg.genes=NULL, info=NULL ) {

		if ( ! is.null(deg.genes)){
			x = reduceTo( x, what='rwo', to = deg.genes )
		}
		if ( ! is.null(info) ) {
			x= reduceTo( x, what='col', colnames(x@data)[which(! is.na( x@userGroups[, info$gname]))] )
		}
		mat = FastWilcoxTest::ZScoreAll( x@data ) 
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

		gn
	}
)
