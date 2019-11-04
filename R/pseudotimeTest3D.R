#' A simple pseudotime analysis based on a linear model, that can create the pseudotime from the two varibales 'a' and 'b'
#' 
#' fit = loess( b~a )
#' 
#' The fitted values will be used as pseudotime and the pseudotiume is plotted 
#' versus the two defininge variables to the file outpath/Pseudotime.png using the grouping color.
#' Pseudotime is depicted as bluered line indicating the orientation blue -> red.
#' 
#' The pseudotime will be added into the samples table and the correlation to 
#' the pseudotime will be added to the annotation table (both replacing old data!).
#' 
#' The function returns the top/bottom (n) correlating genes.
#' 
#' The rolling mean values that are summing up only expression values from one group 
#' are colored in the group colour all others are black. 
#'  
#' @name pseudotimeTest3D
#' @aliases pseudotimeTest3D,cellexalvrR-method
#' @rdname pseudotimeTest3D-methods
#' @docType methods
#' @description Calculate and plot pseudotime relevant information.
#' @param x a cellexalvrR object
#' @param a the first time defining vector (e.g. a MDS dimension)
#' @param b the second time defining vector (e.g. a MDS dimension)
#' @param c the third time defining vector (e.g. a MDS dimension)
#' @param grouping a samples grouping to color the Pseudotime plot.
#' @param outpath a outpath for the files  default=sessionPath(x)
#' @param n how many genes to plot on both sides default=100
#' @param plotGenes additional default genes to plot default=NULL
#' @param smooth roling smoothing window size (default 100)
#' @param invert invert the time before usage ( default=FALSE)
#' @param cleanFolder remove all files from the outpout folder before creating new ones (default FALSE)
#' @param plotType ( 'png', 'png_high_res', 'pdf' )
#' @param summaryPlot the name of the summary plot file ( default NULL no summary plot)
#' @title description of function pseudotimeTest3D
#' @export 
if ( ! isGeneric('pseudotimeTest3D') ){setGeneric('pseudotimeTest3D', ## Name
	function ( x, a, b, c, grouping, outpath=NULL,  n=100, plotGenes=NULL, smooth=100, 
			invert=FALSE, cleanFolder=FALSE, plotType='png', summaryPlot=NULL ) { 
		standardGeneric('pseudotimeTest3D')
	}
) }

setMethod('pseudotimeTest3D', signature = c ('cellexalvrR'),
	definition = function ( x, a, b,c, grouping, outpath=NULL,  n=100, plotGenes=NULL, 
			smooth = 100, invert=FALSE, cleanFolder=FALSE, plotType='png' , summaryPlot=NULL ) {

		openPlot <- function(fname) {
			if ( plotType == 'pdf' ) {
				grDevices::pdf( file=paste(fname,'pdf', sep="."), width=10, height=10 )
			}else if (plotType == 'png_high_res' ){
				grDevices::png ( file=paste(fname,'highRes','png', sep="."), width=1600, height=1600)
			}else {
				grDevices::png ( file=paste(fname,'png', sep="."), width=800, height=800)
			}
		}
	if ( is.null( outpath )) {
		x = sessionPath(x)
		outpath = x@usedObj$sessionPath
	}
	if ( ! file.exists( outpath )) {
		dir.create( outpath )
	}else if ( cleanFolder) {
		unlink(file.path( outpath ,"*.png") )
		unlink(file.path( outpath ,"*.pdf") )
	}
	#if ( is.null( names(a) )) {
#		stop( "This function needs names on the a vector" )
#	}
	
	## would a partial loess work??
	## identify turns
	localLoess <- function (ids, a,b,c ) {
		## return a 3d loess line part
		ret = list()
		inp = list( a=a, b=b, c=c )
		ranges = unlist(lapply( list(a,b,c), function(d) { r = range(d[ids]); r[2] - r[1]} ))
		inp$order= c('a','b','c')[ order( ranges )]
		
		ls = loess( inp[[inp$order[2]]][ids] ~ inp[[inp$order[1]]][ids] )
		ret[[inp$order[1]]] = inp[[inp$order[1]]][ids]
		ret[[inp$order[2]]] = predict( ls)
		ls = loess(  inp[[inp$order[3]]][ids] ~ inp[[inp$order[1]]][ids] )
		ret[[inp$order[3]]] = predict( ls )
		
		names(ret[['a']]) = names( inp[['a']])
		RET = list( 'x' = ret$a, 'y' = ret$b, 'z' = ret$c )
		RET
	}

	res = localLoess( 1:length(a), a, b, c)
	
	o = FastWilcoxTest::euclidian_order3d( res$x, res$y, res$z )
	
	time = FastWilcoxTest::euclidian_distances3d( res$x[o], res$y[o], res$z[o], sum=T )

	res$time = time[order(o)]
	res$x = res$x
	res$y = res$y
	res$z = res$z
	res$a = a
	res$b = b
	res$c = c

	#sadfws

	if ( is.null( x@usedObj$timelines )){
	  x@usedObj$timelines= list()
	}
	x@usedObj$timelines[['lastEntry']] = res
	x@usedObj$timelines[[paste("run", length(names(x@usedObj$timelines))) ]] = res

	id= (ncol(x@userGroups) /2) + 1
	gname = paste( "Time.group", id, sep="." ) #the VR program dependeds on it

	m = match( names(res$a), colnames(x@data) )
	x@userGroups[,gname] = NA
	x@userGroups[m,gname] = res$time
	x@userGroups[,paste(gname, sep=" ", 'order')] = NA
	x@userGroups[m,paste(gname, sep=" ", 'order')] = order(res$time)
	
	x@usedObj$lastGroup = gname

	#x = CreateBin( x , gname)

	return(x)

# 	rgl::plot3d( res, col=gplots::bluered(length(o))[order(o)] )
# 	rgl::rgl.points( a,b,c, col= x@colors[[grouping]][ x@userGroups[,grouping]])
	
# 	# this data would now need to be put into the object?!


# 	fit$orig = fit$fitted.values
# 	fit$fitted.values = time
	
# 	openPlot( file.path( outpath,"Pseudotime" ) )
# 	plot( a ,  b, col= x$usedObj$colorRange[[grouping]][ x$samples[,grouping]] , pch=16)
# 	o = order( fit$fitted.values )
# 	points( a[o], fit$orig[o] , lwd=1.5, col=gplots::bluered(length(a)), pch=16, cex=2 )
# 	#lines( a,  fit$fitted.values, lwd=1.5, col='red' )
# 	dev.off()
				
# #	fit = lm( b~a )
# #	png ( file=file.path( outpath,"Pseudotime.png"), width=800, height=800)
# #	plot( a ,  b, col= x$usedObj$colorRange[[grouping]][ x$samples[,grouping]] )
# #	lines( a,  fit$fitted.values, lwd=1.5, col='red' )
# #	dev.off()
# 	dat = x@data
# 	#dat@x[which(dat@x == -1)] = 0
# 	dat=Matrix::drop0(dat)
	
# 	fit$fitted.values = fit$fitted.values - min( fit$fitted.values )
# 	traj.corGenes = FastWilcoxTest::CorMatrix( dat, fit$fitted.values )
# 	names(traj.corGenes) = rownames(x)
# 	x$annotation$cor2pseudotime = traj.corGenes 
# 	traj.corGenes =traj.corGenes[ which( ! is.na(traj.corGenes))]
	
# 	genes = c( sort(names(traj.corGenes[order(traj.corGenes)[1:n]])), sort(names(traj.corGenes[order(traj.corGenes, decreasing=T)[1:n]]))   ) 
	
# 	## stable variables:
# 	#browser()
# 	roll = function( x, smooth, type ) {
# 		func = NULL
# 		if ( type=='mean') {
# 			func = function(start, x, smooth ) { ret= mean( as.vector(x[(start-smooth):start])); if ( is.na(ret)){browser();}; ret  } 
# 		}else if ( type == 'table' ) {
# 			func = function( start, x, smooth) { length(names(table(as.vector(x[(start-smooth):start])))) }
# 		}else {
# 			stop ( "the roll function needs either 'mean' or 'table' as type!" )
# 		}
# 		#browser()
# 		unlist(lapply ( (smooth+1):length(x), func, x, smooth ) )
# 	}
# 	o = order( fit$fitted.values ) ## time
# 	X = roll ( fit$fitted.values[o], smooth, 'mean' )
# 	colV = roll ( as.numeric(x$samples[o,grouping]), smooth , 'mean' )
# 	colD = roll ( as.numeric(x$samples[o,grouping]), smooth , 'table' )
# 	col = x$usedObj$colorRange[[grouping]][ round( colV ) ]
# 	col[which(colD != 1)] = 'black'

# 	## create a summary plot for all genee
# 	if( ! is.null(summaryPlot)){

# 		openPlot(file.path( outpath, paste(summaryPlot,sep="_",'top') ))
# 		plot( min(X, na.rm=T), 0, xlim=c(min(X, na.rm=T), max(X, na.rm=T) ), ylim=c(0,1), xlab='pseudotime', ylab='smoothed normalized expression', col='white')
	
# 		for(  gname in sort(names(traj.corGenes[order(traj.corGenes)[1:n]])) ) {
# 			Y = roll( dat[gname, o], smooth, 'mean' )
# 			Y = Y - min(Y)
# 			Y = Y / max(Y)
# 			#lines(loess( Y ~ X, se=F , span=0.1),  lwd=1, col= x$usedObj$colorRange[[grouping]][ col ] )
# 			#browser()
# 			lines( X, Y, lwd=1, col= 'black' )
# 			lapply( names(table( col)) , function(n) {
# 				if ( n != 'black'){
# 					lines( X[which(col == n)], Y[which(col==n)], lwd=1, col= n )
# 				}
# 			})
# 		}
# 		dev.off()
		
# 		openPlot(file.path( outpath, paste(summaryPlot,sep="_",'bottom') ))
# 		plot(min(X, na.rm=T), 0, xlim=c(min(X, na.rm=T), max(X, na.rm=T) ), ylim=c(0,1) , xlab='pseudotime', ylab='smoothed normalized expression',col='white')
		
# 		for(  gname in sort(names(traj.corGenes[order(traj.corGenes, decreasing=T)[1:n]])) ) {
# 			Y = roll( dat[gname, o], smooth, 'mean' )
# 			Y = Y - min(Y)
# 			Y = Y / max(Y)
# 			lines( X, Y, lwd=1, col= 'black' )
# 			lapply( names(table( col)) , function(n) {
# 				if ( n != 'black'){
# 					lines( X[which(col == n)], Y[which(col==n)], lwd=1, col= n )
# 				}
# 			})
# 			#lines(loess( Y ~ X, se=F , span=0.1),  lwd=1, col= x$usedObj$colorRange[[grouping]][ col ] )
# 		}
# 		dev.off()
# 	}
# 	if ( is.null(summaryPlot)){
# 	for ( gname in unique(c( plotGenes, genes ) ) ){
# 		fname = paste(sep="_", gname, "traj.corGenes", smooth,"factor" )
# 		message(fname)
		
# 		Y = roll( dat[gname, o], smooth, 'mean' )
# 		openPlot(file.path( outpath, fname))
		
# 		plot(min(X, na.rm=T), 0, xlim=c(min(X, na.rm=T), max(X, na.rm=T) ), 
# 				ylim=c(min(Y), max(Y)) , xlab='pseudotime', ylab='smoothed expression',col='white')
# 		lines( X, Y, lwd=5, col= 'black' )
# 		lapply( names(table( col)) , function(n) {
# 			if ( n != 'black'){
# 				lines( X[which(col == n)], Y[which(col==n)], lwd=5, col= n )
# 			}
# 		})
# 		#plot( x, y ,  main = gname, xlab='pseudotime', ylab='smoothed expression', type='l', lwd=5 )
#         #plot(loess( unlist(Y) ~ unlist(X), se=F , span=0.1),  main = gname, col= col,
# 		#		xlab='pseudotime', ylab='smoothed expression', type='l', lwd=5 )
		
# 		dev.off()
# 	}
# 	}
# 	x$samples$Pseudotime = fit$fitted.values
	
# 	genes
	
} )
