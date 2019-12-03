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
		inp$order= c('a','b','c')[ order( ranges, decreasing=T )]
		
		ls = loess( inp[[inp$order[2]]][ids] ~ inp[[inp$order[1]]][ids] )
		ret[[inp$order[1]]] = inp[[inp$order[1]]][ids]
		ret[[inp$order[2]]] = predict( ls)
		ls = loess(  inp[[inp$order[3]]][ids] ~ inp[[inp$order[1]]][ids] )
		ret[[inp$order[3]]] = predict( ls )

		names(ret[['a']]) = names( inp[['a']])
		RET = list( 'x' = ret$a, 'y' = ret$b, 'z' = ret$c )

		#rgl::plot3d( cbind( x=a,y=b,z=c), col= gplots::bluered(length(a))[ x@userGroups[[paste(grouping, 'order')]] ])
		#rgl::rgl.points( RET )

		RET
	}

	

	res = localLoess( 1:length(a), a, b, c)
	
	o = NULL
	try({o = FastWilcoxTest::euclidian_order3d( res$x, res$y, res$z )},silent= TRUE)
	
	time = FastWilcoxTest::euclidian_distances3d( res$x[o], res$y[o], res$z[o], sum=T )

	res$time = time[order(o)]
	res$x = res$x
	res$y = res$y
	res$z = res$z
	res$a = a
	res$b = b
	res$c = c

	id= (ncol(x@userGroups) /2) + 1
	gname = paste( "Time.group", id, sep="." ) 
	if ( is.null( x@usedObj$timelines )){
	  x@usedObj$timelines= list()
	}

	x@usedObj$timelines[['lastEntry']] = res
	x@usedObj$timelines[[ gname ]] = res

	f = NULL
	if ( file.exists(x@usedObj$SelectionFiles[[ grouping ]] )) {
		## I need to create a new one named 
		info = groupingInfo(x, grouping )
		f = x@usedObj$SelectionFiles[[ gname ]] = 
		paste( sep=".", x@usedObj$SelectionFiles[[ grouping ]], 'time')
		## no colnames: cell name, color, drc name and selection id - fille with 0
		o = order(res$time)
		l = length(o) 
		d = cbind( names(res$c)[o], gplots::bluered(l), rep( info$drc , l ), rep(0, l)  )
		write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file=f )

		f2 = paste( sep=".", f,'points')
		d = cbind( names(res$c)[o], res$x[o], res$y[o], res$z[o]  )
		write.table( d, col.names=F, row.names=F, quote=F, sep="\t", file=f2 )

		if ( file.exists( x@usedObj$sessionPath ) ) {
			file.copy( f, x@usedObj$sessionPath)
			file.copy( f2, x@usedObj$sessionPath)
		}
	}


	#the VR program dependeds on it

	m = match( names(res$a), colnames(x@data) )
	x@userGroups[,gname] = NA
	x@userGroups[m,gname] = res$time
	x@userGroups[,paste(gname, sep=" ", 'order')] = NA
	x@userGroups[m,paste(gname, sep=" ", 'order')] = order(res$time)
	
	x@usedObj$lastGroup = gname

	return(x)
	
} )
