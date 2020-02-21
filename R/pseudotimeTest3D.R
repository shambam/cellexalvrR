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
			invert=FALSE, cleanFolder=FALSE, plotType='png', summaryPlot=NULL) { 
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

		## get the time here, too
		dists =FastWilcoxTest::eDist3d( inp[[inp$order[1]]][ids] , inp[[inp$order[2]]][ids] , inp[[inp$order[3]]][ids], 1 )
		B = which(dists == max(dists))
		dists =FastWilcoxTest::eDist3d( inp[[inp$order[1]]][ids] , inp[[inp$order[2]]][ids] , inp[[inp$order[3]]][ids], B )
		A= which(dists == max(dists))

		inp[[inp$order[2]]][ids[A]]

		alpha <- -atan((inp[[inp$order[2]]][ids[A]]- inp[[inp$order[2]]][ids[B]])/(inp[[inp$order[1]]][ids[A]]- inp[[inp$order[1]]][ids[B]]))
		rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
		M2 <- t(rotm %*% rbind(inp[[inp$order[1]]][ids] , inp[[inp$order[2]]][ids] ) )
		RET$time = order( M2[,1] )

		RET
	}

	#res = localLoess( 1:length(a), a, b, c)

	## Rather base this on slingshot's implementation.
	## But that needs groups.
	dat = cbind( a, b, c )
	opt = optGroupCountKmeans( dat )
	group = kmeans( dat , opt )
	dist_of_centers = FastWilcoxTest::eDist3d( group$centers[,'a'], group$centers[,'b'], group$centers[,'c'], group$cluster[1]-1 )
	end = which( dist_of_centers == max(dist_of_centers))

	sling = slingshot::slingshot(dat, group$cluster, start.clus= group$cluster[1], end.clus = end  ) ## assuming that the first cell selected should also be in the first cluster...
	slingTime = slingshot::slingPseudotime( sling )
	## I am interested in the longest slope
	use = 1
	if ( ncol(slingTime) > 1){
		tmp= apply( slingTime,2, function(x){ length(which(! is.na(x))) } )
		use = which(tmp == max(tmp))
	}
	o = order( slingTime[,use])
	
	res = data.frame( 
		time = slingTime[,use],
		order = order(slingTime[,use]),
		x = sling@curves[[use]]$s[,1],
		y = sling@curves[[use]]$s[,2],
		z = sling@curves[[use]]$s[,3],
		a = a,
		b = b,
		c = c,
	## to not break VR I need to restrict the number of colors here to 10!		
		col=  wesanderson::wes_palette("Zissou1", 10, type = "continuous")[ round(seq( from=1, to=9,  length.out = length(o)))[ order(o)] ] 
		)
	info = groupingInfo(x, grouping )
	res = new('cellexalTime', dat= res, drc=info$drc)
	res = check(res)

	#plot(res)

	# try({o = FastWilcoxTest::euclidian_order3d( res$x, res$y, res$z )},silent= TRUE)
	# time = FastWilcoxTest::euclidian_distances3d( res$x[o], res$y[o], res$z[o], sum=T )

	## add the time as group:

	#the VR program dependeds on it
	x = addSelection( res, x, grouping )

	## no colnames: cell name, color, drc name and selection id - fille with 0
	info = groupingInfo(x, grouping )
	## create the .time selection file for cellexalVR
	f= file.path( x@outpath, basename(info$selectionFile)) 
	f= paste(sep=".", f, 'time')
	exportSelection( res, f )

	if ( file.exists( x@usedObj$sessionPath ) ) {
		file.copy( f, x@usedObj$sessionPath)
		file.copy( paste( sep=".",f,'points'), x@usedObj$sessionPath)
	}

	return(x)
	
} )
