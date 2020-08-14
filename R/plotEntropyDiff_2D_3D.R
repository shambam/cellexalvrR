#' plots the first row of a table into two png xy line plots based 
#' on two tables in double log scale (1) and scaled to the last value in row 2 (2)
#' Iedally these tables have been produced by a call to NaiveEntropySpherical to qualify the entropy of a 2D and 3D mds plot.
#'
#' @name plotEntropyDiff_2D_3D
#' @aliases plotEntropyDiff_2D_3D,cellexalvrR-method
#' @rdname plotEntropyDiff_2D_3D-methods
#' @docType methods
#' @description plots the dcr entropy as shown in the paper
#' @param entro2D  a 2D entropy calculation
#' @param entro3D 2 3D entropy calculation
#' @param file the plot outfile base name
#' @param size the hight and width of the png files default=800
#' @title description of function plotEntropyDiff_2D_3D
#' @export 
setGeneric('plotEntropyDiff_2D_3D', ## Name
	function (entro2D, entro3D, file, size=800 ) { ## Argumente der generischen Funktion
		standardGeneric('plotEntropyDiff_2D_3D') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('plotEntropyDiff_2D_3D', signature = c ('matrix'),
	definition = function (entro2D, entro3D, file, size=800 ) {
	file = stringr::str_replace( file, '.png$', '')
	png( file= paste( sep="", file, '.png'), width=size, height=size)
	plot(log(as.numeric(colnames(entro2D))), log(entro2D[1,]), ylim=c(log(min( c( entro2D[1,], entro3D[1,]))),log( max( c( entro2D[1,], entro3D[1,])))),
  	   xlab='rel max distance [log]', ylab="cummulative entropy [log]",
  	   main= paste("2D vs 3D (red) entropy difference - euclidian spheres",entro2D[2,ncol(entro2D)], "cells"), type='l')
	lines(log(as.numeric(colnames(entro3D))),log( entro3D[1,]) , col='red')
	dev.off()

	max = entro2D[1,ncol(entro2D)]

	png( file= paste( sep="", file,'_scaled', '.png'), width=size, height=size)
	plot(log(as.numeric(colnames(entro2D))), log(entro2D[1,]/max), ylim=c(log(min( c( entro2D[1,], entro3D[1,]))/max),log( max( c( entro2D[1,], entro3D[1,]))/max)),
  	   xlab='rel max distance [log]', ylab="cummulative entropy [log]",
  	   main= paste("2D vs 3D (red) entropy difference - euclidian spheres",entro2D[2,ncol(entro2D)], "cells"), type='l')
	lines(log(as.numeric(colnames(entro3D))),log( entro3D[1,] /max ) , col='red')
	dev.off()

	## knee point analysis:
	png( file= paste( sep="", file,'_kneePoint', '.png'), width=size, height=size)
	plot( log(as.numeric(colnames(entro2D))), log(entro2D[1,] / entro2D[2,] ), 
		xlab="rel max distance [log]", ylab="cummulative entropy / selected cells [log]",
		main="Knee point analysis")
    d=  log(entro2D[1,] / entro2D[2,])
    cut2D = as.numeric(names(which(d==max(d))))[1]
    abline( v=log(cut2D), col='black')
    points( log(as.numeric(colnames(entro3D))), log(entro3D[1,] / entro3D[2,] ), col='red')
    d=  log(entro3D[1,] / entro3D[2,])
    cut3D = as.numeric(names(which(d==max(d))))[1]
    abline( v=log(cut3D), col='red')
    legend( 'topright', legend= c(
    	paste( collapse=" / ", 
    		round(entro2D[, as.character(cut2D)],2)
    	), 
    	paste( collapse=" / ", 
    		round(entro3D[, as.character(cut3D)],2 ) 
    		)) , col=c('black', 'red') , lty=1:2, cex=0.8)
    dev.off()

    png( file= paste( sep="", file,'_scaledToMeanCellsSelected', '.png'), width=size, height=size)
    plot( log(entro2D[1,]), log(entro2D[2,]), 'l', xlab='mean cells in range [log]', ylab='total entropy [log]')
	lines( log(entro3D[1,]), log(entro3D[2,]), col='red')
	dev.off()

} )
