
plotEntropyDiff_2D_3D <- function(entro2D, entro3D, file, size=800 ){
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

}
