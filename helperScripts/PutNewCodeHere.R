mdsPlot2D <- function( cellexalObj, gInfo ) {
	sessionPath = sessionPath(cellexalObj)
	MDS1 = file.path( sessionPath , 'png', filename(c( gInfo$grouping,gInfo$mds, "1_2", 'png' ) ))
	if ( ! file.exists( MDS1 ) ){
		png( file= MDS1, width=1000, height=1000)
		plot(
				cellexalObj@mds[[gInfo$mds]][,1], cellexalObj@mds[[gInfo$mds]][,2], c('grey',gInfo$col)[ gInfo$grouping+1 ],
				main = paste( gInfo$mds ), xlab="dimension 1", ylab= "dimension 2" )
		dev.off()
	}
	MDS2 = file.path( sessionPath , 'png', filename(c( gInfo$grouping,gInfo$mds, "2_3", 'png' ) ))
	if ( ! file.exists( MDS2 ) ){
		png( file= MDS2, width=1000, height=1000)
		plot(
				cellexalObj@mds[[gInfo$mds]][,2], cellexalObj@mds[[gInfo$mds]][,3], c('grey',gInfo$col)[ gInfo$grouping+1 ],
				main = paste( gInfo$mds ), xlab="dimension 2", ylab= "dimension 3" )
		dev.off()
	}
	c( MDS1, MDS2)
}
