logStatResult <- function( x, method, data ) {
	if (! is.null( x$usedObj$sessionName)) {
		## export the data into a file and add a small download link tnto the report
		ofile =   paste( x@usedObj$lastGroup,method,"csv", sep="\t"   )
		write.table( data, file= file.path( x$usedObj$sessionPath,'tables',ofile) , quote=F )
		mainOfile = cellexalObj@usedObj$sessionRmdFiles[1]
		
		cat( sep="\n",
				paste( "##", "Statistical result from Group ",  cellexalObj@usedObj$lastGroup ),
				
				"",
				paste(sep="",  "<a href='",file.path( 'png',ofile),"' download>",ofile,"</a>" ),
				""
				, file = mainOfile, append = TRUE)
	}
	invisible(x)
}