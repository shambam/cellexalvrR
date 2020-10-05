plotViolin <- function(x, gene, grouping, ofile=NULL, width=800, height=800, main=NULL, X11type= 'cairo', family="Helvetica"  ) {

	x <- loadObject(x) #function definition in file 'lockedSave.R'
	x <- userGrouping(x, cellidfile) #function definition in file 'userGrouping.R'

	ok <- which(!is.na(x@userGroups[,x@usedObj$lastGroup]))
	if ( length(ok) > 0) {
		loc <- reduceTo (x, what='col', to=colnames(x@data)[ ok ] ) #function definition in file 'reduceTo.R'
	}else {
		loc <- x
	}
	names= unique( loc@userGroups[,x@usedObj$lastGroup])
	data = lapply( names , 
		function( name ){
			loc@data[gene, which(loc@userGroups[,x@usedObj$lastGroup] == name)]
		} )
	names(data)[0] = 'x'
	col = NULL
	for (names in names) {
		col = c(col, x@colors[[x@usedObj$lastGroup]][name]
	}
	if ( !is.null(ofile) ) {
		grDevices::pdf( file=paste(ofile ,'pdf',sep='.'), width=width, height=height, family=family)
	}
	vioplot( data, names=names, col=col, main=main )
	if ( !is.null(ofile) ){
		grDevices::dev.off()
	}
	message('Done')
	invisible(x)
}
