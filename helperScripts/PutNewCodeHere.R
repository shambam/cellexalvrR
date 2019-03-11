integratParts <- function ( x , path=NULL ) {
	if ( is.null(path) )
		path = x@outpath
	## now we check a list of outpath slots that could be updated:
	F = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj' )
	for ( i in 1:length(F)  ) {
		
		filename = partFname( F[i] , path )
		
		if ( file.exists( filename ) ){
			load( filename )
		}
		if (i == 1) {#sample.RData 
			if ( exists( 'samples' ) )
				x@meta.cell = samples		
		}else if ( i == 2) {
			if ( exists( 'userGroups' ) )
				x@userGroups = userGroups
		}else if ( i == 3) {
			if ( exists( 'annotation' ) )
				x@meta.gene = userGroups
		}else if ( i == 4) {
			if ( exists( 'usedObj' ) )
				x@usedObj@usedObj = usedObj
		}
	}
	invisible( x )
}

partFname <- function( part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj' ), path ) {
	F = c( 'sample.RData', 'usergroups.RData', 'annotation.RData', 'usedObj.RData' )
	fname=NULL
	if (part == 'meta.cell') {#sample.RData
		fname = 'sample'
	}else if ( part == 'meta.gene') {
		fname = 'userGroups'	
	}else if ( part == 'userGroups') {
		fname = 'annotation'
	}else if ( part == 'usedObj') {
		fname = 'usedObj'
	}
	else {
		stop( paste( "This part can not be saved:",part ))
	}
		
	file.path( path, paste(sep="",'.' , fname, 'RData' ) )
}

savePart <- function ( x, part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj' ) ) {
	#meta.cell meta.gene userGroups usedObj
	filename = partFname( F[i] , path )
	
	if (part == 'meta.cell') {#sample.RData
		sample = x@meta.cell
		save( sample, file=filename)
	}else if ( part == 'meta.gene') {
		annotation = x@meta.gene
		save( annotation, file=filename)
	}else if ( part == 'userGroups') {
		userGroups = x@userGroups
		save( userGroups, file=filename)
	}else if ( part == 'usedObj') {
		usedObj = x@usedObj
		save( usedObj, file=filename)
	}
	
	invisible( x )
}