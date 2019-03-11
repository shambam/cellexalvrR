#' @name integratParts
#' @aliases integratParts,cellexalvrR-method
#' @rdname integratParts-methods
#' @docType methods
#' @description integrate the parts that have been split from the main object.
#' @param x the cellexalObj to add to
#' @param path the path where the parts are stored (normally the @outpath)
#' @title description of function integratParts
#' @export 
if ( ! isGeneric('integratParts') ){setGeneric('integratParts', ## Name
	function ( x , path=NULL ) { 
		standardGeneric('integratParts')
	}
) }

setMethod('integratParts', signature = c ('cellexalvrR'),
	definition = function ( x , path=NULL ) {
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
} )

#' @name savePart
#' @aliases savePart,cellexalvrR-method
#' @rdname savePart-methods
#' @docType methods
#' @description save only a part of the cellexal object increasing the VR interaction speed
#' @param x  TEXT MISSING
#' @param part  TEXT MISSING default= c( 'meta.cell'
#' @param 'meta.gene'  TEXT MISSING
#' @param 'userGroups'  TEXT MISSING
#' @param 'usedObj')  TEXT MISSING
#' @title description of function savePart
#' @export 
if ( ! isGeneric('savePart') ){setGeneric('savePart', ## Name
			function ( x, part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj' ) ) { 
				standardGeneric('savePart')
			}
	) }

setMethod('savePart', signature = c ('cellexalvrR'),
		definition = function ( x, part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj' ) ) {
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
		} )


#' @name partFname
#' @aliases partFname,cellexalvrR-method
#' @rdname partFname-methods
#' @docType methods
#' @description returns the possible outfile for the partial saving of the cellexal object
#' @param part to get the fanme for (  'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'all' )
#' @param path the outpath
#' @title description of internally used function partFname
#' @export 
if ( ! isGeneric('partFname') ){setGeneric('partFname', ## Name
			function ( part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'all' ), path ) { 
				standardGeneric('partFname')
			}
	) }

setMethod('partFname', signature = c ('character'),
		definition = function ( part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'all' ), path ) {
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
			}else if ( part == 'usedObj') {
				fname = unlist( lapply( c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj'), partFname, path ))
			}
			else {
				stop( paste( "This part can not be saved:",part ))
			}
			
			unlist( lapply(fname, function( FN ) { file.path( path, paste(sep="",'.' , FN, 'RData' ) )  } ) )
		} )

#' @name cleanParts
#' @aliases cleanParts,cellexalvrR-method
#' @rdname cleanParts-methods
#' @docType methods
#' @description remove all parts from the file system
#' @param path the outpath
#' @title cleanParts removes all parts when storing the whole object.
#' @export 
if ( ! isGeneric('cleanParts') ){setGeneric('cleanParts', ## Name
			function ( path ) { 
				standardGeneric('cleanParts')
			}
	) }

setMethod('cleanParts', signature = c ('character'),
		definition = function ( path ) {
			for ( fname in partFname( 'all', path ) ) {
				if ( file.exists(fname))
					unlink( fname)
			}
		}
)
