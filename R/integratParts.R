#' @name integrateParts
#' @aliases integrateParts,cellexalvrR-method
#' @rdname integrateParts-methods
#' @docType methods
#' @description integrate the parts that have been split from the main object.
#' @param x the cellexalObj to add to
#' @param path the path where the parts are stored (normally the @outpath)
#' @title description of function integrateParts
#' @export 
if ( ! isGeneric('integrateParts') ){setGeneric('integrateParts', ## Name
			function ( x , path=NULL ) { 
				standardGeneric('integrateParts')
			}
	) }

setMethod('integrateParts', signature = c ('cellexalvrR'),
		definition = function ( x , path=NULL ) {
			if ( is.null(path) )
				path = x@outpath
			## now we check a list of outpath slots that could be updated:
			F = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors' )
			for ( i in 1:length(F)  ) {
				
				filename = partFname( F[i] , path )
				
				if ( file.exists( filename ) ){
					#print ( paste("I am trying to load file ", filename ))
					
					load( filename )
					if (i == 1) {#sample.RData 
						x@meta.cell = sample
					}else if ( i == 2) {
						x@meta.gene = annotation
					}else if ( i == 3) {
						x@userGroups = userGroups
					}else if ( i == 4) {
						x@usedObj = usedObj
					}else if ( i==5) {
						x@groupSelectedFrom = groupSelectedFrom
					}else if ( i == 6 ) {
						x@colors = colors
					}
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
#' @param path the optional outpath (default x@outpath)
#' @title description of function savePart
#' @export 
if ( ! isGeneric('savePart') ){setGeneric('savePart', ## Name
			function ( x, part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors' ), path=NULL ) { 
				standardGeneric('savePart')
			}
	) }

setMethod('savePart', signature = c ('cellexalvrR'),
		definition = function ( x, part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors' ), path =NULL ) {
			#meta.cell meta.gene userGroups usedObj
			if ( is.null(path) )
				path = x@outpath
			
			filename = partFname( part, path )
			#print ( paste("I am saving parts file", filename ))
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
			}else if ( part == 'groupSelectedFrom') {
				groupSelectedFrom = x@groupSelectedFrom
				save( groupSelectedFrom, file=filename)
			}else if ( part == 'colors') {
				colors = x@colors
				save( colors, file=filename)
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
			function ( part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'all' ), path ) { 
				standardGeneric('partFname')
			}
	) }

setMethod('partFname', signature = c ('character'),
		definition = function ( part = c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors', 'all' ), path ) {
			F = c( 'sample.RData', 'usergroups.RData', 'annotation.RData', 'usedObj.RData' )
			fname=NULL
			if (part == 'meta.cell') {#sample.RData
				fname = 'sample'
			}else if ( part == 'meta.gene') {
				fname = 'annotation'	
			}else if ( part == 'userGroups') {
				fname = 'userGroups'
			}else if ( part == 'usedObj') {
				fname = 'usedObj'
			}else if ( part == 'groupSelectedFrom') {
				fname = 'groupSelectedFrom'
			}else if ( part == 'colors') {
				fname = 'colors'
			}else if ( part == 'all') {
				fname = unlist( lapply( c( 'meta.cell',  'meta.gene',  'userGroups',  'usedObj', 'groupSelectedFrom', 'colors'), partFname, path ))
				return( fname )
			}
			else {
				stop( paste( "This part can not be saved:",part ))
			}
			
			unlist( lapply(fname, function( FN ) { file.path( path, paste(sep="",'.' , FN, '.RData' ) )  } ) )
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
			#print ( paste(path, "I am cleaning the files:", paste( collapse=", ", partFname( 'all', path ))))
			for ( fname in partFname( 'all', path ) ) {
				if ( file.exists(fname))
					unlink( fname)
			}
		}
)
