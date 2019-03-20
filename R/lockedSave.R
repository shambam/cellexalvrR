#' @name lockedSave
#' @aliases lockedSave,cellexalvrR-method
#' @rdname lockedSave-methods
#' @docType methods
#' @description  Saving the RData in the VR tool might create a problem. Hence this function will
#' @description  save the cellexalObj in a controlled way. Locked save removes all parts from the file system.
#' @param cellexalObj, cellexalvr object
#' @param path the output path
#' @param what which part needs saving? (default NULL == all)
#' @title description of function lockedSave
#' @keywords lockedSave
#' @export lockedSave
if ( ! isGeneric('lockedSave') ){setGeneric('lockedSave', ## Name
	function (cellexalObj, path=NULL, what=NULL ) {
		standardGeneric('lockedSave')
	}
) }

setMethod('lockedSave', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, path=NULL ) {
		if ( is.null(path) ){
			path= cellexalObj@outpath
		}else if ( ! methods::.hasSlot(cellexalObj, "outpath") ) {
			cellexalObj@outpath = path
		}
	ofile = file.path( path, 'cellexalObj.RData' )
	lockFile = file.path( paste(ofile, 'lock', sep= '.'))
	while ( file.exists(lockFile) ){
		Sys.sleep(1)
	}
	file.create(lockFile)
	save(cellexalObj, file=ofile)
	## and now I should remove all parts...
	cleanParts ( path )
	file.remove(lockFile)
	
	print (paste("saved the object to",path))
} )


#' @name lockedLoad
#' @aliases lockedLoad,cellexalvrR-method
#' @rdname lockedLoad-methods
#' @docType methods
#' @description  Loading the RData in the VR tool might create a problem. Hence this function will
#'   save the cellexalObj in a controlled way. 
#' @param cellexalObj the file containing the cellexalObj data
#' @title description of function lockedSave
#' @keywords lockedSave
#' @export lockedSave
if ( ! isGeneric('lockedLoad') ){setGeneric('lockedLoad', ## Name
			function (cellexalObj ) {
				standardGeneric('lockedLoad' )
			}
	) }

setMethod('lockedLoad', signature = c ('character'),
		definition = function (cellexalObj ) {

			lockFile = file.path( paste(cellexalObj, 'lock', sep= '.'))
			while ( file.exists(lockFile) ){
				Sys.sleep(1)
			}
			path = dirname(cellexalObj)
			load( cellexalObj )
			if ( is.na('dat', methods::slotNames(cellexalObj)) ){
				new = MakeCellexaVRObj ( cellexalObj@data, mds.list = cellexalObj@mds,
						specie=cellexalObj@specie,cell.metadata= cellexalObj@meta.cell, facs.data= cellexalObj@index )
			}
			if ( is.null(cellexalObj@outpath) ){
				cellexalObj@outpath = normalizePath( path )
			}
			if ( ! file.exists( cellexalObj@outpath) ) {
				cellexalObj@outpath = normalizePath( path )
			}
			
			cellexalObj
		} )
