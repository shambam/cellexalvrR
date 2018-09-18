#' Saving the RData in the VR tool might create a problem. Hence this function will save the cellexalObj in a controlled way.
#'@param cellexalObj A cellexalvr object
#'@param path the output path
#'@keywords lockedSave
#'@export lockedSave

lockedSave <- function(cellexalObj, path ) {
	ofile = file.path( path, 'cellexalObj.RData' )
	lockFile = file.path( paste(ofile, 'lock', sep= '.'))
	while ( file.exists(lockFile) ){
		Sys.sleep(1)
	}
	file.create(lockFile)
	save(cellexalObj, file=ofile)
	file.remove(lockFile)
	print (paste("saved the object to",path))
}

