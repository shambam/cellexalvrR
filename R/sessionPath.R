#' @name sessionPath
#' @aliases sessionPath,cellexalvrR-method
#' @rdname sessionPath-methods
#' @docType methods
#' @description Use the session ID and object outpath to create a session path for the reports
#' @param cellexalObj the cellexal object
#' @param sessionName the session ID default=NULL
#' @title description of function sessionPath
#' @export 
setGeneric('sessionPath', ## Name
	function (cellexalObj, sessionName=NULL ) { ## Argumente der generischen Funktion
		standardGeneric('sessionPath') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('sessionPath', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, sessionName=NULL ) {
	if ( ! is.null(sessionName) ){
		if ( is.null(cellexalObj@usedObj$sessionName)){
			cellexalObj@usedObj$sessionName = sessionName
			cellexalObj@usedObj$sessionRmdFiles = c()
			lockedSave( cellexalObj)
		}else if ( ! cellexalObj@usedObj$sessionName == sessionName)  {
			cellexalObj@usedObj$sessionName = sessionName
			cellexalObj@usedObj$sessionRmdFiles = c()
			lockedSave( cellexalObj)
		}
	}
	if ( is.null(cellexalObj@usedObj$sessionName )) {
		cellexalObj@usedObj$sessionName = filename( as.character(Sys.time()))
	}
	opath = file.path(cellexalObj@outpath, cellexalObj@usedObj$sessionName)
	if ( ! file.exists( opath ) ){
		dir.create( opath )
		dir.create( file.path( opath, 'png') )
		dir.create( file.path( opath, 'tables' ))
	}
	cellexalObj@usedObj$sessionPath = opath
	cellexalObj
} )

setMethod('sessionPath', signature = c ('character'),
		definition = function (cellexalObj, sessionName=NULL) {
			cellexalObj <- loadObject(cellexalObj)
			logNetwork(cellexalObj, sessionName )
		}
)
