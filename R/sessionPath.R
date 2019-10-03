#' VR method that creates the session specific settings for each VR session.
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
		function (cellexalObj, sessionName=NULL ) { 
			standardGeneric('sessionPath')
		}
)

setMethod('sessionPath', signature = c ('cellexalvrR'),
		definition = function (cellexalObj, sessionName=NULL ) {
			
			
			if ( ! is.null(sessionName) ){
				if ( is.null(cellexalObj@usedObj$sessionName)){
					cellexalObj@usedObj$sessionName = sessionName
					cellexalObj@usedObj$sessionRmdFiles = NULL
					cellexalObj@usedObj$sessionPath = NULL
					cellexalObj@usedObj$sessionCounter = NULL
				}else if ( ! cellexalObj@usedObj$sessionName == sessionName)  {
					cellexalObj@usedObj$sessionName = sessionName
					cellexalObj@usedObj$sessionRmdFiles = NULL
					cellexalObj@usedObj$sessionPath = NULL
					cellexalObj@usedObj$sessionCounter = NULL
					#lockedSave( cellexalObj) #function definition in file 'lockedSave.R'
				}
			}
			if ( is.null(cellexalObj@usedObj$sessionName )) {
				browser()
				cellexalObj@usedObj$sessionName = filename( as.character(Sys.time())) #function definition in file 'filename.R'
				cellexalObj@usedObj$sessionRmdFiles = NULL
				cellexalObj@usedObj$sessionPath = NULL
				cellexalObj@usedObj$sessionCounter = NULL
			}
			if ( is.null(cellexalObj@usedObj$sessionPath) ) {
				## init the session objects
				## add a simple session log start file
				cellexalObj@usedObj$sessionPath = file.path(cellexalObj@outpath, cellexalObj@usedObj$sessionName)
				if (! dir.exists(cellexalObj@usedObj$sessionPath) )  {
					message( paste("I try to create the session path here! - ", cellexalObj@usedObj$sessionPath ))
					dir.create( cellexalObj@usedObj$sessionPath, recursive = TRUE)
					dir.create( file.path( cellexalObj@usedObj$sessionPath, 'png'), recursive = TRUE)
					dir.create( file.path( cellexalObj@usedObj$sessionPath, 'tables'), recursive = TRUE)
				}
				mainOfile = file.path( cellexalObj@usedObj$sessionPath , filename( "00.SessionStart.Rmd" ) ) #function definition in file 'filename.R'
				cellexalObj@usedObj$sessionRmdFiles = c(mainOfile)
				savePart(cellexalObj,part = 'usedObj' ) #function definition in file 'integrateParts.R'
				
				fileConn<-file( mainOfile )
				writeLines(c(paste(
										"# Session Log for Session", cellexalObj@usedObj$sessionName ),
								paste("Analysis of data: ", basename(cellexalObj@outpath) ),
								""
						)
						, fileConn  )
				close(fileConn)
				
				cellexalObj@usedObj$sessionPath = normalizePath( cellexalObj@usedObj$sessionPath )
			}
			
			invisible(cellexalObj)
			
		} )
#' @describeIn sessionPath cellexalvrR
#' @docType methods
#' @description preload the cellexalOvh.RData file 
#' @param cellexalObj the cellexalOvh.RData file 
#' @param sessionName the session ID default=NULL
#' @title description of function sessionPath
#' @export
setMethod('sessionPath', signature = c ('character'),
		definition = function (cellexalObj, sessionName=NULL) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			sessionPath(cellexalObj, sessionName ) #function definition in file 'sessionPath.R'
		}
)
