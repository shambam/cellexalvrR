#' A function checking whether VR is run at the same time as the R process.
#' @name VRmode
#' @aliases VRmode,character-method
#' @rdname VRmode-methods
#' @docType methods
#' @description Windows specific check if CellexalVR.exe is running
#' @returns boolean value if the VR application is running. 
#' @keywords VRmode
#' @title check if CellexalVR.exe is running (Windows specific)
#' @export 
#if ( ! isGeneric('renew') ){
setGeneric('VRmode', ## Name
			function ( file, sleepT=1, debug=FALSE, masterPID = NULL, asFunction =FALSE ) { 
				standardGeneric('VRmode') 
			}
)
#}

setMethod('VRmode', signature = c (),
		definition =  function(file, sleepT=1, debug=FALSE, masterPID = NULL, asFunction =FALSE ){
	tryCatch ( {
		task <- shell('tasklist /fi "imagename eq CellexalVR.exe" /nh /fo csv',
			intern = TRUE)
		OK = grepl("CellexalVR.exe", task)
		}, 
	warning = function(err){
		## likely not working! likely tasklist not available and hence 
		OK = FALSE
    	}
	)
	OK
})