#' renderReport is the final step to create a html log file.
#' 
#' These log files can be accessed from within the VR environment using the inbuilt browser.
#' 
#' @name renderReport
#' @aliases renderReport,cellexalvrR-method
#' @rdname renderReport-methods
#' @docType methods
#' @description after one session this function renders the output gitbook
#' @param cellexalObj the cellexlvrR object
#' @title description of function renderReport
#' @export 
setGeneric('renderReport', ## Name
	function (cellexalObj) { 
		standardGeneric('renderReport')
	}
)

setMethod('renderReport', signature = c ('cellexalvrR'),
	definition = function (cellexalObj) {
	## here you should know which files to render  ;-)
	if ( is.null( cellexalObj@usedObj$sessionPath )){
		cellexalObj = sessionPath(cellexalObj) #function definition in file 'sessionPath.R'
	}
	sessionPath = normalizePath(cellexalObj@usedObj$sessionPath)
	cellexalObj@usedObj$sessionRmdFiles = unique( cellexalObj@usedObj$sessionRmdFiles )
	for ( i in 1:length(cellexalObj@usedObj$sessionRmdFiles) ){
		cellexalObj@usedObj$sessionRmdFiles[i] = normalizePath(cellexalObj@usedObj$sessionRmdFiles[i])
	}
	lockedSave( cellexalObj) #function definition in file 'lockedSave.R'
	
	fileConn<-file(file.path(sessionPath,  '_bookdown.yml') )
	writeLines(c(
		paste('book_filename:', cellexalObj@usedObj$sessionName),
		'output_dir: ../',			
		'delete_merged_file: true' 
        ), fileConn )
    close(fileConn)
	
	oldwd = getwd()
	setwd( cellexalObj@usedObj$sessionPath )
	files = as.character(unlist(lapply( cellexalObj@usedObj$sessionRmdFiles, basename)))
	message( 'bookdown::render_book' )
	## and now a bloody hack:
	cmd = paste(sep="", "bookdown::render_book( input= c('",paste(collapse="', '",files),
		"'), output_format='bookdown::gitbook', clean_envir = FALSE , config_file = '_bookdown.yml' )" )
	script= 'runRender.R'
	if ( file.exists( script)) {
		unlink( script )
	}
	cat( cmd, file=script , append=FALSE)
	Rscript = file.path( R.home(),"bin","Rscript" )
	if ( ! file.exists(Rscript)){
		Rscript = file.path( R.home(),"bin","Rscript.exe" )
	}
	Rscript = paste(sep="", '"', Rscript,'"')
	system( paste(Rscript, script))
	#bookdown::render_book( input=files, , 
	setwd( oldwd )
	
	expected_outfile = file.path(sessionPath, '..', paste(cellexalObj@usedObj$sessionName, sep='', '.html'))
	if ( file.exists( expected_outfile )){
		cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL
		savePart(cellexalObj,part = 'usedObj' ) #function definition in file 'integrateParts.R'
	}else {
		print ( paste( "some error has occured - output ",expected_outfile," file was not created!" ))
	}	
	
	cellexalObj
} )


#' @describeIn renderReport cellexalvrR
#' @docType methods
#' @description preload the cellexalObj
#' @param cellexalObj the cellexal.RData file 
#' @title description of function renderReport
#' @export 
setMethod('renderReport', signature = c ('character'),
		definition = function (cellexalObj) {
			cellexalObj <- loadObject(cellexalObj) #function definition in file 'lockedSave.R'
			renderReport(cellexalObj ) #function definition in file 'renderReport.R'
		}
)


