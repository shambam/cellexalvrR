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
		cellexalObj = sessionPath(cellexalObj)
	}
	sessionPath = normalizePath(cellexalObj@usedObj$sessionPath)
	cellexalObj@usedObj$sessionRmdFiles = unique( cellexalObj@usedObj$sessionRmdFiles )
	for ( i in 1:length(cellexalObj@usedObj$sessionRmdFiles) ){
		cellexalObj@usedObj$sessionRmdFiles[i] = normalizePath(cellexalObj@usedObj$sessionRmdFiles[i])
	}
	lockedSave( cellexalObj)
	
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
	bookdown::render_book( input=files, output_format='bookdown::gitbook', clean_envir = FALSE )
	setwd( oldwd )
	
	
#	#this part is so buggy I need to export it into a new thread
#	## first a short script
#	cmd = paste(
#			Sys.which('Rscript') , 
#			file.path(sessionPath, 'knit.R' ), 
#			file.path( cellexalObj@outpath , 'cellexalObj.RData') 
#	)
#	fileConn<-file(file.path(sessionPath, 'knit.R' ) )
#		
#	writeLines(c(
#					"args <- commandArgs(trailingOnly = TRUE)",
#					"library(cellexalvrR)",
#					"#this should now have this useless comment",
#					"cellexalObj = loadObject( args[1] )" ,
#					"setwd(cellexalObj@usedObj$sessionPath)",
#					"message ( getwd())",
#					"files = as.character(unlist(lapply( cellexalObj@usedObj$sessionRmdFiles, basename)))",
#					"message( paste( files ) )",
#					"bookdown::render_book( input=files, output_format='bookdown::gitbook', clean_envir = FALSE )",
#					paste( sep=" ", "#", cmd )
#			), fileConn )
#	close(fileConn)
#	
#	#print ( cmd )
#	system( 
#			paste(
#					Sys.which('Rscript') , 
#					file.path(sessionPath, 'knit.R' ), 
#					file.path( cellexalObj@outpath , 'cellexalObj.RData') 
#			)
#			)
#
#	for ( i in 1:6 ){
#		if (file.exists( file.path(sessionPath, paste(cellexalObj@usedObj$sessionName, sep='.', 'html') )) ){
#			last
#		}
#		Sys.sleep(10)
#	}
	expected_outfile = file.path(sessionPath, '..', paste('session-log-for-session-',tolower(cellexalObj@usedObj$sessionName), sep='', '.html'))
	if ( file.exists( expected_outfile )){
		cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL
		savePart(cellexalObj,part = 'usedObj' )
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
			cellexalObj <- loadObject(cellexalObj)
			renderReport(cellexalObj )
		}
)