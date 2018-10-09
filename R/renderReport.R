#' @name renderReport
#' @aliases renderReport,cellexalvrR-method
#' @rdname renderReport-methods
#' @docType methods
#' @description after one session this function renders the output gitbook
#' @param cellexalObj the cellexlvrR object
#' @param ofile the html filename (default filename( cellexalObj@usedObj$sessionName) )
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
	
	#this part is so buggy I need to export it into a new thread
	## first a short script
	fileConn<-file(file.path(sessionPath, 'knit.R' ) )
	
	pathSept <- str_replace_all( cellexalObj@outpath , .Platform$file.sep, '/' )
	
	writeLines(c(
					"library(cellexalvrR)",
					"library(methods)",
					paste( sep="","sessionPath = '",pathSept ,"'"),
					"cellexalObj = loadObject( file.path( sessionPath, 'cellexalObj.RData') )" ,
					"setwd('sessionPath')",
					"message ( getwd())",
					"files = as.character(unlist(lapply( cellexalObj@usedObj$sessionRmdFiles, basename)))",
					"message( paste( files ) )",
					"bookdown::render_book( input=files, output_format='bookdown::gitbook', clean_envir = FALSE )"
			), fileConn )
	close(fileConn)
	
	print ( paste(Sys.which('R') ," CMD BATCH", file.path(sessionPath, 'knit.R' ) ) )
	system( paste( Sys.which('R') ," CMD BATCH", file.path(sessionPath, 'knit.R') ) )

	for ( i in 1:6 ){
		if (file.exists( file.path(sessionPath, paste(cellexalObj@usedObj$sessionName, sep='.', 'html') )) ){
			last
		}
		Sys.sleep(10)
	}
	
	if ( file.exists( file.path(sessionPath, paste(cellexalObj@usedObj$sessionName, sep='.', 'html')) )){
		cellexalObj@usedObj$sessionPath = cellexalObj@usedObj$sessionRmdFiles = cellexalObj@usedObj$sessionName = NULL
		lockedSave(cellexalObj)
	}else {
		print ( "some error has occured - output html file was not created!" )
	}	
	
	cellexalObj
} )



setMethod('renderReport', signature = c ('character'),
		definition = function (cellexalObj) {
			cellexalObj <- loadObject(cellexalObj)
			renderReport(cellexalObj )
		}
)