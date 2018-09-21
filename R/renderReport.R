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
	fileConn<-file( '_bookdown.yml' )
	writeLines(c(
		paste('book_filename:', cellexalObj@usedObj$sessionName),
		'output_dir: ../',			
		'delete_merged_file: true' 
        ), fileConn )
    close(fileConn)
	oldwd = getwd()
	tryCatch({
		setwd( sessionPath )
		fileConn<-file( '_bookdown.yml' )
		writeLines(c(
						paste('book_filename:', cellexalObj@usedObj$sessionName),
						'output_dir: ./',			
						'delete_merged_file: true' 
				), fileConn )
		close(fileConn)
		try({ bookdown::render_book( cellexalObj@usedObj$sessionRmdFiles, "bookdown::gitbook" ) }, FALSE)

	},finally = {
		setwd(oldwd)
	}) 


	cellexalObj@usedObj$sessionName = NULL
	
	lockedSave(cellexalObj)
	
	cellexalObj
} )



setMethod('renderReport', signature = c ('character'),
		definition = function (cellexalObj) {
			cellexalObj <- loadObject(cellexalObj)
			renderReport(cellexalObj )
		}
)