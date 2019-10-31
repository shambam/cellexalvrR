#' @name renderFile
#' @aliases renderFile,cellexalvrR-method
#' @rdname renderFile-methods
#' @docType methods
#' @description render only one html section, not the whole session log
#' @param x  the cellexalvrR object
#' @param id the id of the report file to render ('x@usedObj$sessionRmdFiles[id]')
#' @title description of function renderFile
#' @export 
setGeneric('renderFile', ## Name
	function ( x, id ) { ## Argumente der generischen Funktion
		standardGeneric('renderFile') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('renderFile', signature = c ('cellexalvrR'),
	definition = function ( x, id ) {

if ( is.null( x@usedObj$sessionPath )){
		x = sessionPath(x) #function definition in file 'sessionPath.R'
	}
		sessionPath = normalizePath(x@usedObj$sessionPath)

	fname = x@usedObj$sessionRmdFiles[id]
	if ( ! file.exists(fname) ){
		stop( paste( "fname for the sessionRmdFiles id",id,",", fname,"does not exists on the file system"))
	}
	
	fileConn<-file(file.path(sessionPath, '_bookdown.yml') )
	writeLines(c(
		paste('book_filename:', paste(id, x@usedObj$sessionName, sep="_" )),
		paste('output_dir: ../',			
		'delete_merged_file: true' )
		, fileConn 
	) )
    close(fileConn)
    message( paste('bookdown::render_book log id', id) )
	## and now a bloody hack:
	oldwd = getwd()
	setwd( x@usedObj$sessionPath )
	if ( id != 1 ) {
		files = as.character(unlist(lapply( x@usedObj$sessionRmdFiles[c(1,id)], basename)))
	}else {
		files =  basename(x@usedObj$sessionRmdFiles[1])
	}
	
	cmd = paste( sep="","rmarkdown::render(input=",file2Script(fname),", output_format= 'html_document', output_file='",
		paste(id, x@usedObj$sessionName, sep='_' ),"', output_dir='../')")

	script = paste( sep="_", id,"runRender.R")
	if ( file.exists( script)) {
		unlink( script )
	}
	cat( cmd, file=script, append=FALSE )
	Rscript = file.path( R.home(),"bin","Rscript" )
	if ( ! file.exists(Rscript)){
		Rscript = file.path( R.home(),"bin","Rscript.exe" )
	}
	Rscript = paste(sep="", '"', Rscript,'"')
	
	system( paste(Rscript, script ))
	message( paste('bookdown::render_book log id', id, 'finished') )

	setwd( oldwd )
	x
} )


#' @name storeLogContents
#' @aliases storeLogContents,cellexalvrR-method
#' @rdname storeLogContents-methods
#' @docType methods
#' @description store the Rmd contens produced in a logXYZ function into a file and register that one.
#' @param x the CellexalvrR object
#' @param content the text content of the Rmd file
#' @title description of function storeLogContents
#' @export 
setGeneric('storeLogContents', ## Name
	function ( x, content ) { ## Argumente der generischen Funktion
		standardGeneric('storeLogContents') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('storeLogContents', signature = c ('cellexalvrR'),
	definition = function ( x, content ) {

	if ( is.null( x@usedObj$sessionPath )){
		x = sessionPath(x) #function definition in file 'sessionPath.R'
	}
	sessionPath = normalizePath(x@usedObj$sessionPath)
	id = length(x@usedObj$sessionRmdFiles) +1
	fname = paste( sep="_", id, "paritalLog.Rmd" )
	fname = file.path( sessionPath, fname )

	x@usedObj$sessionRmdFiles = c(x@usedObj$sessionRmdFiles, fname )
	
	if ( file.exists(fname) ){
		stop(paste( "The outfile already existed!" , fname))
	}
	
	fileConn<-file( fname )
	writeLines( content, fileConn )
    close(fileConn)
    x
} )
