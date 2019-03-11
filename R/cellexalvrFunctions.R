#' @name loadObject
#' @aliases loadObject,cellexalvrR-method
#' @rdname loadObject-methods
#' @docType methods
#' @description  Loads the cellexalvr object, if the fname is a file
#' @param fname the file to load or a cellexalvr object
#' @param fname  TEXT MISSING
#' @param maxwait  TEXT MISSING default=50
#' @keywords load
#' @title description of function loadObject
#' @export loadObject
if ( ! isGeneric('loadObject') ){setGeneric('loadObject', ## Name
	function ( fname, maxwait=50 ) { 
		standardGeneric('loadObject') 
	}
) }

setMethod('loadObject', signature = c ('character'),
		definition = function ( fname, maxwait=50 ) {
			if ( file.exists( fname) ) {
				waited = 0
				while ( file.exists( paste(fname, 'lock',sep='.'))){
					Sys.sleep(1)
					waited = waited +1
					if ( waited == maxwait) { break }
				}
				if (waited != maxwait ){
					load(fname)
				}else {
					stop( paste("Could not obtain access to locked file", fname ))
				}
			}else {
				stop( paste( "file does not exixit", fname) )
			}
			if ( ! is.null(attributes(cellexalObj@class)$package) ) {
				if ( attributes(cellexalObj@class)$package == 'cellexalvr' ){
					class(cellexalObj) = 'cellexalvrR'
					cellexalObj = renew(cellexalObj)
				}
			}
			## old objects need an update
			if ( ! .hasSlot( cellexalObj, 'dat') ){
				new = MakeCellexaVRObj ( cellexalObj@data, mds.list = cellexalObj@mds,	specie=cellexalObj@specie,cell.metadata= cellexalObj@meta.cell, facs.data= NULL )
				new@userGroups = cellexalObj@userGroups
				new@colors = cellexalObj@colors
				new@groupSelectedFrom = cellexalObj@groupSelectedFrom
				new@userGroups = cellexalObj@userGroups
				new@usedObj = cellexalObj@usedObj
				new@tfs = cellexalObj@tfs
				new@index = cellexalObj@index
				rm(cellexalObj)
				cellexalObj = new
				rm(new)
				gc()
			}
			#tmp = new('cellexalvrR')
			#reload = 0
			
			tryCatch({file.exists(cellexalObj@outpath ) }, error= { 
						cellexalObj = renew(cellexalObj)
						cellexalObj@outpath = normalizePath(dirname( fname ))
					} )
			
			if ( ! file.exists(cellexalObj@outpath )) {
				cellexalObj@outpath = normalizePath(dirname( fname ))
			}else {
				cellexalObj@outpath = normalizePath(cellexalObj@outpath)
			}
			## there might be different other objects in the same path
			## integrat them now
			cellexalObj = integratParts( cellexalObj , normalizePath(dirname( fname )) )
			cellexalObj
		} )

setMethod('loadObject', signature = c ('cellexalvrR'),
		definition = function ( fname, maxwait=50 ) {
			return (fname)
} )
#' @name set.specie
#' @aliases set.specie,cellexalvrR-method
#' @rdname set.specie-methods
#' @docType methods
#' @description  Loads TF annotation into cellexalvr object
#' @param cellexalObj, cellexalvr object
#' @param specie The specie required
#' @param specie  TEXT MISSING default=c("mouse"
#' @param "human")  TEXT MISSING default=c("mouse"
#' @title description of function set.specie
#' @keywords TFs
#' @export set.specie
if ( ! isGeneric('set.specie') ){setGeneric('set.specie', ## Name
	function (cellexalObj, specie=c("mouse","human")) { 
		standardGeneric('set.specie') 
	}
) }

setMethod('set.specie', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, specie=c("mouse","human")) {
	
	if(specie=="mouse"){
		#data(mouse.tfs)
		cellexalObj@tfs <- mouse.tfs
	}
	
	if(specie=="human"){
		#data(human.tfs)
		cellexalObj@tfs <- human.tfs
	}

	cellexalObj@specie <- specie
	cellexalObj
} )
#' @name get.genes.cor.to
#' @aliases get.genes.cor.to,cellexalvrR-method
#' @rdname get.genes.cor.to-methods
#' @docType methods
#' @description  Gets positively and negatively correlated genes to a chosen gene
#' @param cellexalObj A cellexalvr object
#' @param gname The required gene
#' @param output the outfile
#' @param is.smarker Whether the supplied gene is a surface marker (default =F)
#' @param gname  TEXT MISSING
#' @param output  TEXT MISSING
#' @param is.smarker  TEXT MISSING default=F
#' @title description of function get.genes.cor.to
#' @keywords correlation
#' @export get.genes.cor.to
if ( ! isGeneric('get.genes.cor.to') ){setGeneric('get.genes.cor.to', ## Name
	function (cellexalObj, gname, output, is.smarker=F) { 
		standardGeneric('get.genes.cor.to') 
	}
) }

setMethod('get.genes.cor.to', signature = c ('character'),
		definition = function (cellexalObj, gname, output, is.smarker=F) {
			cellexalObj <- loadObject(cellexalObj)
			get.genes.cor.to( cellexalObj, gname, output, is.smarker )
		}
)

setMethod('get.genes.cor.to', signature = c ('cellexalvrR'),
	definition = function (cellexalObj, gname, output, is.smarker=F) {
	
	cellexalObj <- loadObject(cellexalObj)
	dat <- cellexalObj@dat
	rownames(dat) <- tolower(rownames(dat))
	
	goi <- NULL

	if(is.smarker==F){
		goi <- dat[tolower(gname),]
	}
	
	if(is.smarker==T){
		m <- match( tolower(gname), tolower(colnames(cellexalObj@index)))
		

		if(is.na(m)==T)
			stop("Gene name is not an available surface marker")

		goi <- cellexalObj@index[,m]
	}
	
	if ( is.null(goi)) {
		stop( paste("the gname", gname, "is neither a gene nor a fascs maker name", paste( colnames(cellexalObj@index), collapse=", ")) )
	}
	
	calc.cor <- function(v, comp){
		cor(v, comp)
	}
	
	cor.values <-  FastWilcoxTest::CorMatrix( dat, goi)
	names(cor.values) = rownames(dat)
	##cor.values <- apply(dat,1,calc.cor,comp=goi)
	
	ord <- names(sort(cor.values))
	
	pos <- ord[ (length(ord)-1): (length(ord)-10) ]
	neg <- ord[1:10]
	tab <- cbind(pos,neg)
	
	write.table(t(tab),output,row.names=F,col.names=F,sep="\t",quote=F)
} )
