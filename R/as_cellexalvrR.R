#' @name as_cellexalvrR
#' @aliases as_cellexalvrR,environment-method
#' @rdname as_cellexalvrR-methods
#' @docType methods
#' @description convert a BioData list (BioData libraray not loaded) into a cellexalvrR obejct
#' @param x the BioData 'object'
#' @param meta.cell.groups which x$samples column to convert to meta.cell classes
#' @param meta.genes.groups which annotation columns to keep (default NULL)
#' @param userGroups which x§samples columns to add to the userGroups slot
#' @param outpath set the outpath of the object (default getwd())
#' @param specie set the specie to either mouse or human (default check gene names)
#' @title convert a BioData object to cellexalvrR keeping all 3D mds objects.
#' @export 
setGeneric('as_cellexalvrR', ## Name
	function ( x, meta.cell.groups, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) { ## Argumente der generischen Funktion
		standardGeneric('as_cellexalvrR') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('as_cellexalvrR', signature = c ('environment'),
	definition = function ( x, meta.cell.groups, meta.genes.groups = NULL, userGroups=NULL, outpath=getwd(), specie ) {
	## x has to be a BioData object which is read as a simple list here!
	ret = methods::new('cellexalvrR')
	ret@dat = x$dat
	#ret@dat@x = log( exp( ret@dat@x ) +1 ) ## fixed in BioData
	
	if ( ! is.null(meta.genes.groups) )
		ret@mets.gene = x$annoatation[, meta.genes.groups]
	ret@meta.cell = make.cell.meta.from.df( x$samples[,meta.cell.groups] ,rq.fields= meta.cell.groups )
	rownames(ret@meta.cell) = colnames( ret@dat )
	t = data.frame(lapply( 
		x$usedObj$userGroups, 
		function(n) {
			OK = which(! is.na( x$samples[,n]))
			order=as.vector(x$samples[,n])
			order[OK] = 1:length(OK)
			list( x$samples[,n],order) 
		} ))
	
	colnames(t) = unlist(lapply( x$usedObj$userGroups, function (n) paste( n, c("", "order"))))
	ret@userGroups = t
	
	MDS <- names(x$usedObj)[grep ( 'MDS', names(x$usedObj))]
	OK = grep ( '_dim_' , MDS, invert= TRUE )
	if ( length(OK) == 0 ) {
		stop( "cellexalvrR does need at least one 3D MDS structure to work on - please create that first!")
	}
	for ( n in MDS[OK] ) {
		for ( n2 in names(x$usedObj[[n]]) ) {
			new_name = stringr::str_replace_all( n2, "\\s", "_")
			ret@mds[[new_name]] = x$usedObj[[n]][[n2]]
		}
	}
	ret@colors = x$usedObj$colorRange
	ret@specie=x$usedObj$specie
	
	bad = which( ret@dat@x < 0)
	if ( length(bad) > 0 ) {
		ret@dat@x[ bad ] = 0
		ret@dat = Matrix::drop0(ret@dat)
	}
	ret@outpath = outpath
	ret
} )
