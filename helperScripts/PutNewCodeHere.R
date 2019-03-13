as_cellexalvrR <- function( x, meta.cell.groups, meta.genes.groups = NULL, userGroups=NULL ) {
	## x has to be a BioData object which is read as a simple list here!
	ret = new('cellexalvrR')
	ret@dat = x$dat
	if ( ! is.null(meta.genes.groups) )
		ret@mets.gene = x$annoatation[, meta.genes.groups]
	ret@meta.cell = make.cell.meta.from.df( x$samples[,meta.cell.groups])
	if ( ! is.null( userGroups )) {
		ret@userGroups = x$samples[,userGroups]
	}
	MDS <- names(x$usedObj)[grep ( 'MDS', names(x$usedObj))]
	OK = grep ( '_dim_' , MDS, invert= TRUE )
	if ( length(OK) == 0 ) {
		stop( "cellexalvrR does need at least one 3D MDS structure to work on - please create that first!")
	}
	for ( n in MDS[OK] ) {
		for ( n2 in names(x$usedObj[[n]]) ) {
			ret@mds[[n2]] = x$usedObj[[n]][[n2]]
		}
	}
	ret
}