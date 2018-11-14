#' @name getDifferentials
#' @aliases getDifferentials,cellexalvrR-method
#' @rdname getDifferentials-methods
#' @docType methods
#' @description  Creates a heatmap from a selection of groups
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param deg.method The method to use to find DEGs ( 'anova', 'edgeR', 'MAST' or 'Seurat')
#' @param num.sig number of differnetial genes to return (250)
#' @param Log log the results (default=TRUE)
#' @keywords DEGs
#' @title description of function getDifferentials
#' @export getDifferentials
if ( ! isGeneric('getDifferentials') ){setGeneric('getDifferentials', ## Name
	function (cellexalObj,cellidfile,deg.method=c("anova","edgeR", "MAST", 'Seurat'),num.sig=250, Log=TRUE) { 
		standardGeneric('getDifferentials') 
	}
) }

setMethod('getDifferentials', signature = c ('character'),
		definition = function (cellexalObj,cellidfile,deg.method=c("anova","edgeR", "MAST", 'Seurat'),num.sig=250, Log=TRUE) {
			cellexalObj <- loadObject(cellexalObj)
			getDifferentials( cellexalObj,cellidfile,deg.method,num.sig, Log=Log)
		}
)

setMethod('getDifferentials', signature = c ('cellexalvrR'),
	definition = function (cellexalObj,cellidfile,deg.method=c("anova","edgeR", "MAST", 'Seurat'),num.sig=250, Log=TRUE) {

    cellexalObj <- loadObject(cellexalObj)

	cellexalObj <- userGrouping(cellexalObj, cellidfile)
	not <- which(is.na(cellexalObj@userGroups[,cellexalObj@usedObj$lastGroup]))
	if ( length(not) > 0) {
		loc <- reduceTo (cellexalObj, what='col', to=colnames(cellexalObj@data)[- not ] )
	}else {
		loc <- cellexalObj
	}
    if ( ! is.na(match(paste(cellexalObj@usedObj$lastGroup, 'order'), colnames(cellexalObj@data))) ){
		loc <- reorder.samples ( loc, paste(cellexalObj@usedObj$lastGroup, 'order'))
	}
	
	info <- groupingInfo( loc )

	dat <- loc@data

    rem.ind <- which(apply(dat,1,sum)==0)
	dat.f <- dat

	grp.vec <- info$grouping

    col.tab <- info$col

    if(length(rem.ind)>0){
		dat.f <- dat.f[-rem.ind,]
	}
	
    deg.genes <- NULL
    if ( is.null(cellexalObj@usedObj$sigGeneLists)) 
		cellexalObj@usedObj$sigGeneLists = list()
	
	if(length(col.tab) == 1){
		message('cor.stat linear gene stats')
		lin <- function( v, order ) {
			cor.test( v, order, method="spearman" )
		}
		ps <- apply(dat.f,1,lin,order=1:ncol(dat.f))
		
		ps = data.frame((lapply(ps, function(x){ c(x$statistic, x$p.value) })))
		ps = data.frame(t(ps))
		colnames(ps) = c('statsistics', 'p.value' )
		sigp <- order(ps$p.value)[1:num.sig]
		deg.genes <- rownames(ps)[sigp]		
		
		ps[,'p.adj.fdr'] = stats::p.adjust(ps[,'p.value'], method = 'fdr')
		cellexalObj@usedObj$sigGeneLists$lin[[cellexalObj@usedObj$lastGroup]] = ps
		if ( Log ) {
			logStatResult( cellexalObj, 'linear', ps, 'p.adj.fdr' )
		}
		
	}else if(deg.method=="anova"){
		message('anova gene stats')
        anovap <- function(v,labs){
		    anova(lm(v~-1+labs,test="LRT"))$Pr[1]
	    }
		

        if ( length(col.tab) > 1 ){
		    ps <- apply(dat.f,1,anovap,labs=grp.vec)
	    }else if (length(col.tab) == 1 ){
		    ps <- apply(dat.f,1,lin,order=1:ncol(dat.f))
	    }
	
	    sigp <- order(ps)[1:num.sig]
		
	    deg.genes <- rownames(dat.f[sigp,])
		
		## save the original p values for the heatmap report GO function
		if ( is.null(cellexalObj@usedObj$sigGeneLists$anova)) 
			cellexalObj@usedObj$sigGeneLists$anova = list()
	
		d = data.frame('p.value' = ps)
		#rownames(d) = names(ps)
		
		d[,'p.adj.fdr'] = stats::p.adjust(d[,'p.value'], method = 'fdr')
		cellexalObj@usedObj$sigGeneLists$anova[[cellexalObj@usedObj$lastGroup]] = d

		if ( Log ) {
			logStatResult( cellexalObj, 'anova', d, 'p.adj.fdr' )
		}
    }

	if(deg.method=="edgeR"){
		message('edgeR::estimateDisp gene stats')
		dge <- edgeR::DGEList(
    			counts = dat.f, 
    			norm.factors = rep(1, length(dat.f[1,])), 
    			group = grp.vec
			)
		group_edgeR <- factor(grp.vec)
		design <- model.matrix(~ group_edgeR)
		dge <- edgeR::estimateDisp(dge, design = design, trend.method = "none")
		fit <- edgeR::glmFit(dge, design)
		res <- edgeR::glmLRT(fit)
		pVals <- res$table[,4]
		names(pVals) <- rownames(res$table)
		
		ps = data.frame( 'p.value' = pVals, 'p.adj.fdr' =  p.adjust(pVals, method = "fdr") )

		deg.genes <- rownames(ps)[order(ps$p.value)[1:num.sig]]
		
		## save the original p values for the heatmap report GO function
		if ( is.null(cellexalObj@usedObj$sigGeneLists$edgeR)) 
			cellexalObj@usedObj$sigGeneLists$edgeR = list()
		
		cellexalObj@usedObj$sigGeneLists$edgeR[[cellexalObj@usedObj$lastGroup]] = ps
		if ( Log ) {
			logStatResult( cellexalObj, 'edgeR', ps, 'p.adj.fdr' )
		}
		
	}
	
	if(deg.method=='MAST') {
		message('MAST::lrTest gene stats')
		## in parts copied from my BioData::createStats() function for R6::BioData::SingleCells
		if (!requireNamespace("MAST", quietly = TRUE)) {
			stop("MAST needed for this function to work. Please install it.",
					call. = FALSE)
		}
		sca <- MAST::FromMatrix(class='SingleCellAssay', 
				exprsArray= t(dat.f), 
				cData=data.frame(wellKey=colnames(dat.f), GroupName = grp.vec), 
				fData=data.frame(primerid=rownames(dat.f))
		)
		form = '~ GroupName'
		zlm.output <- MAST::zlm( as.formula(form), sca, method='glm', ebayes=T)
		zlm.lr <- MAST::lrTest(zlm.output, form)
		Rtab = zlm.lr[,,'Pr(>Chisq)']
		o <- order(Rtab[,'hurdle'])
		deg.genes <- rownames(Rtab)[o[1:num.sig]]
		deg.genes <- str_replace_all( deg.genes, '_\\d+$', '')
		
		## save the original p values for the heatmap report GO function
		if ( is.null(cellexalObj@usedObj$sigGeneLists$MAST)) 
			cellexalObj@usedObj$sigGeneLists$MAST = list()

		cellexalObj@usedObj$sigGeneLists$MAST[[cellexalObj@usedObj$lastGroup]] = Rtab
		if ( Log ) {
			Rtab = cbind( Rtab, 'p.adj.fdr' = stats::p.adjust(Rtab[,'hurdle'], method = 'fdr'))
			logStatResult( cellexalObj, 'MAST', Rtab, 'p.adj.fdr' )
		}
		
	}
	if(deg.method=='Seurat') {
		message('Seurat::FindAllMarkers gene stats')
		## in parts copied from my BioData::createStats() function for R6::BioData::SingleCells
		if (!requireNamespace("Seurat", quietly = TRUE)) {
			stop("seurat needed for this function to work. Please install it.",
					call. = FALSE)
		}
		sca <- Seurat::CreateSeuratObject(dat.f, project = "SeuratProject", min.cells = 0,
				min.genes = 0, is.expr = 0, normalization.method = NULL,
				scale.factor = 10000, do.scale = FALSE, do.center = FALSE,
				names.field = 1, names.delim = "_", meta.data = data.frame(wellKey=colnames(dat.f), GroupName = grp.vec),
				display.progress = TRUE)
		
		sca = Seurat::SetIdent( sca, colnames(loc@data), as.character(loc@userGroups[ ,cellexalObj@usedObj$lastGroup]) )
		
		all_markers <- Seurat::FindAllMarkers(object = sca)
		message("The Seurat select signififcant genes might need some work!")
		deg.genes = vector('character', num.sig)
		degid = 0
		for ( i in order(all_markers[,'p_val_adj']) ){
			if ( is.na( match ( all_markers[i,'gene'], deg.genes) ) ){
				degid = degid + 1
				deg.genes[degid] = all_markers[i,'gene']
				if ( degid == num.sig){
					break
				}
			}
		}
		if ( is.null(cellexalObj@usedObj$sigGeneLists$Seurat)) 
			cellexalObj@usedObj$sigGeneLists$Seurat = list()
		cellexalObj@usedObj$sigGeneLists$Seurat[[cellexalObj@usedObj$lastGroup]] = all_markers
		if ( Log ) {
			logStatResult( cellexalObj, 'Seurat', all_markers, 'p_val_adj' )
		}
	}
	if ( length(deg.genes) == 0){
		message('deg.genes no entries - fix that')
		browser()		
	}
    lockedSave(cellexalObj)
	deg.genes = rownames(cellexalObj@data)[ match( make.names(deg.genes), make.names( rownames( cellexalObj@data) ) )]
	loc = reduceTo(loc, what='row', to=deg.genes)
	tab <- as.matrix(t(loc@data))
	hc <- hclust(as.dist( 1- cor(tab, method='pearson') ),method = 'ward.D2')
    rownames(loc@data)[hc$order]

} )

#' @name setStatsMethod
#' @aliases setStatsMethod,cellexalvrR-method
#' @rdname setStatsMethod-methods
#' @docType methods
#' @description sets the used statistics method from this list ( "anova","edgeR", "MAST", 'Seurat' )
#' @param x the cellexalvrR object
#' @param method the default stats method to use (default 'anova')
#' @title set the default stats method
#' @export 
setGeneric('setStatsMethod', ## Name
		function ( x, method='anova') { ## Argumente der generischen Funktion
			standardGeneric('setStatsMethod') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('setStatsMethod', signature = c ('cellexalvrR'),
		definition = function ( x, method='anova') {
			if ( ! is.na(match ( method, c("anova","edgeR", "MAST", 'Seurat')))) {
				x@usedObj$statsMethod = method
			}else {
				stop(paste("Stats method", method,"is not implemented"))
			}
			x
		} )

#' @name getStatsMethod
#' @aliases getStatsMethod,cellexalvrR-method
#' @rdname getStatsMethod-methods
#' @docType methods
#' @description get the default stat method rpreviousely set with setStatsMethod()
#' @param x the cellexalvrR object
#' @title get the method to calculate differential gene expression
#' @export 
setGeneric('getStatsMethod', ## Name
		function ( x, method='Seurat') { ## Argumente der generischen Funktion
			standardGeneric('getStatsMethod') ## der Aufruf von standardGeneric sorgt für das Dispatching
		}
)

setMethod('getStatsMethod', signature = c ('cellexalvrR'),
		definition = function ( x, method='Seurat') {
			if ( is.null( x@usedObj$statsMethod) ) {
				return ( 'Seurat' )
			}
			return( x@usedObj$statsMethod )
		} )
