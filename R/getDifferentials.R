#' @name getDifferentials
#' @aliases getDifferentials,cellexalvrR-method
#' @rdname getDifferentials-methods
#' @docType methods
#' @description  Creates a heatmap from a selection of groups
#' @param cellexalObj, cellexalvr object
#' @param cellidfile file containing cell IDs
#' @param deg.method The method to use to find DEGs ( 'anova', 'edgeR', 'MAST' or 'Seurat')
#' @param num.sig number of differnetial genes to return (250)
#' @keywords DEGs
#' @title description of function getDifferentials
#' @export getDifferentials
if ( ! isGeneric('getDifferentials') ){setGeneric('getDifferentials', ## Name
	function (cellexalObj,cellidfile,deg.method=c("anova","edgeR", "MAST", 'Seurat'),num.sig=250) { 
		standardGeneric('getDifferentials') 
	}
) }

setMethod('getDifferentials', signature = c ('character'),
		definition = function (cellexalObj,cellidfile,deg.method=c("anova","edgeR", "MAST", 'Seurat'),num.sig=250) {
			cellexalObj <- loadObject(cellexalObj)
			getDifferentials( cellexalObj,cellidfile,deg.method,num.sig)
		}
)

setMethod('getDifferentials', signature = c ('cellexalvrR'),
	definition = function (cellexalObj,cellidfile,deg.method=c("anova","edgeR", "MAST", 'Seurat'),num.sig=250) {

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
    if(deg.method=="anova"){

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
		names(ps) = rownames(loc@data)
		cellexalObj@usedObj$sigGeneLists$anova[[cellexalObj@usedObj$lastGroup]] = ps
    }

	if(deg.method=="edgeR"){

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
		
		pVals <- p.adjust(pVals, method = "fdr")
		deg.genes <- names(sort(pVals)[1:num.sig])
		
		## save the original p values for the heatmap report GO function
		if ( is.null(cellexalObj@usedObj$sigGeneLists$edgeR)) 
			cellexalObj@usedObj$sigGeneLists$edgeR = list()
		cellexalObj@usedObj$sigGeneLists$edgeR[[cellexalObj@usedObj$lastGroup]] = pVals
	}
	
	if(deg.method=='MAST') {
		## in parts copied from my BioData::createStats() function for R6::BioData::SingleCells
		if (!requireNamespace("MAST", quietly = TRUE)) {
			stop("MAST needed for this function to work. Please install it.",
					call. = FALSE)
		}
		sca <- MAST::FromMatrix(class='SingleCellAssay', 
				exprsArray= dat.f, 
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
		if ( is.null(cellexalObj@usedObj$sigGeneLists$edgeR)) 
			cellexalObj@usedObj$sigGeneLists$edgeR = list()
		pr = Rtab[,'hurdle']
		names(pr) = rownames(loc@data)
		cellexalObj@usedObj$sigGeneLists$edgeR[[cellexalObj@usedObj$lastGroup]] = pr
	}
	if(deg.method=='seurat') {
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
		deg.genes = all_markers[order(all_markers[,'p_val_adj'])[1:(num.sig)],'gene']
		
	}
    lockedSave(cellexalObj)
    deg.genes
} )
