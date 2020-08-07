#' The aim of this function is to make as much data available for CellexalVR as possible.
#' 3D models for all 3 analyses and the corresponding data will be added.
#'
#' @name process_Seurat_ATAC_combined
#' @aliases process_Seurat_ATAC_combined,environment-method
#' @rdname process_Seurat_ATAC_combined-methods
#' @docType methods
#' @description convert a BioData list (BioData library not loaded) into a cellexalvrR obejct
#' @param rna the Seurat rna analysis object
#' @param atac the Seurat atac analysis object
#' @param merged the seurat combined analysis object
#' @param meta.cell.groups which meta.data columns to convert to meta.cell classes
#' @param outpath set the outpath of the object (default getwd())
#' @param specie set the specie to either mouse or human (default check gene names)
#' @title import a Seurat atac combined analysis
#' @export 
setGeneric('process_Seurat_ATAC_combined', ## Name
	function (  rna, atac, merged, meta.cell.groups=NULL, outpath=getwd(), specie ) { 
		standardGeneric('process_Seurat_ATAC_combined') 
	}
)

setMethod( 'process_Seurat_ATAC_combined', signature = c ('Seurat', 'Seurat', 'Seurat'),
	definition = function( rna, atac, merged, meta.cell.groups=NULL, outpath=getwd(), specie ){
	## the whole logic will be quite interesting.
	## Outline:
	## UMAP from all 3 objects with an unified cell lable.
	## expression and chrmosomal areas if possible
	## The total analysis likely needs a merge with the CellexalGenomeVR code.

})