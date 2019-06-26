#' cellexalvrR is the R class that build the backend of CellexalVR
#' 
#' https://www.cellexalvr.med.lu.se/ is a VR application to analyze single cell expression data.
#' This R class is the backend of the VR application and a dependency of that.
#' CellexalvrR is not developed to be a general purpose single cell analysis class like Seurat.
#' It e.g. does not implement normalization or drc methods, but expects all data to be preprocessed.
#' 
#' @name cellexalvrR-class
#' @rdname FastWilcoxTest-class
#' @title cellexalvrR class definition
#' @description  The R backend for the CellexalVR 3D application
#' @slot data the expression matrix (unused!)
#' @slot data the spearse matrix
#' @slot meta.cell the cell level meta information (matrix)
#' @slot meta.gene the gene level meta information (matrix)
#' @slot userGroups internally used to store the user defined groupings from the 3D process (data.frame)
#' @slot colors a list for each userGroups entry defining the color sheme for this grouping
#' @slot groupSelectedFrom local info to store which usergroup has been selected from which drc structure
#' @slot usedObj a generic storage list collecting objects not fitting into any other slot
#' @slot drc a list of all DRC objects to be visible in the 3D application
#' @slot index a matrix for FACS or other linear numeric data that should be available for colouring in the 3D application
#' @slot tfs depricated not used any more
#' @slot specie the species this data is from (mouse or human)
#' @slot outpath the path this object will be saved to
#' @slot version the cellexalvrR package version that created this object.
#' @exportClass cellexalvrR
setClass(Class="cellexalvrR",
		representation=representation(
				data="dgCMatrix",
				meta.cell="matrix", #test commit
				meta.gene="matrix",
				userGroups="data.frame", # whenever a user defined grouping is read from file we add one column
				colors="list", # the color information linked to all user groups
				groupSelectedFrom = 'list', # which drc rep has been used to create the grouping
				usedObj="list", # push all other objects you temporarily need here
				drc="list",
				index=c("matrix"),
				tfs="vector",
				specie="character",
				outpath="character",
				version="character"
		),
		prototype(
				data=new('dgCMatrix'),
				meta.cell=matrix(),
				meta.gene=matrix(),
				userGroups= data.frame(),
				colors=list(),
				groupSelectedFrom=list(),
				usedObj=list(),
				drc=list(),
				index=matrix(),
				tfs=NA_character_,
				specie=NA_character_,
				outpath=NA_character_,
				version=as.character(packageVersion("cellexalvrR"))
		)
)




#' @name cellexalvr-class
#' @title cellexalvr class definition (old)
#' @description This class definition is totally empty and only incuded here to allow for a smooth updatae of the old objects.
#' @exportClass cellexalvr
setClass( 
		Class='cellexalvr', 
		representation = representation ( 
		##	NGS = 'binary'
		),
		contains='cellexalvrR'
)


