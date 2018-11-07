#' @name cellexalvrR
#' @title cellexalvrR class definition
#' @description  The R backend for the CellexalVR 3D application
#' @slot data the expression matrix
#' @slot meta.cell the cell level meta information (matrix)
#' @slot meta.gene the gene level meta information (matrix)
#' @slot userGroups internally used to store the user defined groupings from the 3D process (data.frame)
#' @slot colors a list for each userGroups entry defining the color sheme for this grouping
#' @slot mds a list of all mds objects to be visible in the 3D application
#' @slot index a matrix for FACS or other linear numeric data that should be available for colouring in the 3D application
#' @slot tfs depricated not used any more
#' @slot specie the species this data is from (mouse or human)
#' @slot outpath the path this object will be saved to
#' @exportClass cellexalvrR
setClass(Class="cellexalvrR",
		representation=representation(
				data="matrix",
				meta.cell="matrix", #test commit
				meta.gene="matrix",
				userGroups="data.frame", # whenever a user defined grouping is read from file we add one column
				colors="list", # the color information linked to all user groups
				groupSelectedFrom = 'list', # which mds rep has been used to create the grouping
				usedObj="list", # push all other objects you temporarily need here
				mds="list",
				index=c("matrix"),
				tfs="vector",
				specie="character",
				outpath="character"
		),
		prototype(
				data=matrix(),
				meta.cell=matrix(),
				meta.gene=matrix(),
				userGroups= data.frame(),
				colors=list(),
				groupSelectedFrom=list(),
				usedObj=list(),
				mds=list(),
				index=matrix(),
				tfs=NA_character_,
				specie=NA_character_,
				outpath=NA_character_
		)
)



#' @name cellexalvr
#' @title cellexalvr class definition (old)
#' @exportClass cellexalvr
setClass( 
		Class='cellexalvr', 
		representation = representation ( 
		##	NGS = 'binary'
		),
		contains='cellexalvrR'
)
