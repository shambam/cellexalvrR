library(cellexalvrR)

args <- commandArgs(trailingOnly = TRUE)

datadir <- args[1] ## please give me the user spcific analysis path here!!!!

genes <- args[2] ## the heatmap_<x>.txt file

ontology <- args[3]

if ( is.na( ontology) ) {
	ontology = 'BP'
}

topNodes  <- args[4] 

if ( is.na( topNodes) ) {
	topNodes = 20
}

cellexalObj <- loadObject(file.path(datadir, "cellexalObj.RData"))

ontologyLogPage(cellexalObj, genes, ontology = ontology, topNodes = topNodes )
