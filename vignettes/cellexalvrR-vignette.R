## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=F--------------------------------------------------------------
#  library(devtools)
#  install_github("sonejilab/cellexalvrR")

## ----message=FALSE-------------------------------------------------------
library(cellexalvrR)

if ( ! file.exists("log2data.RData") ) {
   log2data = as.matrix(cellexalObj@data)
   save( log2data, file="log2data.RData")
   rm( log2data)
   
   facs = cellexalObj@index
   save( facs, file= "facs.RData")
   rm( facs )
   
   cell.ids = cellexalObj@meta.cell
   save( cell.ids, file="cell.ids.RData")
   rm(cell.ids)
   
   diff.proj = cellexalObj@drc$diffusion
   save( diff.proj, file="diff.proj.RData")
   rm( diff.proj )
   
   ddr.proj = cellexalObj@drc$DDRtree
   save( ddr.proj, file="ddr.proj.RData" )
   rm(ddr.proj)
   
   tsne.proj = cellexalObj@drc$tSNE
   save( tsne.proj, file="tsne.proj.RData")
   rm(tsne.proj)
   
}

## ---- fig.show='hold',eval=TRUE------------------------------------------
	
load("log2data.RData")
load("facs.RData")
load("cell.ids.RData")
load("diff.proj.RData")
load("ddr.proj.RData")
load("tsne.proj.RData")

## ---- fig.show='hold',eval=T---------------------------------------------
log2data[1:10,1:10]

## ---- fig.show='hold',eval=T---------------------------------------------
head(facs)

## ---- fig.show='hold',eval=T---------------------------------------------
head(cell.ids)

## ---- fig.show='hold',eval=T---------------------------------------------
head(diff.proj)

## ---- fig.show='hold',eval=T---------------------------------------------
proj.list <- list(diffusion=diff.proj,DDRtree=ddr.proj,tSNE=tsne.proj)

