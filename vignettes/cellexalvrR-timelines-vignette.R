## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(cellexalvrR)

cellexalObj = reset( cellexalObj )
cellexalObj


## ----message=FALSE------------------------------------------------------------
head( read.delim( system.file("extdata", "SelectionHSPC_time.txt", package="cellexalvrR"), header=F ) )


## ----message=FALSE------------------------------------------------------------

cellexalObj@outpath = getwd()
cellexalObj = sessionPath( cellexalObj, 'timelineVignett')



## ----message=FALSE------------------------------------------------------------
cellexalObj = userGrouping( cellexalObj, system.file("extdata", "SelectionHSPC_time.txt",package= "cellexalvrR") )
cellexalObj = pseudotimeTest3D(cellexalObj, grouping= cellexalObj@usedObj$lastGroup )
cellexalObj = createStats( cellexalObj@usedObj$timelines[[1]], cellexalObj,  num.sig= 250 )

bossTime = cellexalObj@usedObj$timelines[["lastEntry"]] ## latest

cellexalObj = createReport(bossTime, cellexalObj, info = bossTime)$cellexalObj

bossTime = cellexalObj@usedObj$timelines[["lastEntry"]] ## latest

# the statistics table
print ( head( cellexalObj@usedObj$sigGeneLists$lin[[ cellexalObj@usedObj$lastGroup ]] ))

length( cellexalObj@usedObj$deg.genes)

## ---- message=FALSE-----------------------------------------------------------
cellexalObj@usedObj$timelines

