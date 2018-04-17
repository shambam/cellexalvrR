#'Plots the MDS reduced data for a quick look
#'@param cellexalObj A cellexalvr object
#'@export plotAllMDS

plotAllMDS <- function(cellexalObj){

    for(i in 1:length(cellexalObj@mds)){
        rgl.open()
        rgl.points(cellexalObj@mds[[i]])
        
    }

}