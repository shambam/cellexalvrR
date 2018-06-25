#'Creates a meta cell matrix from a supplied dataframe from required fields
#'@param metad A dataframe of per cell metadata
#'@param rq.fields A vector of name specifiying which columns should me made into metadata
#'@keywords metadata cell
#'@export make.cell.meta.from.df

make.cell.meta.from.df <- function(metad,rq.fields){

    meta4cellexalvr <- NULL

    for(i in 1:length(rq.fields)){
        tmp.met <- to.matrix(metad[,rq.fields[i]],unique(metad[,rq.fields[i]]) )
        colnames(tmp.met) <- paste(rq.fields[i],colnames(tmp.met),sep="@")
        meta4cellexalvr <- cbind(meta4cellexalvr,tmp.met)
    }

    colnames(meta4cellexalvr) <- gsub("\n","",colnames(meta4cellexalvr))
    colnames(meta4cellexalvr) <- gsub(" ","",colnames(meta4cellexalvr))
    meta4cellexalvr
}
