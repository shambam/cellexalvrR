#'Class defintion of cellexalvr
#' @exportClass cellexalvr
setClass("cellexalvr",slots=c(
    data="matrix",
    meta.cell="matrix",
    meta.gene="matrix",
    mds="list",
    index="matrix"
    )   
)


