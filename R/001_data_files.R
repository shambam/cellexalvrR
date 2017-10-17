#' @name CellCycle
#' @title A simple table containing the Human and mouse orthologe CellCycle genes from PMID17994010
#' "Genome-scale RNAi profiling of cell division in human tissue culture cells."
#' @description The data can be used by stating
#' useInbuiltGOIlists (cellexalObj, 'CellCycle' )
#' And it is used to visualize the cell cycle genes in the VR environment.
#' Only the genes also identifyable in mouse were used here.
#' @docType data
#' @usage CellCycle
#' @format data.frame
'CellCycle'

#' A list of mouse transcription factors.
#'
#' A list of mouse transcription factors. Used when generating TF networks
#'
#' @format A vector 1357 in length:
#' \describe{
#'   \item{tf}{transcription factor}
#'   ...
#' }
#' @source \url{http://bioinfo.life.hust.edu.cn/AnimalTFDB/}
"mouse.tfs"

#' A list of human transcription factors.
#'
#' A list of human transcription factors. Used when generating TF networks
#'
#' @format A vector 1468 in length:
#' \describe{
#'   \item{tf}{transcription factor}
#'   ...
#' }
#' @source \url{http://bioinfo.life.hust.edu.cn/AnimalTFDB/}
"human.tfs"

#' @name Epigenetic
#' @title A simple table containing the data from http://epifactors.autosome.ru/ as from 21st September 2017
#' @description This table can be used to create the epigenetics MDS objects.
#' @docType data
#' @usage Epigenetic
#' @format data.frame
'Epigenetic'