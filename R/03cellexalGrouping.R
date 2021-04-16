#' the cellexalGrouping class is mainly to bring a usable order into the time mess
#' 
#' 
#' @name cellexalGrouping-class
#' @rdname cellexalGrouping-class
#' @title cellexalGrouping class definition
#' @description  A simple wrapper to hadle the time and time color mappings.
#' @slot gname the group name
#' @slot selectionFile the VR selection file that is the basis for this grouping
#' @slot order the order the cells have been selected in the VR process
#' @slot drc the drc name this object has been selected from
#' @slot col the color vector for these groups
#' @slot error a string vector that contains all error messages
#' @slot timeObj an optional slot to store a cellexalTime object
#' @slot heatmapBasename the filename basis for the heatmap related to this grouping
#' @exportClass cellexalGrouping
setClass("cellexalGrouping", 
	slots=list(
		gname="character",
		selectionFile="character",
		grouping="numeric",
		VRgrouping = "numeric",
		order="integer",
		drc="character",
		col="character",
		error="character",
		timeObj="cellexalTime",
		heatmapBasename='character'
		)
)


setMethod('show', signature = c ('cellexalGrouping'),
	definition = function ( object ) {
		cat (paste("An object of class", class(object)),"with gname", object@gname,"\n" )
		cat (paste( 'with',length(unique(object@grouping)),
			'groups selected from', object@drc,'drc model'),"\n")
		if ( length( object@heatmapBasename) != 0){
			cat( paste( "The heatmap basname is",object@heatmapBasename,"\n" ))
		}
		cat('\n')
	}
)



setMethod('HTMLtable', signature = c ('cellexalGrouping'),
	definition = function ( x ) {

		tableLine = function(id ) {
			OK = which( x@grouping == id)
			paste(sep="",
				'<tr><td style="background-color:',
				x@col[id],'"></td><td>',
				x@col[id],"</td><td>",
				length(OK),"</td><td>",
				x@VRgrouping[OK[1]], "</td><td>",
				id,"</td></tr>"
			)
		}
		if ( length( x@VRgrouping) == 0){
			x@VRgrouping = x@grouping -1
		}
		paste( sep="", collapse="",
			"\n### group information table\n\n",
			'<table>\n<tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th><th>VR ID</th><th>R ID</th></tr>',
			paste(sep="\n",collapse="\n",sapply( 1:length(x@col), tableLine) ),"</table>"
		)
} )

