

setMethod('show', signature = c ('cellexalGrouping'),
	definition = function ( object ) {
		cat (paste("An object of class", class(object)),"with gname", object@gname,"\n" )
		cat (paste( 'with',length(table(object@grouping)),
			'groups selected from', object@drc,'drc model'),"\n")
		if ( length( object@heatmapBasename) != 0){
			cat( paste( "The heatmap basname is",object@heatmapBasename,"\n" ))
		}
		cat('\n')
	}
)


#' Create an HTML table from a cellexalGrouping.
#' These tables are part of the log html's and show group color as well as cell count, VR- and R-ID.
#' @name HTMLtable
#' @aliases HTMLtable,cellexalGrouping-method
#' @rdname HTMLtable-methods
#' @docType methods
#' @description  convert a H5 annotation (any name) table to a data table
#' @param x the cellexalGrouping
#' @title create a html table from a cellexalGrouping object
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

