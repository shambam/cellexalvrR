#' Log related function - convert a session into a html table that can be added to the log
#'
#' @name group2HTMLtable
#' @docType methods
#' @description create an HTML table from a user grouping
#' @param x, cellexalvrR object
#' @param info the grouping info to convert
#' @param names optional vector of names for the groupings (grouping info is numeric)
#' @title create a log table from a grouping
#' @export 
#if ( ! isGeneric('group2HTMLtable') ){
setGeneric('group2HTMLtable', ## Name
			function ( x, info, names=NULL ) { 
				standardGeneric('group2HTMLtable')
			}
	) 
#}


#' @rdname group2HTMLtable
setMethod('group2HTMLtable', signature = c ('cellexalvrR', 'list'),
		definition = function (x, info, names=NULL ) {

		tableHTML = paste( sep=" ",
				"",
				"### Group Information Table", info$gname,"\n\n",
			'<table>',
			'  <tr><th>Color</th><th>HTML tag</th><th>cell count [n]</th><th>alt name</th></tr>'
		)
		for ( id  in 1:length(info$col) ) {
			tr = paste( sep="",
				'<tr><td style="background-color:', info$col[id],'"',
				"></td><td>",info$col[id],"</td><td>",
				length(which( info$grouping == id) ),"</td><td>")
			if ( ! is.null(names) ) {
				tr = paste( sep="", tr, names[id], '</td></tr>')
			}
			else {
				tr = paste( sep="", tr, '</td></tr>')
			}
			
			tableHTML = paste(sep="\n", tableHTML, tr )
		}
		paste( tableHTML,'</table>' )
	}
)

