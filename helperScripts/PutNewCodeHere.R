color <- function(x, names) {
	col = rep( gray(0.6), length(names) )
	m = match( rownames(x@dat), names)
	col[which(!is.na(m))] = x@dat$col[m[which(!is.na(m))]]
	col
}