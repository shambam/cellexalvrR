#' @name .onAttach
#' @aliases .onAttach,cellexalvr-method
#' @rdname .onAttach-methods
#' @docType methods
#' @description 
#' @param libname  TEXT MISSING
#' @param pkgname  TEXT MISSING
#' @title description of function .onAttach
#' @export 
setGeneric('.onAttach', ## Name
	function (libname, pkgname) { ## Argumente der generischen Funktion
		standardGeneric('.onAttach') ## der Aufruf von standardGeneric sorgt für das Dispatching
	}
)

setMethod('.onAttach', signature = c ('cellexalvr'),
	definition = function (libname, pkgname) {
			#packageStartupMessage("Welcome to my package BioData")
			where <- as.environment("package:BioData")
			clss <- list(
					c("cellexalvr", "R6"),
				#	c("tRNAMINT", "BioData"),
				#	c("MicroArray", "BioData")
			)
			## Ensure clean initial state for subsequent package loads
			## while developing //
			sapply(clss, function(cls) {
						idx <- sapply(cls, isClass )
						suppressWarnings(try(sapply(cls[idx], removeClass,
												where = where), silent = TRUE))
					})
			## Set formal class equivalent //
			sapply(clss, function(cls) {
						try(setOldClass(cls, where = where), silent = TRUE)
					})
#	r6x::formalizeClasses()
		} )
