library(cellexalvrR)
path = '/mnt/VR_Project'
new = lapply (list.dirs(path, recursive=F), function(d) {
		      message ( d)
      if ( file.exists( file.path( d, 'cellexalObj.RData') )) {
	cellexalObj = renew(lockedLoad( file.path( d, 'cellexalObj.RData')) )
        system( paste('mv',  file.path( d, 'cellexalObj.RData'),  file.path( d, 'cellexalObj_oldVersion.RData') ))
        save(cellexalObj,  file= file.path( d, 'cellexalObj.RData') )	
      	NULL
      }else {
	NULL
      }
} )

