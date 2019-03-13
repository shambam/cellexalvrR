
context('VR create heatmap ')

if ( is.na( match('cellexalvrR',rownames(installed.packages()))) ) {
	skip("cellexalvrR has to be installed before this test")
}else if ( installed.packages()['cellexalvrR','Version'] != packageDescription("cellexalvrR")$Version) {
	print ( "Please re-run this test with the updated cellexalvrR lib installed if any error occures" )
}

prefix <- './'

script = file.path(prefix, 'data','vrscripts','make_heatmap.R')

homedir = file.path(prefix, 'data','output','default_user','output' ) # <user specific folder>/output

datadir = file.path(prefix, 'data','output','default_user' )# <user specific folder>

latest_version = 'User.group.1'   # the grouping file - I will take the 'User.group.1.cgr' created in the initial_check.R run


output_filepath = file.path(prefix, 'data','output','default_user','heatmap_0-3-5.txt' )# <homedir>/<heatmapName>.txt

top_genes_number = 250

if (  file.exists(output_filepath) ){
	file.remove( output_filepath )
}

expect_true( file.exists( file.path(datadir, 'cellexalObj.RData' ) ), paste("input object exists", file.path(datadir, 'cellexalObj.RData' ) ) )

#print( paste("Script will test file content:" , file.path(datadir, "cellexalObj.RData") ))

CO <- loadObject( file.path(datadir, 'cellexalObj.RData' ) )

expect_true( all.equal( names(CO@mds), c('graph1', 'graph2')), paste("before: input object mds names == ('graph1', 'graph2') [", 
				paste(collapse=", ", names(CO@mds)), "]" ))

expect_true( CO@outpath == normalizePath(datadir), "outpath is absolute" )

expect_true(! file.exists(output_filepath), paste( "file missing not created yet", 'heatmap_0-3-5.txt') )

print ( paste( 'Rscript', script, homedir,  datadir, latest_version, output_filepath, top_genes_number, 'wilcox' ) )
system( paste( 'Rscript', script, homedir,  datadir, latest_version, output_filepath, top_genes_number, 'wilcox' ))

expect_true( file.exists(output_filepath), paste( "file missing", 'heatmap_0-3-5.txt') )

t = NULL
if ( file.exists(output_filepath) ) {
	t = scan( output_filepath , what=character())
	t = t[-1] ## kill the header.
}
expect_true(length(unique(t)) == 250, paste("not the expected number of genes returned: returned",length(unique(t)), "!= 250 expected" ) )

CO2 <- loadObject( file.path(datadir, 'cellexalObj.RData' ) )

expect_true( all.equal( names(CO2@mds), c('graph1', 'graph2')), paste("after: input object mds names == ('graph1', 'graph2') [", paste(collapse=", ", names(CO2@mds)), "]" ))



#for ( fname in c( 'heatmap_0-3-5.txt' ) ){	
#	expect_true( file.exists( file.path(outputFolder, fname ) ) , paste( "file has not been created", fname) )
#}