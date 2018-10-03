
prefix = ''

script = file.path(prefix, 'data/vrscripts/make_heatmap.R')

homedir = file.path(prefix, 'data/output/default_user/output' ) # <user specific folder>/output

datadir = file.path(prefix, 'data/output/default_user' )# <user specific folder>

latest_version = file.path(prefix, 'data/output/default_user', 'User.group.1.cgr'  )  # the grouping file - I will take the 'User.group.1.cgr' created in the initial_check.R run

output_filepath = file.path(prefix, 'data/output/default_user/heatmap_0-3-5.txt' )# <homedir>/<heatmapName>.txt

top_genes_number = 250

if (  file.exists(output_filepath) ){
	file.remove( output_filepath )
}

system( paste( 'Rscript', script, homedir,  datadir, latest_version, output_filepath, top_genes_number ))


expect_true( file.exists(output_filepath), paste( "file exists", 'heatmap_0-3-5.txt') )

#for ( fname in c( 'heatmap_0-3-5.txt' ) ){	
#	expect_true( file.exists( file.path(outputFolder, fname ) ) , paste( "file exists", fname) )
#}