
prefix = ''

script = file.path(prefix, 'data/vrscripts/make_networks.R')


input_file <- file.path(prefix, 'data/output/default_user', 'User.group.1.cgr'  ) # grouping file path

datadir <-  file.path(prefix, 'data/output/default_user' ) # the user specific folder

output_file  <- file.path(prefix, 'data/output/default_user', "Resources","Networks" ) # the output path


system( paste( 'Rscript', script, input_file, datadir, output_file ))


