## set path to store result and data from samba
deploy_dir <- "/srv/shiny-server/samba_files/"
deploy_data <- "/srv/shiny-server/samba_data/"
deploy_python_scripts <- paste(getwd(), "/python/", sep = "")
deploy_picrust_kegg_files <- paste(getwd(), "/picrust2_kegg/", sep = "")
deploy_condabin <- "path/to/bin/conda"
deploy_condaenv_picrust2 <- "picrust2"

### ENABLE debug
# DEBUG <- TRUE
