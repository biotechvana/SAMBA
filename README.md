# SAMBA
Structure-learning of Aquaculture Microbiomes using a Bayesian-network Approach

## Installing
To run SAMBA first install all R depedencies using ```dep_check.R``` script and ```set_picrust.sh``` by running
```bash
## install all R depedencies
Rscript dep_check.R
## install picrust and setup picrust2 env using conda
./set_picrust.sh
```
### Configuration
Basic SAMBA configuration should be in ```configs.R```, to start edit this file rename the example configuration included with SAMBA source
```
mv configs_example.R configs.R
``` 

Then start editing ```configs.R``` file which include the following :

```R
# dir to store result networks and output files  from running SAMBA 
deploy_dir <- "path/to//samba_files/"
# dir to store static data files used by SAMBA
deploy_data <- "path/to//samba_data/"
# path to python folder which contains picrust scripts
deploy_python_scripts <- paste(getwd(), "/python/", sep = "")
# conda bin to handle picrust env
deploy_condabin <- "path/to/bin/conda"
# picrust env name should be the same as the one located in set_picrust.sh script
deploy_condaenv_picrust2 <- "picrust2"
``` 

## Starting SAMBA
```bash
Rscript start_app.R
```
