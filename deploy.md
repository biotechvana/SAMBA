# 1 install R

Bash:

  - CentOS
```
sudo yum install epel-release
sudo yum update
sudo yum install R
sudo su - -c "R -e \"install.packages(c('rmarkdown', 'devtools', 'RJDBC'), repos='http://cran.rstudio.com/')\""
```
# 2 install shiny
R:

```
install.packages("shiny")
```

# 3 install shiny server
Bash:

  - CentOS:

```
(check version in: https://posit.co/download/shiny-server/)
wget https://download3.rstudio.org/centos7/x86_64/shiny-server-1.5.19.995-x86_64.rpm
```
# 4 open port 3838, reverse proxy, etc
# 5 samba dependencies
R:

```
list_of_packages = c("shiny","shinydashboard","shinydashboardPlus","shinyFiles","bnlearn","shinyjs","DT","data.table","bnviewer","visNetwork","stringr","shinythemes", "purrr", "seqinr","future","ipc","assertr","promises","dplyr","callr", "parallel","this.path","progressr","compare","future.callr","graph","igraph","zip","tibble","tibble", "fresh", "colourpicker", "shinyBS","shinyalert","shinyjqui" )
lapply(list_of_packages, function(x) if(!require(x,character.only = TRUE)) BiocManager::install(x, dependencies = TRUE))
if (!require("reticulate")) remotes::install_github("rstudio/reticulate")
if (!require("reticulate")) install.packages("reticulate")
if (!require("shinyDirectoryInput")) remotes::install_github("wleepang/shiny-directory-input")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
```
# 6 deploy samba
```
git clone https://gitlab.biotechvana.com/samba/samba.git
```
