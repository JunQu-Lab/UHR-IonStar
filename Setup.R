#### Setup ####



#### Update log is included in the manual ####

#### R package installation ####
#### Please install all the dependent R packages using following codes when you try to run this app at first time. ####



#### Also in the step of installation, every time the console shows:"Update all/some/none? [a/s/n]:", please input "a". ####
#### Do you want to install from sources the packages which need compilation? (Yes/no/cancel) no ####
#### After successfully installing all packages, go to 'ui.R' or 'server.R' and click 'Run App' to start. ####


install.packages(c('extrafont','shiny','shinydashboard','DT','knitr',"latticeExtra",
                   "devtools","plyr","ggplot2","ggcorrplot","ggrepel","RSQLite"))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")

BiocManager::install(c("limma","TCGAbiolinks","clusterProfiler", "Biobase", "DO.db", "sesameData",
                       "AnnotationHub","RDAVIDWebService"))

#### Set your working directory to UHR-IonStar folder then intall the package 'IonStarStat' using codes below:
BiocManager::install(c("MCMCglmm","affyPLM","mvoutlier"))
install.packages("IonStarStat_0.1.4.tar.gz", repos = NULL)


library('devtools')
devtools::install_github("vqv/ggbiplot")
