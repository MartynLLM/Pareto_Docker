######################### GLOBAL ###################################
# comments: we assume four variables delineating pareto front
# Project: Clustering Pareto solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
## loading new packages
foo1 <- function(x) {
  for (i in x) {
    if (!requireNamespace(i, quietly = TRUE)) {
      install.packages(i, dependencies = TRUE, quiet = TRUE)
    }
    library(i, character.only = TRUE)
  }
}

## check if any packages are missing (not only here but also for external convert_optain)
foo1(c( "cluster", #added for kmeans.r
        "config", #added for correlation_matrix code
        "configr", "corrplot", 
        "dplyr",  #added for kmeans.r
        "DT", "fs", "fst", 
        "geosphere","geohashTools", "ggplot2","ggtext", "gridExtra",
        "ini", "leaflet", "leafsync",
        "mapview",  "plotly",  "processx", 
       "quanteda",   "RColorBrewer",
       "reshape2", #added for kmeans.r
       "reticulate",
       "scales", "sf", "shiny", "shinycssloaders", "shinydashboard",
       "shinyFiles", "shinyjs","shinythemes",  "shinyWidgets",  "sp",
       "spdep",    "tidyverse",  "tmap",  "viridis"
       #, "webshot" removed ro performance testing
       ))

  # from Claude
  #install.packages(c("cluster", "ggplot2", "gridExtra", "reshape2", "dplyr", "RColorBrewer", "viridis"))

options(shiny.maxRequestSize = 1000*1024^2)

options(warn = -1)
source("functions.R")

save_dir <- "../data/"
input_dir <- "../input/"
output_dir <- "../output/"
pareto_path <- "../data/pareto_fitness.txt" #used too frequently..
if(!dir.exists(save_dir)){  dir.create(save_dir)}
if(!dir.exists(output_dir)){  dir.create(output_dir)}