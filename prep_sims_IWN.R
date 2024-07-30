
# rm(list=ls())

# PRELIMINARIES ---------------------------------------------------------------

# This script uses renv to preserve the R environment specs (e.g., package versions.)
library(renv)
# run this if you want to reproduce results using the R environment we had:
# renv::restore()

to.load = c("dplyr",
            "data.table",
            "purrr",
            "tidyr",
            "stringr",
            "tibble",
            "ggplot2",
            "testthat",
            "plotly",
            "htmlwidgets", # for saving plotly
            "here",
            "xtable",
            "readxl")

# load within installation if needed
for (pkg in to.load) {
  
  cat( paste("\nAbout to try loading package", pkg) )
  
  tryCatch({
    # eval below needed because library() will otherwise be confused
    # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
    eval( bquote( library( .(pkg) ) ) )
  }, error = function(err) {
    install.packages(pkg)
  })
  
}

# run this only if you want to update the R environment specs
# renv::snapshot()


# set working directories
code.dir = here()

data.dir = str_replace_all( string = here(),
                            replacement = "Results/2024-07-30 - as in first journal submission",
                            pattern = "Code" ) 

results.dir = str_replace_all( string = here(),
                               replacement = "Results/2024-07-30 - as in first journal submission",
                               pattern = "Code" ) 
setwd(results.dir)  # check it

# generic results dir
# data.dir = str_replace_all( string = here(),
#                             replacement = "Simulation results/Working results",
#                             pattern = "Simulation code" ) 
# 
# results.dir = str_replace_all( string = here(),
#                                replacement = "Simulation results/Working results",
#                                pattern = "Simulation code" ) 
# setwd(results.dir)  # check it


# below is the only absolute path
# write results directly to directory containing TeX manuscript in Overleaf so stats can be piped directly into text
# this is an absolute path because it must live in Dropbox, outside the project directory, in order to sync with Overleaf
# to reproduce results, just set this to any directory on your local machine
# results will be written to a csv file in that location
overleaf.dir.stats = "/Users/mmathur/Dropbox/Apps/Overleaf/IWN: Imputation without nightMARs (Overleaf)/R_objects"
setwd(overleaf.dir.stats)  # check it

# get helper fns
setwd(code.dir)
source("helper_IWN.R")

# no sci notation
options(scipen=999)

# get simulation data
# already aggregated by stitch_on_sherlock_IWN.R


# read in datasets
setwd(data.dir)
s = fread("stitched.csv")
# check when it was created
file.info("stitched.csv")$ctime


# SANITY CHECKS ---------------------------------------------------------------

### Error messages from missingness methods
table(s$overall.error)

temp = s %>% filter(overall.error == "0 (non-NA) cases")

# which scenarios produce the 0 non-NA cases?
# oh, it's just CC in the DAG that has no complete cases - makes sense :)
as.data.frame( temp %>% group_by(dag_name, coef_of_interest, method) %>%
  summarise(n()) )

# check reps per scenario
t = s %>% group_by(scen.name, method) %>%
  summarise( n() )
table(t$`n()`)



# MAKE AGG DATA ---------------------------------------------------------------

aggo = make_agg_data(s)
agg = wrangle_agg_data(.aggo = aggo)

table(agg$method_pretty)

setwd(data.dir)
fwrite(agg, "agg.csv")


