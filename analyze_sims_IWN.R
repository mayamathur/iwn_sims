
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


# # generic dirs (save)
# data.dir = str_replace_all( string = here(),
#                    replacement = "Simulation results/Working results",
#                    pattern = "Simulation code" ) 
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


# read in dataset
setwd(data.dir)
agg = fread("agg.csv")
# check when it was created
file.info("agg.csv")$ctime


# MAIN RESULTS TABLE ---------------------------------------------------------------

table(agg$method_pretty)

method_keepers = c("gold", "CC", "Am-std", "Am-ours", "MICE-std", "MICE-ours")
agg = agg %>% filter(method %in% method_keepers)
agg = droplevels(agg)


# reorder methods
correct_order = c("Benchmark", "Complete-case", "Amelia (standard)", "MICE (standard)", "Amelia (m-backdoor)", "MICE (m-backdoor)")
agg$method_pretty = factor(agg$method_pretty, levels = correct_order)
levels(agg$method_pretty)

t = agg %>% select(dag_name_pretty, coef_of_interest_pretty, method_pretty, BhatBias, BhatRMSE, BhatCover) %>%
  arrange( dag_name_pretty, coef_of_interest_pretty, method_pretty )


View(t)

print( xtable(t), include.rownames = FALSE)



# DESCRIPTIVES FOR EACH DAG ---------------------------------------------------------------

# for more descriptives, see design_missingness_mechanisms.R

# ~ Simulate Dataset ------------------------------

# pick DAG name: 1B, 1D, 1Fb, 1J
.dag_name = "1J"


# we just want one dataset; using NAs for sim params that don't matter in this context
p = tidyr::expand_grid(
  
  rep.methods = NA,
  
  model = "OLS",
  coef_of_interest = "A",
  
  imp_m = NA,
  imp_maxit = NA,
  mice_method = NA,
  
  dag_name = c( .dag_name ),
  N = c(100000)
)


sim_obj = sim_data(.p = p)
du = sim_obj$du


# ~ Correlation matrix ------------------------------

# subset to variables of interest
if ( .dag_name == "1B" ) keepers = c("A1", "B1", "C1", "RA") 
if ( .dag_name == "1D" ) keepers = c("A1", "B1", "C1", "RA", "RB")
if ( .dag_name == "1Fb" ) keepers = c("A1", "B1", "D1", "RB")
if ( .dag_name == "1J" ) keepers = c("A1", "B1", "C1", "D1", "RC", "RD")

cormat = cor( du %>% select( all_of(keepers) ) )

cormat = round(cormat,2)

cormat[ !upper.tri(cormat) ] = NA

xtable(cormat)

# ~ E[missingness indicators] ------------------------------

# proportion missing
if ("RA" %in% names(du)) {
  update_result_csv(name = paste( "DAG ", .dag_name, " mean RA", sep = "" ),
                    value = round( mean(du$RA), 2) )
  
}


if ("RB" %in% names(du)) {
  update_result_csv(name = paste( "DAG ", .dag_name, " mean RB", sep = "" ),
                    value = round( mean(du$RB), 2) )
  
}

if (.dag_name == "1J"){
  
  # note: in stats_for_paper, nodes are relabeled to match paper rather than code
  update_result_csv(name = paste( "DAG ", .dag_name, " mean RD", sep = "" ),
                    value = round( mean(du$RC), 2) )
  
  update_result_csv(name = paste( "DAG ", .dag_name, " mean RE", sep = "" ),
                    value = round( mean(du$RD), 2) )
  
  
}




# INVESTIGATE DAG (D) - FILE MATCHING ---------------------------------------------------------------

### Q: Why does MICE have dramatically higher coverage than Amelia? Is its CI much wider?
# A: Yes.

s %>% filter(dag_name == "1J") %>%
  group_by(method) %>%
  summarise( mean(bhat_lo), 
             mean(bhat_hi),
             mean(bhat_width))

### Q: Do Amelia and MICE produce any errors/warnings?
# A: No.

temp = s %>% filter(dag_name == "1J" & method == "Am-std")
table( temp$overall.error )


temp = s %>% filter(dag_name == "1J" & method == "MICE-std")
table( temp$overall.error )





