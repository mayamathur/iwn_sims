

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
                   replacement = "Simulation results/2024-06-13 - rerun 1B, 1D, 1Fb",
                   pattern = "Simulation code" ) 

results.dir = str_replace_all( string = here(),
                               replacement = "Simulation results/Working results",
                               pattern = "Simulation code" ) 
setwd(results.dir)  # check it

overleaf.dir.stats = "/Users/mmathur/Dropbox/Apps/Overleaf/IWN: Imputation without nightMARs (Overleaf)/R_objects"
setwd(overleaf.dir.stats)  # check it

# get helper fns
setwd(code.dir)
source("helper_IWN.R")

# no sci notation
options(scipen=999)

# get simulation data
# already aggregated by stitch_on_sherlock_IWN.R


# ANALYZE SIMS ---------------------------------------------------------------

setwd(data.dir)
aggo = read_excel("2024-06-13 agg.xlsx")

#bm: decide how to present this to make it similar to Table 1 :)

# TEMP
.aggo = aggo


wrangle_agg_data = function(.aggo) {
  
  agg = .aggo
  
  # recode variables
  agg$coef_of_interest_pretty = agg$coef_of_interest
  agg$coef_of_interest_pretty[ agg$dag_name %in% c("1B", "1D") & agg$coef_of_interest == "(Intercept)"] = "E[A]"
  agg$coef_of_interest_pretty[ agg$dag_name %in% c("1Fb") & agg$coef_of_interest == "(Intercept)"] = "E[B]" 
  agg$coef_of_interest_pretty[ agg$coef_of_interest == "A"] = "E[B | A]" 
  # check it
  agg %>% group_by(dag_name, coef_of_interest) %>% 
    summarise(unique(coef_of_interest_pretty))
  
  agg$dag_name_pretty = agg$dag_name
  agg$dag_name_pretty[ agg$dag_name == "1B" ] = "DAG (a)"
  agg$dag_name_pretty[ agg$dag_name == "1D" ] = "DAG (b)"
  agg$dag_name_pretty[ agg$dag_name == "1Fb" ] = "DAG (c)"
  
  agg$method_pretty = agg$method
  agg$method_pretty[ agg$method == "gold" ] = "Benchmark"
  agg$method_pretty[ agg$method == "CC" ] = "Complete-case"
  agg$method_pretty[ agg$method == "Am-std" ] = "Amelia (standard)"
  agg$method_pretty[ agg$method == "Am-ours" ] = "Amelia (m-backdoor)"
  agg$method_pretty[ agg$method == "MICE-std" ] = "MICE (standard)"
  agg$method_pretty[ agg$method == "MICE-ours" ] = "MICE (m-backdoor)"
  
  return(agg)
}

agg = wrangle_agg_data(.aggo = aggo)

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

# pick DAG name: 1B, 1D, 1Fb
.dag_name = "1B"


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













