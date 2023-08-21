
# PRELIMINARIES -----------------------------------------

path = "/home/groups/manishad/IWN"
setwd(path)
source("helper_IWN.R")

allPackages = c("here",
                "crayon",
                "dplyr",
                "foreach",
                "doParallel",
                "data.table",
                "purrr",
                "tidyr",
                "tibble",
                "testthat",
                "Hmisc",
                "stringr")

( packagesNeeded = allPackages[ !( allPackages %in% installed.packages()[,"Package"] ) ] )
if( length(packagesNeeded) > 0 ) install.packages(packagesNeeded)

# load all packages
lapply( allPackages,
        require,
        character.only = TRUE)

#**you need to see all "TRUE" printed by this in order for the package to actually be loaded



# SET SIMULATION PARAMETERS -----------------------------------------

scen.params = tidyr::expand_grid(
  
  rep.methods = "gold ; CC ; MICE-std ; Am-std ; MICE-ours ; MICE-ours-pred ; Am-ours",
  model = "OLS",
  
  imp_m = 50,
  imp_maxit = 100,
  
  dag_name = c( "1G" ), # change rep.method string if you change DAGs! 
  N = c(4000) 
)

# remove combos that aren't implemented
scen.params = scen.params %>% filter( !(dag_name %in% c("1G", "1H") &
                                          coef_of_interest == "(Intercept)") )
# add scen numbers
start.at = 1
scen.params = scen.params %>% add_column( scen = start.at : ( nrow(scen.params) + (start.at - 1) ),
                                          .before = 1 )


( n.scen = nrow(scen.params) )
# look at it
head( as.data.frame(scen.params) )

# write the csv file of params (to Sherlock)
setwd(path)
write.csv( scen.params, "scen_params.csv", row.names = FALSE )


########################### GENERATE SBATCHES ###########################

# load functions for generating sbatch files
source("helper_IWN.R")

# number of sbatches to generate (i.e., iterations within each scenario)
n.reps.per.scen = 500
n.reps.in.doParallel = 50
( n.files = ( n.reps.per.scen / n.reps.in.doParallel ) * n.scen )


path = "/home/groups/manishad/IWN"

scen.name = rep( scen.params$scen, each = ( n.files / n.scen ) )
jobname = paste("job", 1:n.files, sep="_")
outfile = paste("/home/groups/manishad/IWN/rmfiles/rm_", 1:n.files, ".out", sep="")
errorfile = paste("/home/groups/manishad/IWN/rmfiles/rm_", 1:n.files, ".err", sep="")
write_path = paste(path, "/sbatch_files/", 1:n.files, ".sbatch", sep="")
runfile_path = paste(path, "/testRunFile.R", sep="")

sbatch_params <- data.frame(jobname,
                            outfile,
                            errorfile,
                            jobtime = "00:30:00", 
                            quality = "normal",
                            node_number = 1,
                            mem_per_node = 64000,
                            mailtype =  "NONE",
                            user_email = "mmathur@stanford.edu",
                            tasks_per_node = 16,
                            cpus_per_task = 1,
                            path_to_r_script = paste(path, "/doParallel_IWN.R", sep=""),
                            args_to_r_script = paste("--args", jobname, scen.name, sep=" "),
                            write_path,
                            stringsAsFactors = F,
                            server_sbatch_path = NA)

generateSbatch(sbatch_params, runfile_path)

n.files

# run just the first one


# sbatch -p qsu,owners,normal /home/groups/manishad/IWN/sbatch_files/1.sbatch

# xxx files
path = "/home/groups/manishad/IWN"
setwd( paste(path, "/sbatch_files", sep="") )
for (i in 1:10) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/IWN/sbatch_files/", i, ".sbatch", sep="") )
}





######## If Running Only Some Jobs To Fill Gaps ########

# run in Sherlock ml load R
path = "/home/groups/manishad/IWN"
setwd(path)
source("helper_IWN.R")

missed.nums = sbatch_not_run( "/home/groups/manishad/IWN/long_results",
                              "/home/groups/manishad/IWN/long_results",
                              .name.prefix = "long_results",
                              .max.sbatch.num = 45 )



setwd( paste(path, "/sbatch_files", sep="") )
for (i in missed.nums) {
  system( paste("sbatch -p qsu,owners,normal /home/groups/manishad/IWN/sbatch_files/", i, ".sbatch", sep="") )
}