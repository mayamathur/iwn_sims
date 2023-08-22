
expit = function(p) exp(p) / (1 + exp(p))
logit = function(p) p / (1-p)





# DATA-GENERATION FNS ---------------------

# # TEST ONLY
# if ( FALSE ) {
#   p = tidyr::expand_grid(
#     
#     # methods to run for each simulation rep
#     rep.methods = "gold-std ; CC-adj ; CC-unadj ; MI-adj ; MI-unadj",
#     model = "OLS",
#     
#     # DAGs: hughes_1c, mediation_1, mediation_2d, R_to_Y_1, R_to_Y_2, R_to_Y_4, Y_to_R_1, Y_to_R_2, comm_cause_1, comm_cause_2
#     # Y_to_R_1 is same as Daniel's 4c
#     dag_name = c("mediation_1"),
#     N = 10^4,
#     # true OLS coefficient of A on Y
#     betaAY = c(1),
#     # OLS coeff of C on Y or vice versa
#     betaCY = c(1),
#     # OLS coef of A on C, if applicable
#     betaAC = c(1),
#     # logistic regression coef of C on R
#     betaCR = c(1),
#     
#     # which var(s) should be missing?
#     # options: "c('A', 'Y', 'C'), c('A'), c('Y')"
#     #missing_vars = "c('A', 'Y')"  # quotation marks must be single inside double
#     missing_vars = "c('Y')"  # quotation marks must be single inside double
#     #missing_vars = "c('A', 'Y')"  # quotation marks must be single inside double
#     
#   )
#   
#   p$scen = 1:nrow(p)
#   .p = p
#   
#   sim_obj = sim_data(.p = .p)
#   dm = sim_obj$dm
#   du = sim_obj$du
#   
#   cor(du)
# }

sim_data = function(.p) {
  
  if (.p$model != "OLS" ) stop("Only handles model OLS for now")
  
  # ~ DAG 1B -----------------------------
  # 1 analysis variable with RA-A collider
  
  if ( .p$dag_name == "1B" ) {
    du = data.frame( U1 = rnorm( n = .p$N ),
                     U2 = rnorm( n = .p$N ),
                     C1 = rnorm( n = .p$N ) )  # only including this because neither imputation method runs with 1 variable
    
    coef1 = 2
    
    # *IMPORTANT: if you change coefs, need to re-estimate beta below
    du = du %>% rowwise() %>%
      mutate( A1 = rnorm( n = 1,
                          mean = coef1*U1 ),
              
              B1 = rnorm( n = 1,
                          mean = coef1*U1 + coef1*U2 ),
              
              RA = rbinom( n = 1,
                           prob = expit(1*U2),
                           size = 1 ),
              
              A = ifelse(RA == 0, NA, A1),
              B = B1,
              C = C1)
    
    
    # cor(du %>% select(A1, B1, C1))
    
    # make dataset for imputation (standard way: all measured variables)
    di_std = du %>% select(B, C, A)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      # regression strings
      form_string = "A ~ 1"
      
      # gold-standard model uses underlying variables
      gold_form_string = "A1 ~ 1"
      
      beta = 0
      
      di_ours = du %>% select(C, A) 
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = "B"
    }
    
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"

      beta = 0.80  # got this empirically
      
      di_ours = NULL  # m-backdoor violated
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL # B is in target law
    }
    
  }  # end of .p$dag_name == "1B"
  
  
  # ~ DAG 1D -----------------------------
  # similar to 1B, but now B has its own missingness, so restricting
  #  the dataset to exclude it from imputation doesn't work
  
  if ( .p$dag_name == "1D" ) {
    
    du = data.frame( U1 = rnorm( n = .p$N ),
                     U2 = rnorm( n = .p$N ),
                     C1 = rnorm( n = .p$N ) )
    
    coef1 = 2
    
    #**IMPORTANT: when coef_of_interest = A,
    # this DAG has beta (below) estimated empirically.
    # if any parameters below change, you'll need to re-estimate it.
    du = du %>% rowwise() %>%
      mutate( A1 = rnorm( n = 1,
                          mean = coef1*U1 ),
              
              B1 = rnorm( n = 1,
                          mean = coef1*U1 + coef1*U2 ),
              
              RA = rbinom( n = 1,
                           prob = expit(3*U2),
                           size = 1 ),
              RB = rbinom( n = 1,
                           prob = 0.9,  # 10% missing
                           size = 1 ),
              #*interestingly, intercept of A less biased for standard MI methods when there is MORE missingness on B
              #  I guess this is because it plays less role in imputation model?
              
              A = ifelse(RA == 0, NA, A1),
              B = ifelse(RB == 0, NA, B1),
              C = C1)
    
    
    # cor(du %>% select(A1, B1, C1))
    
    # make dataset for imputation (standard way: all measured variables)
    di_std = du %>% select(A, B, C)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      
      # regression strings
      form_string = "A ~ 1"
      
      # gold-standard model uses underlying variables
      gold_form_string = "A1 ~ 1"
      
      beta = 0
      
      # and for our imputation
      di_ours = du %>% select(A, C)
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = "B"
    }
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"

      #@GOT THIS EMPIRICALLY from gold-std model
      #  a little tricky to get it theoretically because it's a spurious association
      #  rather than a causal effect
      #@will need to re-estimate this if any of above parameters change
      beta = 0.80
      
      # and for our imputation
      di_ours = NULL
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL
    }
    
  }  # end of .p$dag_name == "1D"
  
  
  # ~ DAG 1G (Type-D) -----------------------------
  
  if ( .p$dag_name == "1G" ) {
    
    du = data.frame( U1 = rnorm( n = .p$N ),
                     U2 = rnorm( n = .p$N ) )

    #coef1 = 2 # as in 2023-08-21 sims, where MICE unexpected performed badly
    coef1 = 1
    
    du = du %>% rowwise() %>%
      mutate( A1 = rnorm( n = 1,
                          mean = coef1*U1 ),
              
              D1 = rnorm( n = 1,
                          mean = coef1*U1 + coef1*U2 ),
              
              B1 = rnorm( n = 1,
                          mean = coef1*A1 ),
              
              C1 = rnorm( n = 1,
                          mean = (coef1/2)*B1 ),
              
              RA = rbinom( n = 1,
                           prob = expit(1*C1 + 1*U2),
                           size = 1 ),
              RB = rbinom( n = 1,
                           prob = 1, # NOT MISSING to avoid unblockable m-bd path
                           size = 1 ),
              
              A = ifelse(RA == 0, NA, A1),
              B = ifelse(RB == 0, NA, B1),
              C = C1,
              D = D1)
    
  
    #cor(du %>% select(A,B,C,D))
    
    # make dataset for imputation (standard way: all measured variables)
    di_std = du %>% select(A, B, C, D)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"

      beta = coef1
      
      # and for our imputation
      di_ours = du %>% select(A, B, C)
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = "D"
    }
    
  }  # end of .p$dag_name == "1G"
  
  
  # ~ DAG 1F (Type-D) -----------------------------
  
  if ( .p$dag_name == "1F" ) {
    
    du = data.frame( U1 = rnorm( n = .p$N ),
                     U2 = rnorm( n = .p$N ) )
    
    #coef1 = 2 # as in 2023-08-21 sims, where MICE unexpected performed badly
    coef1 = 1
    
    du = du %>% rowwise() %>%
      mutate( A1 = rnorm( n = 1,
                          mean = coef1*U1 ),
              
              D1 = rnorm( n = 1,
                          mean = coef1*U1 + coef1*U2 ),
              
              B1 = rnorm( n = 1,
                          mean = coef1*A1 ),
              
              C1 = rnorm( n = 1,
                          mean = (coef1/2)*B1 ),
              
              RA = rbinom( n = 1,
                           prob = 1, # not missing
                           size = 1 ),
              RB = rbinom( n = 1,
                           prob = expit(1*C1 + 1*U2), 
                           size = 1 ),
              
              A = ifelse(RA == 0, NA, A1),
              B = ifelse(RB == 0, NA, B1),
              C = C1,
              D = D1)
    
    
    #cor(du %>% select(A,B,C,D))
    
    # make dataset for imputation (standard way: all measured variables)
    di_std = du %>% select(A, B, C, D)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coef1
      
      # and for our imputation
      di_ours = du %>% select(A, B, C)
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = "D"
    }
    
  }  # end of .p$dag_name == "1F"
  
  
  
  
  # ~ DAG 1H (Type-M) -----------------------------
  
  if ( .p$dag_name == "1H" ) {
    
    du = data.frame( A1 = rnorm( n = .p$N ) )
    
    coef1 = 1
    #coef1 = 2  # as in 2023-08-21 sims, where MICE unexpected performed badly
    
    #**IMPORTANT: this DAG has beta (below) estimated empirically.
    # if any parameters below change, you'll need to re-estimate it.
    du = du %>% rowwise() %>%
      mutate( C1 = rnorm( n = 1,
                          mean = coef1*A1 ),
              
              B1 = rnorm( n = 1,
                          # coef1 is the direct effect of A1
                          mean = coef1*C1 + coef1*A1 ),
              
              RB = rbinom( n = 1,
                           prob = expit(1*C1), 
                           size = 1 ),
              
              A = A1,
              B = ifelse(RB == 0, NA, B1),
              C = C1)
  
    #cor(du %>% select(A1, B1, C1))
    
    # make dataset for imputation (standard way: all measured variables)
    di_std = du %>% select(A, B, C)
    
    
    ### For just the intercept of A
    if ( .p$coef_of_interest == "(Intercept)" ){ 
      stop("Intercept not implemented for this DAG")
    }
    
    ### For the A-B association
    if ( .p$coef_of_interest == "A" ){ 
      # regression strings
      form_string = "B ~ A"
      
      # gold-standard model uses underlying variables
      gold_form_string = "B1 ~ A1"
      
      beta = coef1^2 + coef1  # mediation total effect
      
      # and for our imputation
      di_ours = NULL  # same as std imputation
      
      # custom predictor matrix for MICE-ours-pred
      exclude_from_imp_model = NULL
    }
    
  }  # end of .p$dag_name == "1H"
  
  
  # ~ Finish generating data ----------------
  
  # marginal prevalences
  colMeans(du, na.rm = TRUE)
  
  # can't use llist here in case some list elements are NULL (e.g., di_ours)
  # return( llist(du,
  #               di_std,
  #               di_ours,
  #               exclude_from_imp_model,
  #               form_string,
  #               gold_form_string,
  #               coef_of_interest,
  #               beta) )
  
  return( list(du = du,
               di_std = di_std,
               di_ours = di_ours,
               exclude_from_imp_model = exclude_from_imp_model,
               form_string = form_string,
               gold_form_string = gold_form_string,
               beta = beta) )
  
}


# ANALYSIS METHOD FNS ---------------------

# miss_method: CC, MI, gold, IPW
# model: logistic, OLS
fit_regression = function(form_string,
                          model,
                          coef_of_interest,
                          miss_method,
                          du,
                          imps) {
  
  
  # # test only
  # form_string = CC_adj_form_string
  # model = "OLS"
  # miss_method = "CC"
  
  
  #if ( miss_method %in% c("CC", "IPW") ) dat = dm
  if ( miss_method == "MI" ) dat = imps
  if ( miss_method %in% c("gold", "CC") ) dat = du
  
  # ~ CC and gold std  ---------------------
  if ( miss_method %in% c("CC", "gold") ) {
    
    if ( model == "OLS" ) {
      #if ( nuni(dat$Y) <= 2 ) stop("You have a binary outcome but are fitting OLS with model-based SEs; need to allow robust SEs")
      
      mod = lm( eval( parse(text = form_string) ),
                data = dat )
      
    }
    
    if ( model == "logistic" ) {
      mod = glm( eval( parse(text = form_string) ),
                 data = dat,
                 family = binomial(link = "logit") )
    }
    
    bhats = coef(mod)
    CI = as.numeric( confint(mod)[coef_of_interest,] )
    
    return( list( stats = data.frame( bhat = as.numeric( bhats[coef_of_interest] ),
                                      bhat_lo = CI[1],
                                      bhat_hi = CI[2],
                                      bhat_width = CI[2] - CI[1] ) ) )
  }
  
  # ~ MI  ---------------------
  if ( miss_method == "MI" ) {
    
    
    if ( model == "OLS" ) {
      #if ( nuni( complete(imps,1)$Y) <= 2 ) stop("You have a binary outcome but are fitting OLS with model-based SEs; need to allow robust SEs")
      
      # works for both MICE and Amelia
      mod = with(imps,
                 lm( eval( parse(text = form_string) ) ) )
    }
    
    
    if ( model == "logistic" ) {
      mod = with(imps,
                 glm( eval( parse(text = form_string) ),
                      family = binomial(link = "logit") ) )
    }
    
    mod_pool = pool(mod)
    summ = summary(mod_pool, conf.int = TRUE)
    
    bhat_lo = summ$`2.5 %`[ summ$term == coef_of_interest ]
    bhat_hi = summ$`97.5 %`[ summ$term == coef_of_interest ]
    
    return( list( stats = data.frame( bhat = mod_pool$pooled$estimate[ mod_pool$pooled$term == coef_of_interest ],
                                      bhat_lo = bhat_lo,
                                      bhat_hi = bhat_hi,
                                      bhat_width = bhat_hi - bhat_lo ) ) )
  }
  
}


# MODEL-FITTING HELPERS ---------------------

# ~~ Wrapper Fn to Safely Run a Method -------

# See note at the beginning of this script
#  this fn automatically runs the method within a tryCatch loop, 
#  records any error messages, and writes a results row to global var rep.res whether 
#  or not the estimation method threw an error

# Important: this fn works if method.fn() returns multiple rows
# BUT in that case, it assumes that the CIs are shared for all rows of that method

# expects global vars: all.errors, rep.res
# directly edits res via superassignment
run_method_safe = function( method.label,
                            method.fn,
                            .rep.res ) {
  
  cat( paste("\n run_method_safe flag 1: about to try running method", method.label) )
  
  
  tryCatch({
    
    method.output = method.fn()
    new.rows = method.output$stats
    
    if ( !exists("new.rows") ) {
      cat("\n\n**** Object new.rows didn't exist for method", method.label)
      cat("\nHere is method.output:\n")
      print(method.output)
    }
    
    cat( paste("\n run_method_safe flag 2: done calling method.fn() for", method.label) )
    
    error = NA
    
  }, error = function(err) {
    # needs to be superassignment because inside the "error" fn
    error <<- err$message
    
    # only need one variable in the blank dataframe since bind_rows below
    #  will fill in the rest
    new.rows <<- data.frame( method = method.label )
    
  })
  
  new.rows = new.rows %>% add_column( method = method.label, .before = 1 )
  new.rows$overall.error = error
  
  
  if ( nrow(.rep.res) == 0 ) .rep.res = new.rows else .rep.res = bind_rows(.rep.res, new.rows)
  return(.rep.res) 
  
}



# coefName: which coefficient to report
# ols: the OLS model from lm()
my_ols_hc0 = function( coefName, ols ){
  
  ( bhat.ols = coef(ols)[coefName] )
  
  # heteroskedasticity-consistent robust SEs:
  (se.hc0 = sqrt( sandwich::vcovHC( ols, type="HC0")[coefName, coefName] ) )
  
  tcrit = qt(.975, df = ols$df.residual)
  t = as.numeric( abs(bhat.ols / se.hc0) )
  
  return( data.frame(
    est = bhat.ols,
    se = se.hc0,
    lo = bhat.ols - tcrit * se.hc0,
    hi = bhat.ols + tcrit * se.hc0,
    pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ) ) )
}


# SMALL GENERIC HELPERS ---------------------

# quickly look at results when running doParallel locally
srr = function(.rep.res) {
  
  
  cat("\n")
  print( .rep.res %>%
           mutate_if(is.numeric, function(x) round(x,2)) )
  cat("\n")
  
}

# quick mean with NAs removed
meanNA = function(x){
  mean(x, na.rm = TRUE)
}


# check CI coverage
covers = function( truth, lo, hi ) {
  return( as.numeric( (lo <= truth) & (hi >= truth) ) )
}

# get names of dataframe containing a string
namesWith = function(pattern, dat){
  names(dat)[ grepl(pattern = pattern, x = names(dat) ) ]
}


# quick length(unique)
nuni = function(x) {
  length(unique(x))
}

# (re-)install package AND its dependencies
# useful for stupid rstan issues in which rstan itself it UTD but not its dependencies
# https://stackoverflow.com/questions/21010705/update-a-specific-r-package-and-its-dependencies
instPkgPlusDeps <- function(pkg, install = FALSE,
                            which = c("Depends", "Imports", "LinkingTo"),
                            inc.pkg = TRUE) {
  stopifnot(require("tools")) ## load tools
  ap <- available.packages() ## takes a minute on first use
  ## get dependencies for pkg recursively through all dependencies
  deps <- package_dependencies(pkg, db = ap, which = which, recursive = TRUE)
  ## the next line can generate warnings; I think these are harmless
  ## returns the Priority field. `NA` indicates not Base or Recommended
  pri <- sapply(deps[[1]], packageDescription, fields = "Priority")
  ## filter out Base & Recommended pkgs - we want the `NA` entries
  deps <- deps[[1]][is.na(pri)]
  ## install pkg too?
  if (inc.pkg) {
    deps = c(pkg, deps)
  }
  ## are we installing?
  if (install) {
    install.packages(deps)
  }
  deps ## return dependencies
}

# example
# instPkgPlusDeps("fields")


# CLUSTER FNS ---------------------------------------------------------------

# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
# DO NOT CHANGE THE INDENTATION IN THE BELOW OR ELSE SLURM 
#  WILL SILENTLY IGNORE THE BATCH COMMANDS DUE TO EXTRA WHITESPACE!!
sbatch_skeleton <- function() {
  return(
    "#!/bin/bash
#################
#set a job name  
#SBATCH --job-name=JOBNAME
#################  
#a file for job output, you can check job progress
#SBATCH --output=OUTFILE
#################
# a file for errors from the job
#SBATCH --error=ERRORFILE
#################
#time you think you need; default is one hour
#SBATCH --time=JOBTIME
#################
#quality of service; think of it as job priority
#SBATCH --qos=QUALITY
#################
#submit to both owners and normal partition
#SBATCH -p normal,owners
#################
#number of nodes you are requesting
#SBATCH --nodes=NODENUMBER
#################
#memory per node; default is 4000 MB
#SBATCH --mem=MEMPERNODE
#you could use --mem-per-cpu; they mean what we are calling cores
#################
#get emailed about job BEGIN, END, and FAIL
#SBATCH --mail-type=MAILTYPE
#################
#who to send email to; please change to your email
#SBATCH  --mail-user=USER_EMAIL
#################
#task to run per node; each node has 16 cores
#SBATCH --ntasks=TASKS_PER_NODE
#################
#SBATCH --cpus-per-task=CPUS_PER_TASK
#now run normal batch commands

ml load v8
ml load R/4.2.0
R -f PATH_TO_R_SCRIPT ARGS_TO_R_SCRIPT")
}



generateSbatch <- function(sbatch_params,
                           runfile_path = NA,
                           run_now = F) {
  
  #sbatch_params is a data frame with the following columns
  #jobname: string, specifies name associated with job in SLURM queue
  #outfile: string, specifies the name of the output file generated by job
  #errorfile: string, specifies the name of the error file generated by job
  #jobtime: string in hh:mm:ss format, max (maybe soft) is 48:00:00 
  #specifies the amoung of time job resources should be allocated
  #jobs still running after this amount of time will be aborted
  #quality: kind of like priority, normal works
  #node_number, integer: the number of nodes (computers w/16 cpus each) to allocate 
  #mem_per_node, integer: RAM, in MB, to allocate to each node
  #mailtype, string: ALL, BEGIN, END, FAIL: what types of events should you be notified about via email
  #user_email string: email address: email address to send notifications
  #tasks_per_node: integer, number of tasks, you should probably use 1
  #cpus_per_task: integer, 1-16, number of cpus to use, corresponds to number of available cores per task
  #path_to_r_script: path to r script on sherlock
  #args_to_r_script: arguments to pass to r script on command line
  #write_path: where to write the sbatch file
  #server_sbatch_path: where sbatch files will be stored on sherlock
  #runfile_path is a string containing a path at which to write an R script that can be used to run
  #the batch files generated by this function. 
  #if NA, no runfile will be written
  #run_now is a boolean specifying whether batch files should be run as they are generated
  
  sbatches <- list()
  if (!is.na(runfile_path)) {
    outfile_lines <- c(paste0("# Generated on ",  Sys.time()))
  }
  for (sbatch in 1:nrow(sbatch_params) ) {
    gen_batch <- sbatch_skeleton()
    #set job name
    if (is.null(sbatch_params$jobname[sbatch])) { 
      gen_batch <- gsub("JOBNAME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBNAME", sbatch_params$jobname[sbatch], gen_batch) 
    }
    #set outfile name
    if (is.null(sbatch_params$outfile[sbatch])) { 
      gen_batch <- gsub("OUTFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("OUTFILE", sbatch_params$outfile[sbatch], gen_batch) 
    }
    #set errorfile name
    if (is.null(sbatch_params$errorfile[sbatch])) { 
      gen_batch <- gsub("ERRORFILE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ERRORFILE", sbatch_params$errorfile[sbatch], gen_batch) 
    }
    #set jobtime
    if (is.null(sbatch_params$jobtime[sbatch])) { 
      gen_batch <- gsub("JOBTIME", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("JOBTIME", sbatch_params$jobtime[sbatch], gen_batch) 
    }
    #set quality
    if (is.null(sbatch_params$quality[sbatch])) { 
      gen_batch <- gsub("QUALITY", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("QUALITY", sbatch_params$quality[sbatch], gen_batch) 
    }
    #set number of nodes
    if (is.null(sbatch_params$node_number[sbatch])) { 
      gen_batch <- gsub("NODENUMBER", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("NODENUMBER", sbatch_params$node_number[sbatch], gen_batch) 
    }
    #set memory per node
    if (is.null(sbatch_params$mem_per_node[sbatch])) { 
      gen_batch <- gsub("MEMPERNODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MEMPERNODE", sbatch_params$mem_per_node[sbatch], gen_batch) 
    }
    #set requested mail message types
    if (is.null(sbatch_params$mailtype[sbatch])) { 
      gen_batch <- gsub("MAILTYPE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("MAILTYPE", sbatch_params$mailtype[sbatch], gen_batch) 
    }
    #set email at which to receive messages
    if (is.null(sbatch_params$user_email[sbatch])) { 
      gen_batch <- gsub("USER_EMAIL", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("USER_EMAIL", sbatch_params$user_email[sbatch], gen_batch) 
    }
    #set tasks per node
    if (is.null(sbatch_params$tasks_per_node[sbatch])) { 
      gen_batch <- gsub("TASKS_PER_NODE", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("TASKS_PER_NODE", sbatch_params$tasks_per_node[sbatch], gen_batch) 
    }
    #set cpus per task
    if (is.null(sbatch_params$cpus_per_task[sbatch])) { 
      gen_batch <- gsub("CPUS_PER_TASK", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("CPUS_PER_TASK", sbatch_params$cpus_per_task[sbatch], gen_batch) 
    }
    #set path to r script
    if (is.null(sbatch_params$path_to_r_script[sbatch])) { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("PATH_TO_R_SCRIPT", sbatch_params$path_to_r_script[sbatch], gen_batch) 
    }
    #set args to r script
    if (is.null(sbatch_params$args_to_r_script[sbatch])) { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", "unnamed", gen_batch) 
    } else { 
      gen_batch <- gsub("ARGS_TO_R_SCRIPT", sbatch_params$args_to_r_script[sbatch], gen_batch) 
    }
    
    #write batch file
    if (is.null(sbatch_params$write_path[sbatch])) { 
      cat(gen_batch, file = paste0("~/sbatch_generated_at_", gsub(" |:|-", "_", Sys.time()) ), append = F)
    } else { 
      cat(gen_batch, file = sbatch_params$write_path[sbatch], append = F)
    }
    
    if (!is.na(sbatch_params$server_sbatch_path[sbatch])) {
      outfile_lines <- c(outfile_lines, paste0("system(\"sbatch ", sbatch_params$server_sbatch_path[sbatch], "\")"))
    } 
    sbatches[[sbatch]] <- gen_batch
  }
  if (!is.na(runfile_path)) {
    cat(paste0(outfile_lines, collapse = "\n"), file = runfile_path)
  }
  if(run_now) { system(paste0("R -f ", runfile_path)) } 
  
  return(sbatches)
}


# looks at results files to identify sbatches that didn't write a file
# .max.sbatch.num: If not passed, defaults to largest number in actually run jobs.

sbatch_not_run = function(.results.singles.path,
                          .results.write.path,
                          .name.prefix,
                          .max.sbatch.num = NA ) {
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # extract job numbers
  sbatch.nums = as.numeric( unlist( lapply( strsplit( keepers, split = "_"), FUN = function(x) x[5] ) ) )
  
  # check for missed jobs before the max one
  if ( is.na(.max.sbatch.num) ) .max.sbatch.num = max(sbatch.nums)
  all.nums = 1 : .max.sbatch.num
  missed.nums = all.nums[ !all.nums %in% sbatch.nums ]
  
  # give info
  print( paste("The max job number is: ", max(sbatch.nums) ) )
  print( paste( "Number of jobs that weren't run: ",
                ifelse( length(missed.nums) > 0, length(missed.nums), "none" ) ) )
  
  if( length(missed.nums) > 0 ) {
    setwd(.results.write.path)
    write.csv(missed.nums, "missed_job_nums.csv")
  }
  
  return(missed.nums)
  
}

# FN: STITCH RESULTS FILES -------------------------------------

# given a folder path for results and a common beginning of file name of results files
#   written by separate workers in parallel, stitch results files together into a
#   single csv.

stitch_files = function(.results.singles.path, .results.stitched.write.path=.results.singles.path,
                        .name.prefix, .stitch.file.name="stitched_model_fit_results.csv") {
  
  # .results.singles.path = "/home/groups/manishad/MRM/sim_results/long"
  # .results.stitched.write.path = "/home/groups/manishad/MRM/sim_results/overall_stitched"
  # .name.prefix = "long_results"
  # .stitch.file.name="stitched.csv"
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  
  # grab variable names from first file
  names = names( read.csv(keepers[1] )[-1] )
  
  # read in and rbind the keepers
  tables <- lapply( keepers, function(x) read.csv(x, header= TRUE) )
  s <- do.call(rbind, tables)
  
  names(s) = names( read.csv(keepers[1], header= TRUE) )
  
  if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
  write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  return(s)
}


# quickly look at results from job #1

res1 = function() {
  setwd("/home/groups/manishad/SAPH/long_results")
  rep.res = fread("long_results_job_1_.csv")
  srr()
  
  cat("\nErrors by method:" )
  print( rep.res %>% group_by(method) %>%
           summarise(prop.error = mean( overall.error != "" ) ) )
  
  #table(rep.res$overall.error)
  
  cat("\n\nDim:", dim(rep.res))
  cat("\n\nReps completed:", nrow(rep.res)/nuni(rep.res$method))
}
