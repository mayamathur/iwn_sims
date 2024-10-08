
# IMPORTANT NOTES -----------------------------


# for interactive Sherlock:
# path = "/home/groups/manishad/IWN"; setwd(path); source("doParallel_IWN.R")


# because Sherlock 2.0 restores previous workspace
rm( list = ls() )


# are we running locally?
run.local = FALSE
#  run.local = TRUE

# should we set scen params interactively on cluster?
# *if you accidently set this to TRUE and run via sbatches on cluster,
#   they will all run the same scenario! 
interactive.cluster.run = FALSE

# should lots of output be printed for each sim rep?
verbose = FALSE

# ~~ Packages -----------------------------------------------
toLoad = c("crayon",
           "dplyr",
           "foreach",
           "doParallel",
           "data.table",
           "purrr",
           "tidyr",
           "tibble",
           "testthat",
           "Hmisc",
           "stringr",
           "mice",
           "Amelia",
           "sandwich")

if ( run.local == TRUE | interactive.cluster.run == TRUE ) toLoad = c(toLoad, "here")


# SET UP FOR CLUSTER OR LOCAL RUN ------------------------------

# ~~ Cluster Run ----------------------------------------
if (run.local == FALSE ) {
  
  # load command line arguments
  args = commandArgs(trailingOnly = TRUE)
  
  cat("\n\n args received from sbatch file:", args)
  
  jobname = args[1]
  scen = args[2]  # this will be a number
  
  # load packages with informative messages if one can't be installed
  # **Common reason to get the "could not library" error: You did ml load R/XXX using an old version
  any.failed = FALSE
  for (pkg in toLoad) {
    
    cat( paste("\nAbout to try loading package", pkg) )
    
    tryCatch({
      # eval below needed because library() will otherwise be confused
      # https://www.mitchelloharawild.com/blog/loading-r-packages-in-a-loop/
      eval( bquote( library( .(pkg) ) ) )
    }, error = function(err) {
      cat( paste("\n*** COULD NOT LOAD PACKAGE:", pkg) )
      any.failed <<- TRUE
    })
    
  }
  if ( any.failed == TRUE ) stop("Some packages couldn't be loaded. See outfile for details of which ones.")
  
  # helper code
  path = "/home/groups/manishad/IWN"
  setwd(path)
  source("helper_IWN.R")
  
  
  # get scen parameters (made by genSbatch.R)
  setwd(path)
  scen.params = read.csv( "scen_params.csv" )
  p <<- scen.params[ scen.params$scen == scen, ]
  
  cat("\n\nHEAD OF ENTIRE SCEN.PARAMS:\n")
  print(p)
  
  
  
  # ~~*********** Set cluster sim reps ----------------
  # simulation reps to run within this job
  # **this need to match n.reps.in.doParallel in the genSbatch script
  sim.reps = 100
  
  # set the number of cores
  registerDoParallel(cores=16)
  
}



# FOR LOCAL USE  ------------------------------
if ( run.local == TRUE ) {
  
  lapply( toLoad,
          require,
          character.only = TRUE)
  
  
  # helper fns
  code.dir = here()
  setwd(code.dir)
  source("helper_IWN.R")
  
  # for saving intermediate results
  data.dir = str_replace( string = here(),
                          pattern = "Simulation code",
                          replacement = "Simulation results" )
  
  
  # ~~ ********** Set Sim Params: Local Run -----------------------------
  
  scen.params = tidyr::expand_grid(
    
    #rep.methods = "gold ; CC ; MICE-std ; Am-std ; MICE-ours ; MICE-ours-pred ; Am-ours",
    rep.methods = "CC", 
    
    model = "OLS",
    coef_of_interest = "A",
    N = c(10000),
    
    # MICE parameters
    # as on cluster
    imp_m = 5,  # CURRENTLY SET LOW
    imp_maxit = 100,
    mice_method = "norm",
    
    # # for quicker sims
    # imp_m = 5,
    # imp_maxit = 5,
    # N = c(100),
    
    #dag_name = c( "1B", "1D", "1G", "1H" ),
    dag_name = "1B"
  )
  
  
  # scen.params = tidyr::expand_grid(
  #   
  #   #rep.methods = "gold ; CC ; MICE-std ; Am-std ; MICE-ours ; MICE-ours-pred ; Am-ours",
  #   rep.methods = "MICE-std", 
  #   
  #   model = "OLS",
  #   #coef_of_interest = c( "(Intercept)", "A"),  # "(Intercept)" or "A"
  #   coef_of_interest = "A",
  #   N = c(1000),
  #   
  #   # MICE parameters
  #   # as on cluster
  #   imp_m = 50,  # also for Amelia
  #   imp_maxit = 100,
  #   mice_method = "norm",
  # 
  #   # # for quicker sims
  #   # imp_m = 5,
  #   # imp_maxit = 5,
  #   # N = c(100),
  #   
  #   #dag_name = c( "1B", "1D", "1G", "1H" ),
  #   dag_name = "1B"
  # )
  # remove combos that aren't implemented
  scen.params = scen.params %>% filter( !(dag_name %in% c("1G", "1H", "1F") &
                                            coef_of_interest == "(Intercept)") )
  
  start.at = 1  # scen name to start at
  scen.params$scen = start.at:( nrow(scen.params) + start.at - 1 )
  
  sim.reps = 1  # reps to run in this iterate
  
  # set the number of local cores
  registerDoParallel(cores=8)
  
  scen = 1
  # data.frame(scen.params %>% filter(scen.name == scen))
  
  # just to avoid errors in doParallel script below
  jobname = "job_1"
  i = 1
}



# RUN SIMULATION ------------------------------


# mimic Sherlock structure
if (run.local == TRUE) ( scens_to_run = scen.params$scen )
if (run.local == FALSE) ( scens_to_run = scen )  # from sbatch


# BEGIN FOR-LOOP to run multiple scens locally
# if running on cluster, scen will just be length 1
for ( scen in scens_to_run ) {
  
  if ( exists("rs") ) rm(rs)
  if ( exists("rep.res") ) rm(rep.res)
  
  # doParallel handles ONE scen at a time
  # system.time is in seconds
  # ~ ********** Beginning of ForEach Loop -----------------------------
  doParallel.seconds = system.time({
    rs = foreach( i = 1:sim.reps, .combine = bind_rows ) %dopar% {
      #for debugging (out file will contain all printed things):
      #for ( i in 1:4 ) {
      
      # only print info for first sim rep for visual clarity
      if ( i == 1 ) cat("\n\n~~~~~~~~~~~~~~~~ BEGIN SIM REP", i, "~~~~~~~~~~~~~~~~")
      
      # results for just this simulation rep
      if ( exists("rep.res") ) suppressWarnings( rm(rep.res) )
      
      # extract simulation params for this scenario (row)
      # exclude the column with the scenario name itself (col) 
      if ( verbose == TRUE ) {
        cat("\n\n scen variable:\n")
        print(scen)
        
        cat("\n\n scen.params again:\n")
        print(scen.params)
      }
      
      p = scen.params[ scen.params$scen == scen, names(scen.params) != "scen"]
      coef_of_interest = p$coef_of_interest
      
      # show beginning of dataset
      if ( i == 1 & verbose == TRUE) cat("\n\nDIM AND HEAD OF P (SINGLE ROW OF SCEN.PARAMS):\n")
      
      # parse methods string
      ( all.methods = unlist( strsplit( x = p$rep.methods,
                                        split = " ; " ) ) )
      
      
      # ~ Simulate Dataset ------------------------------
      
      sim_obj = sim_data(.p = p)
      
      du = sim_obj$du
      di_std = sim_obj$di_std
      di_ours = sim_obj$di_ours
      ( form_string = as.character( sim_obj$form_string ) )
      ( gold_form_string = as.character( sim_obj$gold_form_string ) )
      ( beta = as.numeric(sim_obj$beta) )
      ( exclude_from_imp_model = as.character( sim_obj$exclude_from_imp_model ) )
      
      
      # coefficient of interest for gold-standard model
      if ( coef_of_interest == "(Intercept)" ){
        coef_of_interest_gold = "(Intercept)"
        
      } else if ( coef_of_interest == "A:C"){
        coef_of_interest_gold = "A1:C1"
      } else {
        # *this assumes coef_of_interest is always the factual variable
        #  (e.g., A), so need to add "1" to use the variable
        # that's in gold-standard model
        coef_of_interest_gold = paste(coef_of_interest, "1", sep = "")
      }
      
      # some methods don't make sense for certain combos of DAG and coef_of_interest
      #@this happens when a variable needed for imputation model is also in the target law
      #  later could deal with this by adding the variable back into dataset after imputation
      if ( (p$dag_name == "1D" & coef_of_interest == "A") |
           (p$dag_name == "1B" & coef_of_interest == "A") ) {
        all.methods = all.methods[ !all.methods %in% c("MICE-ours", "Am-ours") ]
      }
      
      
      # ~ Make Imputed Data ------------------------------
      
      
      # ~~ MICE-std ----
      # details of how mice() implements pmm:
      # ?mice.impute.pmm
      if ( "MICE-std" %in% all.methods & !is.null(di_std) ) {
        
        imps_mice_std = mice( di_std,
                              maxit = p$imp_maxit,
                              m = p$imp_m,
                              method = p$mice_method,
                              printFlag = FALSE )
        
        # sanity check
        imp1 = complete(imps_mice_std, 1)
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_mice_std = NULL
        }
        
      } else {
        imps_mice_std = NULL
      }
      
      
      
      
      # test only: fit the imputation model manually to foreshadow collider issues
      #bm: think through this one some more... :)
      # but first collect the stuff running on cluster and run more to fill in table
      if (FALSE){
        # DAG 1D
        if ( p$dag_name == "1D" ) {
          summary( lm(A1 ~ B1 + C1, data = du) )
          lm(A ~ B + C, data = du)
          
          summary( lm(A ~ B + C, data = di_std) )
          # as expected, the coef for B is wrong because of C
          
          # look at first mice imputation
          imp1 = complete(imps_mice_std,1)
          summary( lm(A ~ B + C, data = imp1) )  # still has spurious association
          
          
          # analysis model
          summary( lm(B ~ A, data = imp1) )
          
          # cf. truth
          summary( lm(B1 ~ A1, data = du) )
        }
        
        # DAG 1F
        if ( p$dag_name == "1F" ) {
          summary( lm(A1~B1+D1, data = du) )
          lm(A~B+D, data = du)
          
          # mimic the imputation model
          summary( lm(A~B+C+D, data = di_std) )
          # as expected, the coef for B is wrong because of D
          
          # look at first mice imputation
          imp1 = complete(imps_mice_std,1)
          cor(imp1) # has CORRECT (0) association between A and B; unexpected
          
          # but analysis model is fine!
          # **maybe because it essentially marginalizes over D and the model for
          #  A is collapsible?
          summary( lm(B ~ A, data = imp1) )
        }
        
        # DAG 1J
        if ( p$dag_name == "1J" ) {
          
          cor(du)

          # look at first mice imputation
          imp1 = complete(imps_mice_std,1)
          cor(imp1) # has CORRECT (0) association between C and D; unexpected since they're never observed together
          
   
          summary( lm(B ~ A, data = imp1) )
        } 
        
      }
      
      # ~~ MICE-ours ----
      # MICE by restricting dataset
      if ( "MICE-ours" %in% all.methods & !is.null(di_ours) ) {
        
        imps_mice_ours = mice( di_ours,
                               maxit = p$imp_maxit,
                               m = p$imp_m,
                               method = p$mice_method,
                               printFlag = FALSE )
        
        # sanity check
        imp1 = complete(imps_mice_ours, 1)
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_mice_ours = NULL
        }
        
      } else {
        imps_mice_ours = NULL
      }
      
      
      # ~~ MICE-ours-pred ----
      # MICE by adjusting predictor matrix
      if ( "MICE-ours-pred" %in% all.methods & !is.null(di_std) & !is.null(exclude_from_imp_model) ) {
        
        # modify predictor matrix instead of restricting dataset
        ini = mice(di_std, maxit=0)
        pred = ini$predictorMatrix
        pred[,exclude_from_imp_model] = 0
        
        imps_mice_ours_pred = mice( di_std,
                                    predictorMatrix = pred,
                                    maxit = p$imp_maxit,
                                    m = p$imp_m,
                                    method = p$mice_method,
                                    printFlag = FALSE )
        
        # for later sanity checks
        actual_pred = imps_mice_ours_pred$predictorMatrix
        sums = colSums(actual_pred)  # if >0, means var was used in imp model
        mice_ours_pred_vars_included = names( sums[ sums > 0 ] )
        
        # sanity check
        imp1 = complete(imps_mice_ours_pred, 1)
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_mice_ours_pred = NULL
        }
        
      } else {
        imps_mice_ours_pred = NULL
      }
      
      
      
      
      # ~~ Am-std ----
      if ( "Am-std" %in% all.methods & !is.null(di_std) ) {
        
        imps_am_std = amelia( as.data.frame(di_std),
                              m=p$imp_m,
                              p2s = 0 # don't print output
        )
        
        
        imp1 = imps_am_std$imputations$imp1
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_am_std = NULL
        }
        
        
      } else {
        imps_am_std = NULL
      }
      
      
      if ( "Am-ours" %in% all.methods & !is.null(di_ours) ) {
        imps_am_ours = amelia( as.data.frame(di_ours),
                               m=p$imp_m,
                               p2s = 0 # don't print output
        )
        
        imp1 = imps_am_ours$imputations$imp1
        
        if ( any(is.na(imp1)) ) {
          message("MI left NAs in dataset - what a butt")
          imps_am_ours = NULL
        }
        
      } else {
        imps_am_ours = NULL
      }
      
      
      
      # ~ Initialize Global Vars ------------------------------
      
      # initialize rep.res st run_method_safe and other standalone estimation fns
      #  will correctly recognize it as having 0 rows
      rep.res = data.frame()
      
      
      # ~ Fit Models ------------------------------
      
      
      # ~~ Gold standard: No missing data ----
      if ( "gold" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("gold"),
                                  
                                  method.fn = function(x) fit_regression(form_string = gold_form_string,
                                                                         model = p$model,
                                                                         # *this assumes coef_of_interest is always the factual variable
                                                                         #  (e.g., A), so need to add "1" to use the variable
                                                                         # that's in gold-standard model
                                                                         coef_of_interest = coef_of_interest_gold,
                                                                         miss_method = "gold",
                                                                         du = du,
                                                                         imps = NULL),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      # ~~ Complete-case analysis (naive) ----
      if ( "CC" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("CC"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         # *this assumes coef_of_interest is always the factual variable
                                                                         #  (e.g., A), so need to add "1" to use the variable
                                                                         # that's in gold-standard model
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "CC",
                                                                         du = di_std,
                                                                         imps = NULL),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      # ~~ MICE-std ----
      if ( "MICE-std" %in% all.methods ) {
        rep.res = run_method_safe(method.label = c("MICE-std"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_mice_std),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      # ~~ MICE-ours ----
      if ( "MICE-ours" %in% all.methods & !is.null(imps_mice_ours) ) {
        rep.res = run_method_safe(method.label = c("MICE-ours"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_mice_ours),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      # ~~ MICE-ours-pred ----
      if ( "MICE-ours-pred" %in% all.methods & !is.null(imps_mice_ours_pred) ) {
        rep.res = run_method_safe(method.label = c("MICE-ours-pred"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_mice_ours_pred),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      
      # ~~ Am-std ----
      if ( "Am-std" %in% all.methods & !is.null(imps_am_std) ) {
        rep.res = run_method_safe(method.label = c("Am-std"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_am_std),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      # ~~ Am-ours ----
      if ( "Am-ours" %in% all.methods & !is.null(imps_am_ours) ) {
        rep.res = run_method_safe(method.label = c("Am-ours"),
                                  
                                  method.fn = function(x) fit_regression(form_string = form_string,
                                                                         model = p$model,
                                                                         coef_of_interest = coef_of_interest,
                                                                         miss_method = "MI",
                                                                         du = NULL,
                                                                         imps = imps_am_ours),
                                  .rep.res = rep.res )
      }
      
      if (run.local == TRUE) srr(rep.res)
      
      
      
      
      
      # ~ Add Scen Params and Sanity Checks --------------------------------------
      
      # add in scenario parameters
      # do NOT use rbind here; bind_cols accommodates possibility that some methods' rep.res
      #  have more columns than others
      rep.res = p %>% bind_cols( rep.res )
      
      # these don't come from p because they are from sim_data instead
      rep.res$coef_of_interest = coef_of_interest
      rep.res$beta = beta
      
      rep.res$form_string = form_string
      rep.res$gold_form_string = gold_form_string
      
      # add more info
      rep.res = rep.res %>% add_column( rep.name = i, .before = 1 )
      rep.res = rep.res %>% add_column( scen.name = scen, .before = 1 )
      rep.res = rep.res %>% add_column( job.name = jobname, .before = 1 )
      
      
      
      cat("\ndoParallel flag: Before adding sanity checks to rep.res")
      # could add info about simulated datasets here
      # preface them with "sancheck." to facilitate looking at sanchecks alone
      
      
      # amount of missing data
      # using di_std to avoid having R indicators, etc., in the dataset
      if ( !is.null(di_std) ) {
        rep.res = rep.res %>% add_column( sancheck.prop_complete = sum( complete.cases(di_std) ) / nrow(di_std) )
      }
      
      if ( !is.null(di_ours) ) {
        rep.res = rep.res %>% add_column( sancheck.di_ours.vars = paste( names(di_ours), collapse = " " ) )
      }
      
      if ( exists("mice_ours_pred_vars_included") ){
        if ( !is.null(mice_ours_pred_vars_included) ) {
          rep.res = rep.res %>% add_column( sancheck.mice_ours_pred_vars_included = paste( mice_ours_pred_vars_included, collapse = " " ) )
        }
      }
      
      
      cat("\n\n")
      print(rep.res)
      
      rep.res
    }  ### end foreach loop
    
  } )[3]  # end system.time
  
  
  if ( run.local == TRUE ) {
    # save locally and organize after this scen
    setwd(data.dir)
    fwrite( rs,
            paste( "rs_scen_", scen, ".csv", sep = "" ) )
    
    # also bind into new file
    if ( scen == scens_to_run[1] ) rs_all_scens = rs
    else rs_all_scens = bind_rows(rs_all_scens, rs)
  }
  
}  # END FOR-LOOP to run multiple scens locally



if ( run.local == TRUE ) {
  dim(rs_all_scens)
  
  
  # fill in true beta
  beta_emp = rs_all_scens %>% filter(method == "gold") %>%
    group_by(scen.name) %>%
    summarise(beta = meanNA(bhat)) 
  as.data.frame(beta_emp)
  
  rs_all_scens = rs_all_scens %>% rowwise() %>%
    mutate( beta = ifelse( !is.na(beta),
                           beta,
                           beta_emp$beta[ beta_emp$scen.name == scen.name ] ) )
  
  
  
  t = rs_all_scens %>% group_by(method) %>%
    summarise( 
      reps = n(),
      Bhat = meanNA(bhat),
      BhatBias = meanNA(bhat - beta),
      BhatLo = meanNA(bhat_lo),
      BhatHi = meanNA(bhat_hi),
      BhatRMSE = sqrt( meanNA( (bhat - beta)^2 ) ),
      BhatCover = meanNA( covers(truth = beta,
                                 lo = bhat_lo,
                                 hi = bhat_hi) ) ) %>%
    arrange() %>%
    mutate_if(is.numeric, function(x) round(x,2)) 
  
  as.data.frame(t)
  
  #bm: confounder file-matching is not biased?? why?
  # come back to this one...
  
  
  setwd(data.dir)
  fwrite( rs_all_scens,
          paste( "stitched_local.csv", sep = "" ) )
}


# ~~ End of ForEach Loop ----------------
rs$rep.seconds = doParallel.seconds/sim.reps
rs$rep.seconds[ rs$method != unique(rs$method)[1] ] = NA


expect_equal( as.numeric( sum(rs$rep.seconds, na.rm = TRUE) ),
              as.numeric(doParallel.seconds) )




# ~ QUICK RESULTS SUMMARY ---------------------------------------------------

if ( run.local == TRUE ) {
  rs %>%
    dplyr::select(method, bhat, bhat_width, bhat_covers) %>%
    
    group_by(method) %>%
    summarise_if(is.numeric, function(x) round( meanNA(x), 2 ) )
  
  any(is.na(rs$bhat))
}


# ~ WRITE LONG RESULTS ------------------------------
if ( run.local == FALSE ) {
  setwd("/home/groups/manishad/IWN/long_results")
  fwrite( rs, paste( "long_results", jobname, ".csv", sep="_" ) )
}
