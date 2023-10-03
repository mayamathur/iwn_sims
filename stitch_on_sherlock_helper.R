
stitch = function(){
  path = "/home/groups/manishad/IWN"
  setwd(path)
  source("helper_IWN.R")
  
  
  # PRELIMINARIES ----------------------------------------------
  
  library(data.table)
  library(dplyr)
  library(testthat)
  library(xlsx)
  # s = stitch_files(.results.singles.path = "/home/groups/manishad/IWN/sim_results/long",
  #                  .results.stitched.write.path = "/home/groups/manishad/IWN/sim_results/overall_stitched",
  #                  .name.prefix = "long_results",
  #                  .stitch.file.name="stitched.csv")
  
  .results.singles.path = "/home/groups/manishad/IWN/long_results"
  .results.stitched.write.path = "/home/groups/manishad/IWN/overall_stitched"
  .name.prefix = "long_results"
  .stitch.file.name="stitched.csv"
  
  
  # MAKE STITCHED DATA ----------------------------------------------
  
  # get list of all files in folder
  all.files = list.files(.results.singles.path, full.names=TRUE)
  
  # we only want the ones whose name includes .name.prefix
  keepers = all.files[ grep( .name.prefix, all.files ) ]
  length(keepers)
  
  # grab variable names from first file
  names = names( read.csv(keepers[1] ) )
  
  # read in and rbind the keepers
  tables <- lapply( keepers, function(x) read.csv(x, header= TRUE) )
  
  # sanity check: do all files have the same names?
  # if not, could be because some jobs were killed early so didn't get doParallelTime
  #  variable added at the end
  #  can be verified by looking at out-file for a job without name "doParallelTime"
  allNames = lapply( tables, names )
  # # find out which jobs had wrong number of names
  # lapply( allNames, function(x) all.equal(x, names ) )
  # allNames[[1]][ !allNames[[1]] %in% allNames[[111]] ]
  
  # bind_rows works even if datasets have different names
  #  will fill in NAs
  s <- do.call(bind_rows, tables)
  
  names(s) = names( read.csv(keepers[1], header= TRUE) )
  
  if( is.na(s[1,1]) ) s = s[-1,]  # delete annoying NA row
  # write.csv(s, paste(.results.stitched.write.path, .stitch.file.name, sep="/") )
  
  cat("\n\n nrow(s) =", nrow(s))
  cat("\n nuni(s$scen.name) =", nuni(s$scen.name) )
  
  # debugging help:
  # if there's only 1 unique scen, you might have set
  #  interactive.cluster.run = TRUE in doParallel
  
  
  # ~ Check for Bad Column Names ---------------------------
  
  # not sure why this is needed - has NA columns at end
  names(s)
  any(is.na(names(s)))
  
  if ( any(is.na(names(s))) ) {
    NA.names = which( is.na(names(s) ) )
    s = s[ , -NA.names ]
    
  }
  
  s = s %>% filter(!is.na(scen.name))
  
  table(s$dag_name)
  
  # Quick Look ----------------------------------------------
  
  # sanity check
  table(s$dag_name, s$coef_of_interest)
  
  correct.order = c("gold", "CC", "Am-std", "Am-ours", "MICE-std", "MICE-ours", "MICE-ours-pred")
  s$method = factor(s$method, levels = correct.order)
  
  # fill in beta (where it's NA) using gold-standard
  # s = data.frame( scen.name = c(1,1,1,1,2,2,2,2),
  #                 beta = c( rep(NA,4), rep(1,4)),
  #                 method = rep( c("gold", "gold", "other", "other"), 2),
  #                 bhat = rep( c(2,3,0,1), 2 ) )  # test
  
  beta_emp = s %>% filter(method == "gold") %>%
    group_by(scen.name) %>%
    summarise(beta = meanNA(bhat)) 
  as.data.frame(beta_emp)
  
  s2 = s
  
  s2 = s2 %>% rowwise() %>%
    mutate( beta = ifelse( !is.na(beta),
                           beta,
                           beta_emp$beta[ beta_emp$scen.name == scen.name ] ) )
  
  # sanity check
  as.data.frame( s2 %>% group_by(dag_name, coef_of_interest) %>%
                   summarise(beta[1]) )
  # end of filling in beta
  
  
  t = s2 %>% group_by(dag_name, coef_of_interest, method) %>%
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
}