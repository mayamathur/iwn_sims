


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
                "ggplot2",
                "ggmice",
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



# PICK DAG TO GENERATE -----------------------------------------

# most parameters don't matter, so set to NULL
p = tidyr::expand_grid(
  
  rep.methods = NA,
  
  model = "OLS",
  coef_of_interest = "A",
  
  imp_m = NA,
  imp_maxit = NA,
  mice_method = NA,
  
  dag_name = c( "1I" ),
  N = c(5000)
)


# ~ Simulate Dataset ------------------------------

sim_obj = sim_data(.p = p)

du = sim_obj$du
di_std = sim_obj$di_std
di_ours = sim_obj$di_ours

# long format
l = reshape2::melt(du, id.vars = "RA", measure.vars = names(du)[ names(du) != "RA"] )


# temp only: complete cases
p = ggplot( data = du,
            aes(x = A1,
                y = B1,
                color = as.factor(RA == 1 & RB == 1) ) ) +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", fill = NA)+
  theme_bw()
p

#### Correlation of counterfactuals
cor( du %>% select(A1, B1, U1, U2, U3, C1, D1, RA))
cor( du %>% select(A1, B1, C1, RA, RB))


# ~ Missingness patterns ------------------------------

#### Proportion missing by variable
colMeans(is.na(du))



# ~~ DAG 1B  -------------------------------------------------
if (p$dag_name == "1B") {
  y_axis_vars = c("A1")
  y_axis_vars = c("A1","B1","U1","U2")
  
  lp = l %>% filter(variable %in% y_axis_vars)
  
  #### Boxplots of each counterfactual variable by missingness status
  p = ggplot( data = lp,
              aes(x = variable,
                  y = value,
                  fill = as.factor(RA)
              ) ) +
    #geom_violin() +
    geom_boxplot()+
    theme_bw()
  p
  
  #### A1-B1 relationship within missingness status
  # if parallel lines => CC unbiased
  p = ggplot( data = du,
              aes(x = A1,
                  y = B1,
                  color = as.factor(RA) ) ) +
    geom_point(alpha = 0.3) + 
    geom_smooth(method = "lm", fill = NA)+
    theme_bw()
  p
}


# ~~ DAG 1G  -------------------------------------------------

if (p$dag_name == "1G") {
  
  y_axis_vars = c("A1")
  y_axis_vars = c("A1","B1","U1","U2")
  
  lp = l %>% filter(variable %in% y_axis_vars)
  
  #### Boxplots of each counterfactual variable by missingness status
  p = ggplot( data = lp,
              aes(x = variable,
                  y = value,
                  fill = as.factor(RA)
              ) ) +
    #geom_violin() +
    geom_boxplot()+
    theme_bw()
  p
  
  #### A1-B1 relationship within missingness status
  # if parallel lines => CC unbiased
  p = ggplot( data = du,
              aes(x = A1,
                  y = B1,
                  color = as.factor(RA) ) ) +
    geom_point(alpha = 0.3) + 
    geom_smooth(method = "lm", fill = NA)+
    theme_bw()
  p
}








