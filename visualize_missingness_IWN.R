


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
  
  dag_name = c( "1B" ),
  N = c(50000)
)


# ~ Simulate Dataset ------------------------------

sim_obj = sim_data(.p = p)

du = sim_obj$du
di_std = sim_obj$di_std
di_ours = sim_obj$di_ours

# long format
l = reshape2::melt(du, id.vars = "RA", measure.vars = names(du)[ names(du) != "RA"] )


# ~ Missingness patterns ------------------------------

y_axis_vars = c("A1")
y_axis_vars = c("A1","B1","U1","U2")

lp = l %>% filter(variable %in% y_axis_vars)

#### Proportion missing by variable
colMeans(is.na(du))

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

# # ggmice: https://amices.org/ggmice/
# ggmice( data = du,
#        aes(A, B) ) + geom_point()
# 


#### A1-B1 relationship within missingness status

p = ggplot( data = du,
            aes(x = A1,
                y = B1,
                color = as.factor(RA) ) ) +
  geom_point(alpha = 0.3) + 
  geom_smooth(method = "lm", fill = NA)+
  theme_bw()
p
# for other structures, could stratify by collider here












