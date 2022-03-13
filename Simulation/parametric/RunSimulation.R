# Simulation, parametric multiple comparisons

rm(list = ls())

# load the packages
pacman::p_load(multcomp, purrr, tidyverse, parallel, ggplot2, abind)

# setwd("./Simulation")
source("00_functions/Functions.R")

# Heteroscedastic cases ---------------------------------------------------

# Case 1: balanced and unbalanced, heteroscedastic vars, power = 0.7
RunSim(case             = 1,
       seed             = 2,
       ngroups          = c(3,4,5),
       heteroscedastic  = TRUE,
       CC               = c("Dunnett", "Tukey"),
       t                = "all",
       N                = NULL,
       power            = 0.7)

# Case 2: balanced and unbalanced, heteroscedastic vars, power = 0.8
RunSim(case             = 2,
       seed             = 7,
       ngroups          = c(3,4,5),
       heteroscedastic  = TRUE,
       CC               = c("Dunnett", "Tukey"),
       t                = "all",
       N                = NULL,
       power            = 0.8)

# Case 3: balanced and unbalanced, heteroscedastic vars, power = 0.9
RunSim(case             = 3,
       seed             = 10,
       ngroups          = c(3,4,5),
       heteroscedastic  = TRUE,
       CC               = c("Dunnett", "Tukey"),
       t                = "all",
       N                = NULL,
       power            = 0.9)



# Homogeneous cases -------------------------------------------------------

# Case 1: balanced and unbalanced, homogeneous vars, power = 0.7
RunSim(case             = 1,
       seed             = 21,
       ngroups          = c(3,4,5),
       heteroscedastic  = FALSE,
       CC               = c("Dunnett", "Tukey"),
       t                = "all",
       N                = NULL,
       power            = 0.7)

# Case 2: balanced and unbalanced, homogeneous vars, power = 0.8
RunSim(case             = 2,
       seed             = 23,
       ngroups          = c(3,4,5),
       heteroscedastic  = FALSE,
       CC               = c("Dunnett", "Tukey"),
       t                = "all",
       N                = NULL,
       power            = 0.8)

# Case 3: balanced and unbalanced, homogeneous vars, power = 0.9
RunSim(case             = 3,
       seed             = 4,
       ngroups          = c(3,4,5),
       heteroscedastic  = FALSE,
       CC               = c("Dunnett", "Tukey"),
       t                = "all",
       N                = NULL,
       power            = 0.9)






