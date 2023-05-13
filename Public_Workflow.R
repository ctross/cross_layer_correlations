################################################ Packages
library(openssl)
library(grid)
library(gridBase)
library(gridExtra)
library(ggplot2)
library(kinship2)
library(igraph)
library(reshape2)
library(plyr)
library(geosphere)
library(GGally)
library(network)
library(sna)
library(rethinking)
library(colorspace)
library(rstan)
library(rethinking)
library(ggraph)
library(xtable)

################################################ Set WD and load data
 path = "C:\\Users\..."
 setwd(path)
 load("ColombianDataPublicationVersion.RData")

######################### This code creates descriptive
 source("Code/Network_Summaries.R")

 source("Code/Network_Plots_BS.R")
 source("Code/Network_Plots_SC.R")

 source("Code/Hive_Plots_SC.R")
 source("Code/Hive_Plots_BS.R")

######################### This code runs the stan models for Coastal Site
 iter = 2500
 warmup = 1000
 chains = 1
 refresh = 1
 seed = 8765309

 sm = stan_model(file="./Code/SRM_5_prior_bandage_no_comments.stan")

 res_bs = sampling(object = sm, data = model_dat_bs, chains = chains, refresh=refresh, iter = iter, warmup = warmup, seed = seed)

 ######################### This code runs the stan models for Inland Site
 iter = 1600
 warmup = 1000
 chains = 1
 refresh = 1
 seed = 8765309

 sm = stan_model(file="./Code/SRM_5_prior_bandage_no_comments.stan")

 res_sc = sampling(object = sm, data = model_dat_sc, chains = chains, refresh=refresh, iter = iter, warmup = warmup, seed = seed)

 ######################### Now plots
 source("Code/DyadicPlots_BS.R")
 source("Code/DyadicPlots_SC.R")
 source("Code/Merged_DR_Plots.R")

 source("Code/GeneralizedPlots_BS.R")
 source("Code/GeneralizedPlots_SC.R")
 source("Code/Merged_GR_Plots.R")

 ####################### Finally, paper estimates
 source("Code/Make_R_LaTeX_Variables.R")
 

