# 
#  Manage_Scenario.R
#

setwd(inpath)

version <- 'September 17, 2024'

# Log text file that holds messages for this scenario
log_file <- paste0(outpath,"logfile_",scen,"_",mid,"_",yr,".txt")

#  Input PUMS extract csv file that was used by read_config_hh.R
pums_file <- pums

# Output csv file for the weights created by algo_hh.R
weights_file <- paste0(outpath,"weights_final_",scen,"_",mid,"_",yr,".csv")

# Output csv file for iteration statistics
iter_file <- paste0(outpath,"iter_stats.csv")

# Maximum number of iterations
num_iters <- 1000

# Update factor for weights (Suggested default=.01)
update_factor <- .01

# Maximum deviation from targets (Suggested default=.5)
max_norm <- .5

# Indicator for randomization of household order (Suggested default=TRUE)
random <- TRUE

# If random=TRUE, seed for random order of households.
# seed=-1 -> Use the system clock to set the seed; or
# supplying a number for the seed will allow reproduceability
# of the random order.
seed <- -1
##get rid of scientific notation
options(scipen=999)
#######################################################
#
#    END of user-supplied paramaters
#
#######################################################

# Write parameters to log file.

write(paste("Manage_Scenario.R version:", version), log_file)
write(Sys.time(),file=log_file,append=TRUE)
write(paste("Log file:",log_file),file=log_file,append=TRUE)
write(paste("PUMS file:",pums_file),file=log_file,append=TRUE)
write(paste("Weights file:",weights_file),file=log_file,append=TRUE)
write(paste("Iteration statistics file:",iter_file),file=log_file,append=TRUE)
write(paste("Maximum number of iterations:",num_iters),file=log_file,append=TRUE)
write(paste("Update factor:",update_factor),file=log_file,append=TRUE)
write(paste("Manimum deviation from targets:",max_norm),file=log_file,append=TRUE)
if(random) {
  write("Household order is randomized",file=log_file,append=TRUE)
  if(seed==-1) write("The system clock is used to set the seed.",file=log_file,append=TRUE)
  else write(paste("The seed for randomization is",seed),file=log_file,append=TRUE)
}

# Input save_list file that was created by read_config_hh.R
# This manages the least squares algorithm in algo_hh.R
# Note: savefilehh.RData is the name given by read_config_hh.R,
#       so typically this does not need to be changed.
save_list_file <- paste0(inpath,"savefilehh_",mid,".RData")

# import libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

# source custom functions
# source("../Scripts/algo_hh.R")

# read the input file
# inp <- data.table::fread(file=pums_file)
inp <- pums_file
# read the savefile RData object
conditions <- read_rds(save_list_file)
# set the write flag. No reason to set this to TRUE.
write_flag <- FALSE

# run the algorithm
weights <- random_descent_hh(inp, conditions, num_iters, update_factor,write_flag,
                             max_norm,iter_file,log_file,
                             random,seed)
# write the weights to file
data.table::fwrite(weights, weights_file)
