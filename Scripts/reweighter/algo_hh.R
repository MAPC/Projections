#
# algo_hh.R
#
# base randomized descent algorithm with functionality for changing HH weights as well as Person weights
# optimized
#
# Version August 14, 2019
#

# import libraries
library(dplyr)
library(data.table)
# increase the number of significant digits
options(digits=20)
# library(bit64)
# import custom functions
source("objective.R")

# randomized descent algoritm
# INPUTS:-
# inp: data table of input (POP2016.csv)
# cond: data table of savefile.RData
# num_iter: number of iterations the algorithm should run
# u_factor: factor by which the weights are updated
# wflag: flag whether to write current version of target files each iteration
# max_norm: maximum deviation from target stopping condition
# iter_file: filename for iteration statistics
# log_file: filename for the log file (should exist)
# OUTPUTS: set of updated weights 
random_descent_hh <- function(inp, cond, num_iter, u_factor, wflag, 
                              max_norm=0,iter_file="iter_stats.csv",
                              log_file="log.txt",
                              random=FALSE,seed=-1) {
    
    # some initializations
    version_of_least_square_weights <- "algo_hh.R Version August 14, 2019"
    
    tbegin <- Sys.time()
    print(version_of_least_square_weights)
    write(date(),file=log_file,append=TRUE)
    write(version_of_least_square_weights,file=log_file,append=TRUE)
    
    
    try_change <- function(r,nblocks,child,dwfactor,d_OF) {
        # r is the record number of the household
        # nblocks is the number of blocks of tables
        # child is the vector of record numbers of household members
        # dwfactor is the signed cumulative factor on the original weight
        #                           (+ for an increase, - for a decrease)
        # d_OF is the cumulative change in the objective function for this household.
        #

        w_delta_h <- dwfactor * weights[["WGTP"]][r]
        
        for (b in seq(nblocks)){
            # skip block if there are no tables
            if (n_tables[b] == 0) {next}
            # iterate through each table
            for (t in seq(n_tables[[b]])){
                # iterate through each member of the HH
                for (c in child){
                    id <- ids[[b]][[t]][c]
                    # skip if it doesn't belong to any of the cells
                    if (id == 0) {next}
                    # get the updated PWGTP weight differentials
                    w_delta_p <- dwfactor * weights[["PWGTP"]][c]
                    # get the baseline and target value
                    target_val <- targets[[b]][[t]][id]
                    base_old <- baselines[[b]][[t]][id]
                    
                    # get the modified objective function value
                    
                    if (cond[[b]][["special_cond_var"]] == "none"){
                        d_OF <- d_OF - 2*w_delta_p*(target_val - base_old) + w_delta_p^2
                        baselines[[b]][[t]][id] <<- baselines[[b]][[t]][id] + w_delta_p
                    }
                    else if (cond[[b]][["special_cond_var"]] == "SPORDER"){
                        d_OF <- d_OF - 2*w_delta_h*(target_val - base_old) + w_delta_h^2
                        baselines[[b]][[t]][id] <<- baselines[[b]][[t]][id] + w_delta_h
                        break
                    }
                }
            }
        }
        return(d_OF)
    }
    
    change_wfactor <- function(r,nblocks,child,dwfactor) {
        new_wfactor <- weights[["WFACTOR"]][[r]] + dwfactor
        for(c in child) {weights[["WFACTOR"]][[c]] <<- new_wfactor}
    }
    
    make_change <- function(r,nblocks,child,dwfactor) {
        
        w_delta_h <- dwfactor * weights[[ "WGTP"]][r]
        
        for (b in seq(nblocks)){
            # skip block if there are no tables
            if (n_tables[b] == 0) {next}
            # iterate through each table
            for (t in seq(n_tables[[b]])){
                # iterate through each member of the HH
                for (c in child){
                    id <- ids[[b]][[t]][c]
                    # skip if it doesn't belong to any of the cells
                    if (id == 0) {next}
                    # get the updated PWGTP weight differentials
                    w_delta_p <- dwfactor * weights[["PWGTP"]][c]
                    
                    # get the modified objective function value
                    
                    if (cond[[b]][["special_cond_var"]] == "none"){
                        baselines[[b]][[t]][id] <<- baselines[[b]][[t]][id] + w_delta_p
                    }
                    else if (cond[[b]][["special_cond_var"]] == "SPORDER"){
                        baselines[[b]][[t]][id] <<- baselines[[b]][[t]][id] + w_delta_h
                        break
                    }
                }
            }
        }
    }
    
    
    # get the number of tables and blocks
    n_blocks <- length(cond) - 2
    n_tables <- list()
    # get input file name
    inp_file <- cond[["file_name"]]
    # get the target variable
    target_var <- list()
    
    # form the weights list of vectors of weighting factors, person and household weights
    weights <- list()
    weights[["WFACTOR"]]<-vector(mode="numeric",length=dim(inp)[1])
    weights[["WFACTOR"]][]<-1
    weights[["PWGTP"]] <- as.numeric(inp[["PWGTP"]])
    weights[["WGTP"]] <- as.numeric(inp[["WGTP"]])
    
    # get information for each block
    for (b in seq(n_blocks)){
        # get the number of tables
        n_tables[[b]] <- cond[[b]][["num_tables"]]
        # get the target variable
        target_var[[b]] <- cond[[b]][["target_var"]]
    }
    # init the ids, baselines and targets
    ids <- list()
    baselines <- list()
    targets <- list()
    table_names <- c()

    # iterate through all the tables
    for (b in seq(n_blocks)){
        t_baselines <- list()
        t_targets <- list()
        t_ids <- list()
        for (t in seq(n_tables[[b]])){
            table <- cond[[b]][[t]]
            data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
            table_names <- c(table_names,table[[1]])
            # get the ids, baseline and target values
            t_baselines[[t]] <- as.double(data[["BASELINE"]]) 
            t_targets[[t]] <- as.double(data[["TARGET"]]) 
            t_ids[[t]] <- cond[[b]][[t]][[7]]
            # add a new column containing the intermediate weights 
            data <- mutate(data, INTER=t_baselines[[t]])
            data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
        }
        # save the baselines, targets and ids
        baselines[[b]] <- t_baselines
        targets[[b]] <- t_targets
        ids[[b]] <- t_ids
    }
    # initialize iteration stats
    iter_stats <- data.frame(matrix(ncol=7+length(table_names),nrow=0))
    names(iter_stats) <- c("Iteration","Objective Function","num_increase","num_decrease","num_nochange","num_lower_increase","num_raise_decrease",table_names)
    # calculate the initial objective function and the initial norms
    of_val <- calc_objective(targets, baselines)
    print(paste("init ofval: ", of_val))
    norms<-calc_norms(targets,baselines)
    iter_stats[1,] <- c(0,of_val,0,0,0,0,0,unlist(norms))
    
    # initialize list of households (household heads)
    r_int <- which(inp$SPORDER == 1)
    if(random) {
        if(seed==-1) set.seed(Sys.time())
        else set.seed(seed)
    }
    
    # initialize iteration condition, iteration number
    
    iter_cond <- TRUE
    iter <- 0

    # run the algorithm for num_iter steps
    while (iter_cond){
        # initialize stuff for this iteration
        iter <- iter + 1
        n_increase <- 0
        n_decrease <- 0
        n_nochange <- 0
        n_lower_increase <- 0
        n_raise_decrease <- 0
        start_time <- Sys.time()
        # get a random integer ordering such that main person of the HH is selected
        # major speed-up (from 25 s/iter to 3 s/iter)
        
        # iterate through the rows
        if(random) r_int <- sample(r_int,length(r_int),replace=FALSE)

        for (r in r_int){
            # store the objective function value
            of_new <- of_val
            # get the list of people in the HH
            child <- cond[["children"]][[r]]
            # get the updated WGTP weight differential
            dwfactor <- u_factor * weights[["WFACTOR"]][r]
            
            increase<-FALSE
            decrease<-FALSE
            lower_increase<-FALSE
            raise_decrease<-FALSE
            wfactor_type_1 <- TRUE
            
            if(weights[["WFACTOR"]][[r]] >= 1) {
                d_OF <- try_change(r,n_blocks,child,dwfactor,0)
                if(d_OF<0) {
                    increase<-TRUE
                    n_increase <- n_increase + 1
                }
                else {
                    d_OF <- try_change(r,n_blocks,child,-2*dwfactor,d_OF)
                    if(d_OF<0) {
                        decrease<-TRUE
                        lower_increase<-TRUE
                        n_decrease <- n_decrease + 1
                        n_lower_increase <- n_lower_increase + 1
                    }
                }
            }
            else {  # weight factor is less than 1
                wfactor_type_1 <- FALSE
                d_OF <- try_change(r,n_blocks,child,-dwfactor,0)
                if(d_OF<0) {
                    decrease<-TRUE
                    n_decrease <- n_decrease + 1
                }
                else {
                    d_OF <- try_change(r,n_blocks,child,2*dwfactor,d_OF)
                    if(d_OF<0) {
                        increase<-TRUE
                        raise_decrease<-TRUE
                        n_increase <- n_increase + 1
                        n_raise_decrease <- n_raise_decrease + 1
                    }
                }
            }
            
            if(increase)       change_wfactor(r,n_blocks,child, dwfactor)
            else if (decrease) change_wfactor(r,n_blocks,child,-dwfactor)
            else {
                if(wfactor_type_1) make_change(r,n_blocks,child,dwfactor)
                else               make_change(r,n_blocks,child,-dwfactor)
                n_nochange <- n_nochange + 1
            }
        }
            
            
        # write the baselines to file
        if (wflag){
            for (b in seq(n_blocks)){
                if (n_tables[[b]] == 0) {next}
                for (t in seq(n_tables[[b]])){
                    table <- cond[[b]][[t]]
                    data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
                    data <- mutate(data, INTER=baselines[[b]][[t]])
                    data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
                }
            }
        }

        # calcuate objective function
        of_val <- calc_objective(targets,baselines)
        norms <- calc_norms(targets,baselines)
        
        # update iter_stats
        iter_stats[iter+1,] <- c(iter,of_val,n_increase,n_decrease,n_nochange,n_lower_increase,n_raise_decrease,unlist(norms))
        
        
        # report the iteration time
        end_time <- Sys.time()
        time_taken <- difftime(end_time,start_time,units="mins")

        print(paste("Iter No.:", iter, "| Time:", convert_units(time_taken), 
                    find_units(time_taken), "| OFVal:", of_val,
                    "| Max Norm:", max(unlist(norms)) ))
                
        write(paste("Iter No.:", iter, "| Time:", convert_units(time_taken), 
                    find_units(time_taken), "| OFVal:", of_val,
                    "| Max Norm:", max(unlist(norms)) ), file=log_file,append=TRUE)
        # check for stopping conditions
        
        if(max(unlist(norms)) <= max_norm) {
            print("Maximum deviation from target stopping condition achieved.")
            write("Maximum deviation from target stopping condition achieved.",file=log_file,append=TRUE)
            iter_cond <- FALSE
        }
        else if(n_increase == 0 & n_decrease == 0) {
            print("No change in weights stopping condition met.")
            write("No change in weights stopping condition met.",file=log_file,append=TRUE)
            iter_cond <- FALSE
        }
        else if(iter == num_iter) {
            print("Maximum number of iterations reached.")
            write("Maximum number of iterations reached.",file=log_file,append=TRUE)
            iter_cond <- FALSE
        }
    }
    
    # write iteration stats
    write.csv(iter_stats,file=iter_file,row.names = FALSE)
    
    # write tables
    for (b in seq(n_blocks)){
        if (n_tables[[b]] == 0) {next}
        for (t in seq(n_tables[[b]])){
            table <- cond[[b]][[t]]
            data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
            data <- mutate(data, INTER=baselines[[b]][[t]])
            data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
        }
    }

    # return the updated weights
    weights <- data.table(
        WFACTOR = weights[["WFACTOR"]],
        new_PWGTP = weights[["PWGTP"]],
        new_WGTP = weights[["WGTP"]]
    )
    weights <- mutate(weights, SERIALNO=inp$SERIALNO)
    weights <- mutate(weights, SPORDER=inp$SPORDER)
    weights$new_PWGTP <- weights$WFACTOR*weights$new_PWGTP
    weights$new_WGTP  <- weights$WFACTOR*weights$new_WGTP
    
    # print stats on weight factors
    print("Quartile statistics for weighting factors:")
    print(summary(weights$WFACTOR))
    print("Standard deviation of weighting factors:")
    print(sd(weights$WFACTOR))
    
    write("Quartile statistics for weighting factors: Min, Q1, Median, Mean, Q3, Max",file=log_file,append=TRUE)
    write(summary(weights$WFACTOR),file=log_file,append=TRUE)
    write("Standard deviation of weighting factors:",file=log_file,append=TRUE)
    write(sd(weights$WFACTOR),file=log_file,append=TRUE)
    
    total_elapsed <- difftime(Sys.time(),tbegin,units="mins")
    write(paste("Total elapsed time:",convert_units(total_elapsed),find_units(total_elapsed)),file=log_file,append=TRUE)
    
    return(weights)
}
