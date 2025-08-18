#
# objective.R
#
# Helper functions for algo_hh.R
#
# Version August 8, 2019

library(data.table)
library(dplyr)

calc_objective <- function(target, baseline) {

    objective <- 0
    for (i in seq_along(target)){
        for (j in seq_along(target[[i]])){
            objective <- objective + sum((baseline[[i]][[j]] - target[[i]][[j]]) ** 2)
        }
    }
    return(objective)
}

norm <- function(x,y) {
    return(max(abs(x-y)))
}

calc_norms <- function(target,baseline) {
    norms<-list()
    for (i in seq_along(target)){
        t_norms<-list()
        for (j in seq_along(target[[i]])){
            t_norms[[j]]<-norm(baseline[[i]][[j]],target[[i]][[j]])
        }
        norms[[i]]<-t_norms
    }
    return(norms)
}

#################################################
#
#  Two functions to convert elapsed times below
#
#################################################

convert_units <- function(x) {
    # input x is in minutes
    if(x>60) x <- x/60   # returns hours
    else if(x<1) x <- x*60  # returns seconds
    return(x)
}

find_units <- function(x) {
    # input is in minutes
    if(x>60) return("hours")
    else if(x<1) return("seconds")
    else return("minutes")
}
