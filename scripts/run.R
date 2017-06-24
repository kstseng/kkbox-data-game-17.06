###########################' 
##' set working directory #'
###########################' 
setwd("kaggle-kkbox-data-game-1706")

###########################' 
##' create directory .    #'
###########################' 
dir.create("data")         # put data in this folder
dir.create("submission")

###########################' 
##' run scripts           #'
###########################'
##' load data
source("load_data.R")
##' make transition matrix
source("make_transition_matrix.R")
##' make submission by transition matrix
source("make_sub_from_transition_matrix.R")
##' make data in order to build model
source("data_processing.R")
##' train a xgboost classification model
source("xgb_training.R")
##' combine the transition result and xgboost result, and make submission file
source("ensemble.R")