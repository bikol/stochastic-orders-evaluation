library(stats)
library(gaoptim)
library(dplyr)
library(futile.logger)
library(foreach)
library(parallel)
library(doParallel)

source("config.R")
source("data.R")
source("stochastic-orders.R")

if(!exists("outcomes.all")){
    e1 <- new.env()
    load(EVALUATION.OUTPUT.LOCATION, envir=e1)
    outcomes.all <- get("outcomes.all", e1)
    rm(e1)
}

flog.info("Creating local FORK cluster")

cl = makeCluster(THREADS, outfile="")
registerDoParallel(cl)

# workaround for test, allows to speedup computations
cutPoint = 2
perm = sample(1:nrow(outcomes.all), cutPoint)

flog.info("Start")
results = foreach(
    f1.name = colnames(outcomes.all),
    .packages=c("dplyr", "futile.logger"),
    .combine=rbind) %dopar% {
    sapply(colnames(outcomes.all), function(f2.name){
        f1 = as.character(outcomes.all[,f1.name])
        f2 = as.character(outcomes.all[,f2.name])
        return(calcDominationDegree(skipNA(f1)[perm], skipNA(f2)[perm])$val)
    })
    }
# rows are X, and columns are Y,
# results[[x, y]] contains X>Y dominance degree
rownames(results) = colnames(outcomes.all)
