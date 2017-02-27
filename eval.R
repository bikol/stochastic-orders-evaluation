library(stats)
library(gaoptim)
library(dplyr)
library(futile.logger)
library(foreach)
library(parallel)
library(doParallel)

source("config.R")
source("cost.R")
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
cutPoint = 4

flog.info("Start")

print(system.time({
    results = foreach(
    f1.name = colnames(outcomes.all)[6:(6+cutPoint)],
    .packages=c("dplyr", "futile.logger"),
    .combine=rbind) %dopar% {
    sapply(colnames(outcomes.all), function(f2.name){
        f1 = as.character(outcomes.all[,f1.name])
        f2 = as.character(outcomes.all[,f2.name])
        return(calcDominationDegree(skipNA(f1), skipNA(f2), cost.4)$val)
    })
    }
    # rows are X, and columns are Y,
    # results[[x, y]] contains X>Y dominance degree
    rownames(results) = colnames(outcomes.all)[6:(6+cutPoint)]
}))

flog.info("Finished!")


stopCluster(cl)
