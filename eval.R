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

if (!exists("outcomes.all")) {
    e1 <- new.env()
    load(EVALUATION.OUTPUT.LOCATION, envir = e1)
    outcomes.all <- get("outcomes.all", e1)
    rm(e1)
}

flog.info("Creating local FORK cluster")

cl = makeCluster(THREADS, outfile = "")
registerDoParallel(cl)

flog.info("Start")

print(system.time({
    results = foreach(
        f1.name = colnames(outcomes.all),
        .packages = c("dplyr", "futile.logger"),
        .combine = rbind
    ) %dopar% {
        sapply(colnames(outcomes.all), function(f2.name) {
            f1 = as.character(outcomes.all[, f1.name])
            f2 = as.character(outcomes.all[, f2.name])
            # for test, for full results use 1e6, for 1e3 ~ approx. 3h on 4-core CPU
            return(calcDominationDegree(f1, f2, cost.6, maxit = 1e3)$val)
        })
    }
    # rows are X, and columns are Y,
    # results[[x, y]] contains X>Y dominance degree
    rownames(results) = colnames(outcomes.all)
}))

flog.info("Finished!")


stopCluster(cl)

save(RESULTS.LOCATION)

# fn1.n = "i_(soft_s.min_0.25)_(cen_0.025)"
# fn2.n = "iMean_1_(cen_0.025)_2"
#
# fn1 = as.character(outcomes.all[,fn1.n])
# fn2 = as.character(outcomes.all[,fn2.n])
#
# print(sapply(colnames(outcomes.all), function(f2.name){
#     f1 = as.character(outcomes.all[,"mean_(dec_(owa_1))_cen_0.025"])
#     f2 = as.character(outcomes.all[,f2.name])
#     return(calcDominationDegree(f2, f1, cost.6, maxit=1e4)$val)
# }))
