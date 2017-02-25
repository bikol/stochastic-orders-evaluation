library(stats)
library(gaoptim)
library(dplyr)

# initial problem


## Cost function definition
cost.func.min = 0
cost.func.max = 5
cost.func.tp = function(cost) {
    return(pmax(0,-0.4 * cost + 1))
}
cost.func.tp.default = 0
cost.func.tn = function(cost) {
    return(pmax(0,-1 * cost + 1))
}
cost.func.tn.default = 0
cost.func.fp = function(cost) {
    return(if_else(cost <= 2.5, pmax(0, 0.4 * cost), pmax(0,-0.4 * cost + 2)))
}
cost.func.fp.default = 2.5
cost.func.fn = function(cost) {
    return(pmax(0, 0.2 * cost))
}
cost.func.fn.default = 5
##

getInitialPoint = function(f1, f2) {
    default1 = apply(cbind(f1, f2), 1, function(row) {
        return(switch(
            row[[1]],
            "TP" = cost.func.tp.default,
            "TN" = cost.func.tn.default,
            "FN" = cost.func.fn.default,
            "FP" = cost.func.fp.default
        ))
    })
    default2 = apply(cbind(f1, f2), 1, function(row) {
        return(switch(
            row[[2]],
            "TP" = cost.func.tp.default,
            "TN" = cost.func.tn.default,
            "FN" = cost.func.fn.default,
            "FP" = cost.func.fp.default
        ))
    })
    return(list(x = default1, y = default2))
}

getOptimVarCount = function(f1, f2) {
    fillIndex = 0
    for (i in 1:length(f1)) {
        if (!(f1[[i]] == "TP" || f1[[i]] == "TN")) {
            fillIndex = fillIndex + 1
        }
    }
    for (i in 1:length(f1)) {
        if (f1[[i]] != f2[[i]] && f2[[i]]!="FN") {
            fillIndex = fillIndex + 1
        }
    }
    return(fillIndex)
}

gen.getFilled = function(f1, f2) {
    force(f1)
    force(f2)
    copyFromFill = c()
    
    for (i in 1:length(f1)) {
        if (f1[[i]] == "TP" || f1[[i]] == "TN") {
            copyFromFill = append(copyFromFill, switch(f1[[i]],
                                                       "TN" = cost.func.tn.default,
                                                       "TP" = cost.func.tp.default))
            
        } else{
            copyFromFill = append(copyFromFill, NA)
        }
    }
    copyFromX = c()
    
    for (i in 1:length(f2)) {
        if (f1[[i]] == f2[[i]]) {
            copyFromFill = append(copyFromFill, 666)
            copyFromX = append(copyFromX, NA)
        } else if(f2[[i]]=="FN") {
            copyFromFill = append(copyFromFill, cost.func.fn.default)
            copyFromX = append(copyFromX, 666)
        } else {
            copyFromFill = append(copyFromFill, NA)
            copyFromX = append(copyFromX, 666)
        }
    }
    return(function(fill) {
        force(fill)
        newCopyFromFill = c(copyFromFill)
        newCopyFromFill[is.na(newCopyFromFill)] = fill
        
        x = newCopyFromFill[1:(length(newCopyFromFill) / 2)]
        y = newCopyFromFill[((length(newCopyFromFill) / 2) + 1):length(newCopyFromFill)]
        
        y[is.na(copyFromX)] = x[is.na(copyFromX)]
        return(list(x = x, y = y))
    })
}


stochastic.dominance.func = function(x, y, f1, f2) {
    # check whether x dominates y in 1st stachastic dominance
    
    # x and y are reward based
    unq = c(-cost.func.max - 1, unique(c(x, y)),-cost.func.min + 1)
    
    for (c in unq) {
        if (sum(x > c) / length(x) < sum(y > c) / length(y)) {
            return(0)
        }
    }
    
    # otherwise x>y so we can calculate possibility degree
    p.x = min(
        recode(
            f1,
            TP = cost.func.tp(-x),
            TN = cost.func.tn(-x),
            FN = cost.func.fn(-x),
            FP = cost.func.fp(-x)
        )
    )
    
    # efficacy optimisation
    if (p.x <= 0) {
        return(0)
    }
    p.y = min(
        recode(
            f2,
            TP = cost.func.tp(-y),
            TN = cost.func.tn(-y),
            FN = cost.func.fn(-y),
            FP = cost.func.fp(-y)
        )
    )
    return(min(p.x, p.y))
}

bruteOpt = function(f1, f2, fill, iters = 100000) {
    count = getOptimVarCount(f1, f2)
    return(max(replicate(iters, function() {
        x = sample(c(0, -0.7143, -0.8333, -1, -1.25, -2, -2.5, -3.3333, -5),
                   count,
                   replace = T)
        filled = fill(x)
        return(stochastic.dominance.func(filled[[1]], filled[[2]], f1, f2))
    }())))
}

gaOpt = function(f1,
                 f2,
                 popSize = 25000,
                 mutRate = 0.05) {
    count = getOptimVarCount(f1, f2)
    print(paste0("Variables to optimise: ", count))
    fill = gen.getFilled(f1, f2)
    return(GAReal(
        function(x) {
            filled = fill(x)
            return(stochastic.dominance.func(filled[[1]], filled[[2]], f1, f2))
        },
        rep(-5, count),
        rep(0, count),
        popSize = popSize,
        mutRate = mutRate
    ))
}

ff1 = c("TN", "TN", "TN", "TN", "FP", "TP", "TP", "TP", "TP", "FN")
ff2 = c("TN", "FP", "FP", "FP", "FP", "TP", "TP", "TP", "TP", "TP")
ff3 = c("TN", "TN", "TN", "TN", "FP", "TP", "TP", "TP", "FN", "FN")

LR1 = c("FP", "TN", "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TN",   "TP", "FN", "TP", "TP", "TP", "TP", "TP", "TP", "N0", "TN", "TN",  "TN", "TN", "TN", "TN", "TP", "TP", "FP", "FP", "FP", "N0", "FP",   "FP", "TN", "FP", "FP", "FP", "N0", "N0", "TP", "TN", "TN", "TN",  "N0", "TP", "FP", "TN", "TP", "N0", "TN", "TN", "N0", "FP", "TP",   "TN", "FP", "TN", "FP", "N0", "N0", "FP", "N0", "TN", "N0", "TN",  "FP", "TN", "TN", "N0", "N0", "N0", "TN", "FP", "TP", "TN", "TP",   "TP", "N0", "TP", "FP", "N0", "TP", "N0", "TP", "TP", "N0", "TP",   "N0", "N0", "TP", "TP", "N1", "N0", "N0", "N0", "N0", "TN", "N0",   "N1", "N0", "N1", "N0", "N0", "FP", "N0", "N0", "TP", "N0", "N0",   "TP", "FP", "TP", "N0", "N0", "N1", "FP", "TN", "N0", "N0", "N1",   "N0", "N0", "N0", "TP", "TP", "N0", "N0", "N0", "N0", "TP", "N0",   "FP", "N0", "FP", "N0", "TP", "N0", "FP", "N0", "N0", "N0", "N0",   "FP", "N1", "N0", "TP", "TP", "TP", "N0", "N0", "TP", "N0", "N0",   "FP", "N0", "TP", "N0", "TP", "TP", "TP", "N0", "N0", "N0", "TP",   "N0", "TP", "FP", "TP", "N0", "TP", "N0", "TP", "TP", "FN", "N0")
LR2 = c("TN", "TN", "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TN",   "TP", "FN", "TP", "TP", "TP", "TP", "TP", "TP", "FP", "N0", "N0", "N0", "TN", "TN", "TN", "TP", "TP", "FP", "FP", "FP", "FP", "FP",   "FP", "N0", "FP", "FP", "FP", "N0", "N0", "TP", "TN", "TN", "TN",   "N0", "TP", "FP", "TN", "TP", "TN", "N0", "TN", "N0", "FP", "TP",   "TN", "FP", "TN", "FP", "FP", "FP", "FP", "N0", "N0", "N0", "TN",  "FP", "TN", "TN", "N0", "N0", "N0", "TN", "N0", "TP", "TN", "TP",   "TP", "TN", "TP", "FP", "N0", "TP", "N0", "TP", "TP", "N0", "TP",   "N0", "N0", "TP", "TP", "N1", "N0", "N0", "N0", "N0", "TN", "N0",   "N1", "N0", "N1", "N0", "N0", "FP", "N0", "N0", "TP", "N0", "N0",   "TP", "TN", "TP", "N0", "N0", "TP", "FP", "TN", "N0", "N0", "N1",   "N0", "N0", "N0", "TP", "TP", "N0", "N0", "N0", "TN", "TP", "N0",   "FP", "N0", "FP", "N0", "TP", "N0", "FP", "N0", "N0", "N0", "N0",   "FP", "N1", "N0", "TP", "TP", "TP", "N0", "N0", "TP", "N0", "N0",   "FP", "N0", "TP", "N0", "TP", "TP", "TP", "N0", "N0", "N0", "TP",   "N0", "TP", "FP", "N1", "N0", "TP", "N0", "TP", "TP", "FN", "N0")
SM = c("TN", "TN", "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TN",        "TP", "FN", "TP", "TP", "TP", "TP", "TP", "TP", "TN", "TN", "TN",        "TN", "TN", "TN", "TN", "TP", "TP", "FP", "TN", "TN", "TN", "FP",        "TN", "TN", "FP", "FP", "FP", "TN", "TN", "TP", "TN", "TN", "TN",        "TN", "TP", "TN", "TN", "FN", "TN", "FP", "TN", "TN", "FP", "TP",        "TN", "FP", "TN", "TN", "TN", "FP", "FP", "TN", "TN", "TN", "TN",        "FP", "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TP", "TN", "TP",        "TP", "TN", "TP", "FP", "TN", "TP", "FP", "TP", "TP", "TN", "TP",        "FP", "TN", "TP", "TP", "N1", "TN", "TN", "TN", "FP", "TN", "TN",        "TP", "TN", "N1", "N0", "TN", "FP", "N0", "TN", "TP", "TN", "TN",        "TP", "FP", "TP", "TN", "FP", "N1", "FP", "FP", "TN", "TN", "N1",        "N0", "TN", "TN", "TP", "TP", "TN", "TN", "TN", "TN", "TP", "TN",        "FP", "TN", "FP", "FP", "N1", "TN", "FP", "TN", "N0", "N0", "TN",        "FP", "FN", "FP", "TP", "TP", "TP", "TN", "TN", "TP", "N0", "TN",        "TN", "FP", "TP", "TN", "TP", "TP", "TP", "TN", "FP", "N0", "TP",        "TN", "TP", "TN", "N1", "TN", "FN", "TN", "TP", "TP", "TP", "TN")
Tim = c("TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN",         "TP", "FN", "TP", "TP", "TP", "TP", "TP", "TP", "TN", "TN", "TN",         "TN", "TN", "TN", "TN", "N1", "TP", "TN", "TN", "TN", "TN", "TN",         "TN", "TN", "TN", "N0", "FP", "TN", "TN", "TP", "TN", "TN", "TN",         "TN", "N1", "TN", "TN", "TP", "TN", "TN", "N0", "TN", "TN", "TP",         "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "N0",         "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "N0", "TP",         "FN", "TN", "N1", "TN", "N0", "TP", "N0", "TP", "N1", "TN", "TP",         "TN", "TN", "N1", "FN", "N1", "TN", "N0", "TN", "N0", "N0", "N0",         "N1", "TN", "FN", "N0", "TN", "N0", "N0", "TN", "N1", "N0", "TN",         "TP", "N0", "N1", "TN", "N0", "TP", "N0", "N0", "N0", "N0", "N1",         "FP", "N0", "TN", "FN", "TP", "TN", "TN", "TN", "TN", "N1", "TN",         "N0", "TN", "TN", "TN", "N1", "TN", "N0", "N0", "N0", "TN", "N0",         "N0", "N1", "N0", "FN", "N1", "N1", "TN", "TN", "TP", "TN", "TN",         "FP", "N0", "FN", "TN", "TP", "N1", "N1", "TN", "TN", "TN", "N1",         "TN", "N1", "N0", "N1", "TN", "N1", "TN", "N1", "N1", "FN", "N0")
Alc = c("TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN",        "TP", "FN", "TP", "TP", "TP", "TP", "TP", "TP", "TN", "TN", "FP",         "N0", "TN", "N0", "TN", "N1", "TP", "N0", "TN", "FP", "N0", "N0",         "TN", "N0", "FP", "FP", "N0", "N0", "N0", "TP", "TN", "TN", "TN",         "TN", "FN", "N0", "N0", "N1", "TN", "TN", "TN", "TN", "N0", "TP",         "TN", "N0", "TN", "TN", "TN", "N0", "FP", "TN", "N0", "N0", "TN",         "N0", "TN", "N0", "TN", "N0", "TN", "TN", "TN", "TP", "TN", "TP",         "TP", "TN", "TP", "TN", "N0", "TP", "TN", "TP", "TP", "N0", "N1",         "N0", "N0", "TP", "N1", "N1", "N0", "N0", "N0", "TN", "TN", "TN",         "N1", "TN", "FN", "N0", "N0", "N0", "N0", "N0", "N1", "N0", "N0",         "N1", "N0", "N1", "N0", "N0", "N1", "N0", "N0", "N0", "N0", "N1",         "TN", "N0", "TN", "N1", "TP", "N0", "TN", "N0", "N0", "N1", "N0",         "N0", "TN", "FP", "TN", "TP", "N0", "FP", "N0", "N0", "N0", "N0",         "FP", "N1", "TN", "N1", "TP", "N1", "N0", "N0", "N1", "TN", "TN",         "FP", "N0", "N1", "N0", "N1", "TP", "N1", "TN", "TN", "TN", "N1",         "N0", "TP", "FP", "TP", "N0", "TP", "TN", "N1", "N1", "FN", "N0")
RMI = c("TN", "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TN", "TN",        "TP", "FN", "TP", "TP", "TP", "TP", "FN", "TP", "TN", "TN", "TN",         "TN", "TN", "TN", "TN", "N1", "N1", "TN", "TN", "TN", "TN", "TN",         "TN", "TN", "FP", "N0", "FP", "TN", "TN", "N1", "TN", "TN", "TN",         "TN", "N1", "TN", "TN", "TP", "TN", "TN", "TN", "TN", "TN", "TP",         "TN", "FP", "TN", "TN", "TN", "FP", "TN", "TN", "TN", "TN", "TN",         "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "TN", "N1",         "TP", "TN", "N1", "TN", "N0", "TP", "N0", "TP", "N1", "TN", "TP",         "TN", "TN", "TP", "FN", "N1", "TN", "TN", "TN", "N0", "N0", "N0",         "N1", "TN", "TP", "N0", "TN", "N0", "N0", "TN", "N1", "N0", "TN",         "TP", "N0", "TP", "TN", "N0", "TP", "N0", "TN", "TN", "TN", "N1",         "FP", "TN", "TN", "FN", "N1", "TN", "TN", "TN", "TN", "FN", "TN",         "FP", "TN", "FP", "TN", "N1", "TN", "N0", "TN", "N0", "TN", "N0",         "TN", "N1", "FP", "FN", "N1", "N1", "TN", "TN", "TP", "TN", "TN",         "N0", "FP", "TP", "TN", "TP", "N1", "N1", "TN", "TN", "TN", "TP",         "TN", "N1", "N0", "TP", "TN", "N1", "TN", "N1", "N1", "FN", "N0")
mean_dec_owa_min_cen_0.025 =    c("TN", "TN", "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "FN", "TP", "TP", "TP", "TP", "TP", "TP", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "TP", "TN", "TN", "FP", "TN", "N0", "TN", "TN", "FP", "FP", "FP", "TN", "TN", "TP", "TN", "TN", "TN", "TN", "TP", "TN", "TN", "TP", "TN", "TN", "TN", "TN", "N0", "TP", "TN", "FP", "TN", "TN", "TN", "TN", "FP", "TN", "TN", "TN", "TN", "N0", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "TN", "TP", "TP", "TN", "TP", "N0", "TN", "TP", "TN", "TP", "TP", "TN", "TP", "TN", "TN", "TP", "TP", "N1", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "TN", "FN", "TN", "TN", "FP", "TN", "TN", "TP", "TN", "TN", "TP", "FP", "TP", "TN", "TN", "TP", "FP", "TN", "TN", "TN", "FN", "TN", "TN", "TN", "TP", "TP", "TN", "TN", "TN", "TN", "TP", "TN", "FP", "TN", "FP", "TN", "TP", "TN", "FP", "TN", "TN", "TN", "TN", "FP", "FN", "TN", "TP", "TP", "TP", "TN", "TN", "TP", "TN", "TN", "FP", "FP", "TP", "TN", "TP", "TP", "TP", "TN", "TN", "TN", "TP", "TN", "TP", "N0", "TP", "TN", "TP", "TN", "TP", "TP", "FN", "TN")
mean_ep_cen_0.0_3 =             c("TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "FN", "TP", "TP", "TP", "TP", "TP", "TP", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "TP", "TN", "TN", "FP", "TN", "TN", "TN", "TN", "FP", "FP", "FP", "TN", "TN", "TP", "TN", "TN", "TN", "TN", "TP", "TN", "TN", "TP", "TN", "TN", "TN", "TN", "TN", "TP", "TN", "FP", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TN", "TP", "TN", "TP", "TP", "TN", "TP", "TN", "TN", "TP", "TN", "TP", "TP", "TN", "TP", "TN", "TN", "TP", "TP", "N1", "TN", "TN", "TN", "FP", "TN", "TN", "TP", "TN", "FN", "N0", "TN", "FP", "N0", "TN", "TP", "TN", "TN", "TP", "FP", "TP", "TN", "FP", "TP", "FP", "TN", "TN", "TN", "N1", "FP", "TN", "TN", "FN", "TP", "TN", "TN", "TN", "TN", "TP", "TN", "FP", "TN", "TN", "TN", "TP", "TN", "FP", "TN", "N0", "TN", "TN", "FP", "FN", "TN", "TP", "TP", "TP", "TN", "TN", "TP", "TN", "TN", "FP", "FP", "TP", "TN", "TP", "TP", "TP", "TN", "TN", "TN", "TP", "TN", "TP", "FP", "TP", "TN", "TP", "TN", "TP", "TP", "FN", "TN")


skipNA = function(f){
    toReturn = f
    toReturn[f=="N0"] = "FP"
    toReturn[f=="N1"] = "FN"
    return(toReturn)
}

LR1.4 = skipNA(LR1)
LR2.4 = skipNA(LR2)
mean_dec_owa_min_cen_0.025.4 = skipNA(mean_dec_owa_min_cen_0.025)
mean_ep_cen_0.0_3.4 = skipNA(mean_ep_cen_0.0_3)

# ga1=gaOpt(ff1, ff2)
ga1=gaOpt(mean_dec_owa_min_cen_0.025.4, mean_ep_cen_0.0_3.4)

# ga1$evolve(3)
# plot(ga1)
# print(max(ga1$bestFit()))

# ga1=gaOpt(ff1, ff3)
# ga1$evolve(20)
# plot(ga1)
# print(max(ga1$bestFit()))

