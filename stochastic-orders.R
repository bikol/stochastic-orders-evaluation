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

# return point where f1 for sure dominates f2, if it is possible
getSurePoint = function(f1, f2) {
    x = c()
    y = c()
    for(i in 1:length(f1)){
        if(f1[[i]]=="FP" && f2[[i]]=="TN"){
            x = append(x, 5/7)
            y = append(y, 5/7)
        } else if(f1[[i]]=="FN" && f2[[i]]=="TP"){
            x = append(x, 5/3)
            y = append(y, 5/3)
        } else if(f1[[i]]=="FN"){
            # f1[[i]]==f2[[i]]
            x = append(x, cost.func.fn.default)
        } else if(f1[[i]]=="FP"){
            # f1[[i]]==f2[[i]]
            x = append(x, cost.func.fp.default)
        } else if(f1[[i]]=="TN" && f2[[i]]=="FP"){
            y = append(y, cost.func.fp.default)
        }
    }
    return(c(x, y))
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

bruteOpt = function(f1, f2, iters = 100000) {
    count = getOptimVarCount(f1, f2)
    fill = gen.getFilled(f1, f2)
    eval.func = function() {
        x = sample(cost.func.intesections,
                   count,
                   replace = T)
        filled = fill(x)
        return(stochastic.dominance.func(filled[[1]], filled[[2]], f1, f2))
    }
    return(max(replicate(iters, eval.func())))
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

numericalOpt = function(f1, f2, maxit=1000000, method = "BFGS") {
    count = getOptimVarCount(f1, f2)
    flog.debug(paste0("Variables to optimise: ", count))
    fill = gen.getFilled(f1, f2)
    eval.func = function(x){
        filled=fill(x)
        return(stochastic.dominance.func(-filled$x, -filled$y, f1, f2))
    }
    par = getSurePoint(f1, f2)
    if(length(par) != count){
        flog.error("SurePoint:",par,capture=T)
        flog.error("Optimisation variables count: %d", count)
        stop("SurePoint lenght not match variables to optimise count.")
    }
    return(optim(par, eval.func, method = method, control=list(fnscale=-1, maxit=maxit)))
}

calcDominationDegree = function(f1, f2){
    init.p = getInitialPoint(f1, f2)
    init.val = stochastic.dominance.func(init.p$x, init.p$y, f1, f2)
    if(abs(1-init.val)<1e-4) {
        return(list(val=1.0, x=init.p$x, y=init.p$y))
    }

    numOpt = numericalOpt(f1, f2)
    fill = gen.getFilled(f1, f2)
    filled = fill(numOpt$par)
    return(list(val=numOpt$value, x=filled$x, y=filled$y))
}
