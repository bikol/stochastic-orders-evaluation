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
