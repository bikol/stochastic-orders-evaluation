getOptimVarCount = function(f1, f2) {
    fillIndex = 0
    for (i in 1:length(f1)) {
        if (!(f1[[i]] == "TP" || f1[[i]] == "TN")) {
            fillIndex = fillIndex + 1
        }
    }
    for (i in 1:length(f1)) {
        if (f1[[i]] != f2[[i]] && f2[[i]] != "FN") {
            fillIndex = fillIndex + 1
        }
    }
    return(fillIndex)
}

gen.getFilled = function(f1, f2, cost) {
    force(f1)
    force(f2)
    force(cost)

    copyFromFill = c()
    for (i in 1:length(f1)) {
        if (f1[[i]] == "TP" || f1[[i]] == "TN") {
            copyFromFill = append(copyFromFill, switch(
                f1[[i]],
                "TN" = -cost$tn.default,
                "TP" = -cost$tp.default
            ))

        } else{
            copyFromFill = append(copyFromFill, NA)
        }
    }

    copyFromX = c()
    for (i in 1:length(f2)) {
        if (f1[[i]] == f2[[i]]) {
            copyFromFill = append(copyFromFill, 666)
            copyFromX = append(copyFromX, NA)
        } else if (f2[[i]] == "FN") {
            copyFromFill = append(copyFromFill,-cost$fn.default)
            copyFromX = append(copyFromX, 666)
        } else {
            copyFromFill = append(copyFromFill, NA)
            copyFromX = append(copyFromX, 666)
        }
    }
    return(function(fill) {
        # fill should be reward based
        force(fill)
        newCopyFromFill = c(copyFromFill)
        newCopyFromFill[is.na(newCopyFromFill)] = fill

        x = newCopyFromFill[1:(length(newCopyFromFill) / 2)]
        y = newCopyFromFill[((length(newCopyFromFill) / 2) + 1):length(newCopyFromFill)]

        y[is.na(copyFromX)] = x[is.na(copyFromX)]
        return(list(x = x, y = y))
    })
}


stochastic.dominance.func = function(x, y, f1, f2, cost) {
    # x and y are reward based

    # check whether x dominates y in 1st stachastic dominance
    unq = unique(c((-cost$max - 1), x, y, (-cost$min + 1)))
    for (c in unq) {
        if ((sum(x > c) / length(x)) < (sum(y > c) / length(y))) {
            return(0)
        }
    }

    # otherwise x>y so we can calculate possibility degree
    p.x = min(
        recode(
            f1,
            TP = cost$tp(-x),
            TN = cost$tn(-x),
            N0 = cost$n0(-x),
            N1 = cost$n1(-x),
            FN = cost$fn(-x),
            FP = cost$fp(-x)
        )
    )

    # efficacy optimisation
    if (p.x <= 0) {
        return(0)
    }
    p.y = min(
        recode(
            f2,
            TP = cost$tp(-y),
            TN = cost$tn(-y),
            N0 = cost$n0(-y),
            N1 = cost$n1(-y),
            FN = cost$fn(-y),
            FP = cost$fp(-y)
        )
    )
    return(min(p.x, p.y))
}

bruteOpt = function(f1, f2, cost, iters = 100000) {
    count = getOptimVarCount(f1, f2)
    flog.debug("Variables to optimise: %d", count)
    fill = gen.getFilled(f1, f2, cost)
    eval.func = function() {
        x = sample(cost$intesections,
                   count,
                   replace = T)
        filled = fill(x)
        return(stochastic.dominance.func(filled[[1]], filled[[2]], f1, f2, cost))
    }
    return(max(replicate(iters, eval.func())))
}

gaOpt = function(f1,
                 f2,
                 cost,
                 popSize = 25000,
                 mutRate = 0.05) {
    count = getOptimVarCount(f1, f2)
    flog.debug("Variables to optimise: %d", count)
    fill = gen.getFilled(f1, f2, cost)
    return(GAReal(
        function(x) {
            filled = fill(x)
            return(stochastic.dominance.func(filled[[1]], filled[[2]], f1, f2, cost))
        },
        rep(-cost$max, count),
        rep(-cost$min, count),
        popSize = popSize,
        mutRate = mutRate
    ))
}

numericalOpt = function(f1,
                        f2,
                        cost,
                        maxit = 10000,
                        method = "BFGS") {
    count = getOptimVarCount(f1, f2)
    flog.debug("Variables to optimise: %d", count)
    fill = gen.getFilled(f1, f2, cost)
    eval.func = function(x) {
        filled = fill(x)
        return(stochastic.dominance.func(filled$x, filled$y, f1, f2, cost))
    }
    par = cost$getSurePoint(f1, f2)
    if (length(par) != count) {
        flog.error("SurePoint:", par, capture = T)
        flog.error("Optimisation variables count: %d", count)
        stop("SurePoint lenght not match variables to optimise count.")
    }
    return(optim(
        par,
        eval.func,
        method = method,
        control = list(fnscale = -1, maxit = maxit)
    ))
}

calcDominationDegree = function(f1, f2, cost, maxit = 10000) {
    init.p = cost$getInitialPoint(f1, f2)
    init.val = stochastic.dominance.func(init.p$x, init.p$y, f1, f2, cost)
    if (abs(1 - init.val) < 1e-4) {
        flog.debug("Initial point reaches full diminance")
        return(list(
            val = 1.0,
            x = init.p$x,
            y = init.p$y
        ))
    }
    flog.debug("BFGS optim")
    numOpt1 = numericalOpt(f1, f2, cost, method = "BFGS", maxit = maxit)
    flog.debug("Solution found: %s in %s steps",
               numOpt1$value,
               numOpt1$counts[[1]] + numOpt1$counts[[2]])
    flog.debug("Nelder-Mead optim")
    numOpt2 = numericalOpt(f1, f2, cost, method = "Nelder-Mead", maxit =
                               maxit)
    flog.debug("Solution found: %s in %s steps",
               numOpt2$value,
               numOpt2$counts[[1]])
    if (numOpt1$value > numOpt2$value) {
        flog.debug("Best: BFGS")
        numOpt = numOpt1
    } else {
        flog.debug("Best: Nelder-Mead")
        numOpt = numOpt2
    }
    fill = gen.getFilled(f1, f2, cost)
    filled = fill(numOpt$par)
    return(list(
        val = numOpt$value,
        x = filled$x,
        y = filled$y
    ))
}
