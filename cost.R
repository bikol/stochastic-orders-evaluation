## Cost function definition for 4 outcomes (TP,TN, FP, FN) model
cost.4 = list()
cost.4$minValue = 0
cost.4$maxValue = 5
cost.4$n0 = function(cost) {
    stop("Illegal state")
}
cost.4$n1 = function(cost) {
    stop("Illegal state")
}
cost.4$tp = function(cost) {
    return(pmax(0, -0.4 * cost + 1))
}
cost.4$tp.default = 0
cost.4$tn = function(cost) {
    return(pmax(0, -1 * cost + 1))
}
cost.4$tn.default = 0
cost.4$fp = function(cost) {
    return(if_else(cost <= 2.5, pmax(0, 0.4 * cost), pmax(0, -0.4 * cost + 2)))
}
cost.4$fp.default = 2.5
cost.4$fn = function(cost) {
    return(pmax(0, 0.2 * cost))
}
cost.4$fn.default = 5

# required for bruteOpt
cost.4$intesections = c(0,-5 / 7,-5 / 6,-1,-1.25,-5 / 3,-2.5,-10 / 3,-5)

# return initial point with all values equal to default for outcomes. Reward based
cost.4$getInitialPoint = function(f1, f2) {
    default1 = apply(cbind(f1, f2), 1, function(row) {
        return(switch(
            row[[1]],
            "TP" = 0,
            "TN" = 0,
            "FN" = -5,
            "FP" = -2.5
        ))
    })
    default2 = apply(cbind(f1, f2), 1, function(row) {
        return(switch(
            row[[2]],
            "TP" = 0,
            "TN" = 0,
            "FN" = -5,
            "FP" = -2.5
        ))
    })
    return(list(x = default1, y = default2))
}

# return point where f1 for sure dominates f2, if it is possible
# returns only optim variables, reward based
cost.4$getSurePoint = function(f1, f2) {
    x = c()
    y = c()
    for (i in 1:length(f1)) {
        if (f1[[i]] == "FP" && f2[[i]] == "TN") {
            x = append(x,-5 / 7)
            y = append(y,-5 / 7)
        } else if (f1[[i]] == "FN" && f2[[i]] == "TP") {
            x = append(x,-5 / 3)
            y = append(y,-5 / 3)
        } else if (f1[[i]] == "FN") {
            # f1[[i]]==f2[[i]]
            x = append(x,-5)
        } else if (f1[[i]] == "FP") {
            # f1[[i]]==f2[[i]]
            x = append(x,-2.5)
        } else if (f1[[i]] == "TN" && f2[[i]] == "FP") {
            y = append(y,-2.5)
        }
    }
    return(c(x, y))
}
##

## Cost function definition for 6 outcomes (TP,TN, N0 FP, N1, FN) model
cost.6 = list()
cost.6$minValue = 0
cost.6$maxValue = 5
cost.6$tp = function(cost) {
    return(pmax(0, -0.4 * cost + 1))
}
cost.6$tp.default = 0
cost.6$tn = function(cost) {
    return(pmax(0, -1 * cost + 1))
}
cost.6$tn.default = 0
cost.6$fp = function(cost) {
    return(if_else(cost <= 2.5, pmax(0, 0.4 * cost), pmax(0, -0.4 * cost + 2)))
}
cost.6$fp.default = 2.5
cost.6$fn = function(cost) {
    return(pmax(0, 0.2 * cost))
}
cost.6$fn.default = 5
cost.6$n0 = function(cost) {
    return(if_else(cost <= 1, pmax(0, cost), pmax(0,-cost + 2)))
}
cost.6$n0.default = 1
cost.6$n1 = function(cost) {
    return(if_else(cost <= 2, pmax(0, cost - 1), pmax(0, (-1 / 3) * cost + (5 /
                                                                                3))))
}
cost.6$n1.default = 2

# required for bruteOpt, invalid for cost.6
cost.6$intesections = c(0,-5 / 7,-5 / 6,-1,-1.25,-5 / 3,-2.5,-10 / 3,-5)

# return initial point with all values equal to default for outcomes. Reward based
cost.6$getInitialPoint = function(f1, f2) {
    default1 = apply(cbind(f1, f2), 1, function(row) {
        return(switch(
            row[[1]],
            "TP" = 0,
            "TN" = 0,
            "FN" = -5,
            "FP" = -2.5,
            "N0" = -1,
            "N1" = -2
        ))
    })
    default2 = apply(cbind(f1, f2), 1, function(row) {
        return(switch(
            row[[2]],
            "TP" = 0,
            "TN" = 0,
            "FN" = -5,
            "FP" = -2.5,
            "N0" = -1,
            "N1" = -2
        ))
    })
    return(list(x = default1, y = default2))
}

# return point where f1 for sure dominates f2, if it is possible
# returns only optim variables, reward based
cost.6$getSurePoint = function(f1, f2) {
    x = c()
    y = c()
    for (i in 1:length(f1)) {
        if (f1[[i]] == "FP" && f2[[i]] == "TN") {
            x = append(x,-5 / 7)
            y = append(y,-5 / 7)
        } else if (f1[[i]] == "FN" && f2[[i]] == "TP") {
            x = append(x,-5 / 3)
            y = append(y,-5 / 3)
        } else if (f1[[i]] == "N0" && f2[[i]] == "FP") {
            x = append(x,-1)
            y = append(y,-2.5)
        } else if (f1[[i]] == "N0" && f2[[i]] == "TN") {
            x = append(x,-0.5)
            y = append(y,-0.5)
        } else if (f1[[i]] == "N1" && f2[[i]] == "TP") {
            x = append(x,-10 / 7)
            y = append(y,-10 / 7)
        } else if (f1[[i]] == "FP" && f2[[i]] == "N0") {
            x = append(x,-10 / 7)
            y = append(y,-10 / 7)
        } else if (f1[[i]] == "FN" && f2[[i]] == "N1") {
            x = append(x,-25 / 7)
            y = append(y,-25 / 7)
        } else if (f1[[i]] == "N1" && f2[[i]] == "FN") {
            x = append(x,-2)
        } else if (f1[[i]] == "TN" && f2[[i]] == "N0") {
            y = append(y,-1)
        } else if (f1[[i]] == "TP" && f2[[i]] == "N1") {
            y = append(y,-2)
        } else if (f1[[i]] == "N0" && f2[[i]] == "N0") {
            x = append(x,-1)
        } else if (f1[[i]] == "N1" && f2[[i]] == "N1") {
            x = append(x,-2)
        } else if (f1[[i]] == "FN" && f2[[i]] == "FN") {
            x = append(x,-5)
        } else if (f1[[i]] == "FP" && f2[[i]] == "FP") {
            x = append(x,-2.5)
        } else if (f1[[i]] == "TN" && f2[[i]] == "FP") {
            y = append(y,-2.5)
        }
    }
    return(c(x, y))
}
##
