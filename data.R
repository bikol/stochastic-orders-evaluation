ff1 = c("TN", "TN", "TN", "TN", "FP", "TP", "TP", "TP", "TP", "FN")
ff2 = c("TN", "FP", "FP", "FP", "FP", "TP", "TP", "TP", "TP", "TP")
ff3 = c("TN", "TN", "TN", "TN", "FP", "TP", "TP", "TP", "FN", "FN")

skipNA = function(f){
    toReturn = f
    toReturn[f=="N0"] = "FP"
    toReturn[f=="N1"] = "FN"
    return(toReturn)
}
