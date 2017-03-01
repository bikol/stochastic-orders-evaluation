library(igraph)

source("config.R")

if(!exists("results")){
    e1 <- new.env()
    load(RESULTS.LOCATION, envir=e1)
    results <- get("results", e1)
    rm(e1)
}

result.norm2 = results
result.norm2 = (result.norm2-min(result.norm2))/(1-min(result.norm2))
diag(result.norm2) = rep(0, ncol(results))

removeToSmall = function(mx, ths){
    mx[mx<=ths]=0
    return(mx)
}

result.norm3 = result.norm2/(result.norm2+t(result.norm2))

diag(result.norm2) = rep(0, ncol(results))
diag(result.norm3) = rep(0, ncol(results))

sel = names(sort(apply(result.norm3, 2, max))[1:10])
sel2 = names(sort(apply(result.norm2, 2, max))[1:10])

gr= graph_from_adjacency_matrix(removeToSmall(result.norm3[sel, sel], 0.57), mode=list("directed"), weighted = T, diag=F)
gr2= graph_from_adjacency_matrix(removeToSmall(result.norm2[sel2, sel2], 0.25), mode=list("directed"), weighted = T, diag=F)

plot(gr, layout=layout_nicely(gr), edge.label=round(E(gr)$weight,2))
plot(gr2, layout=layout_nicely(gr2), edge.label=round(E(gr)$weight,2))
