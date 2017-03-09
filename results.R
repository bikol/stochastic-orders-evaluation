library(igraph)
library(tikzDevice)
options(tikzPdftexWarnUTF = FALSE)

source("config.R")

if(!exists("results")){
    e1 <- new.env()
    load(RESULTS.LOCATION, envir=e1)
    results <- get("results", e1)
    rm(e1)
}

# normalisation
results.norm = (results-min(results))/(1-min(results))
results.norm = results.norm/(results.norm+t(results.norm))
diag(results.norm) = rep(0, ncol(results.norm))


removeToSmall = function(mx, ths){
    mx[mx<=ths]=0
    return(mx)
}


sel = c("mean_(dec_(owa_cen))_cen_0.025",        "mean_(dec_(owa_min))_cen_0.025",        "mean_ep_min_0.0_3",        "mean_ep_cen_0.0_3",        "iMean_wid_(cen_0.025)_2",        "mean_(dec_(owa_1))_cen_0.025",        "iMean_em_(cen_0.0)_3",        "mean_wid_min_0.025_3",        "mean_ep_max_0.0_3",        "cho_auc_cen_0.025",        "cho_card_cen_0.025",        "i_sug_card_(cen_0.025)",        "t.min_max_0.0")
labels = LETTERS[1:length(sel)]

# proposed approach graph
gr= graph_from_adjacency_matrix(
    removeToSmall(results.norm[sel, sel], 0.59),
    mode=list("directed"),
    weighted = T,
    diag=F)
tikz(file="latex/graph-proposed.tex", sanitize = T, height = 5, width = 6,
     documentDeclaration = '\\documentclass[11pt,a4paper,oldfontcommands]{memoir}')
plot(gr, layout=layout_with_sugiyama(gr)$layout, vertex.label=labels, edge.label=round(E(gr)$weight,2), edge.arrow.size=0.5,edge.label.cex=0.75)
dev.off()


# statistical preference graph
sp.matrix = as.matrix(read.csv("datasets/sp-dom-matrix.csv", header=T, row.names=1,check.names=F))
gr.sp= graph_from_adjacency_matrix(
    sp.matrix[sel, sel],
    mode=list("directed"),
    weighted = T,
    diag=F)
tikz(file="latex/graph-sp.tex", sanitize = T, height = 5, width = 6,
     documentDeclaration = '\\documentclass[11pt,a4paper,oldfontcommands]{memoir}')
plot(gr.sp, layout=layout_with_sugiyama(gr.sp)$layout, vertex.label=labels, edge.arrow.size=0.5,edge.label.cex=0.75)
dev.off()

# 1st dominance graph
st.matrix = as.matrix(read.csv("datasets/1st-dom-matrix.csv", header=T, row.names=1,check.names=F))
gr.1st= graph_from_adjacency_matrix(
    st.matrix[sel, sel],
    mode=list("directed"),
    weighted = T,
    diag=F)
tikz(file="latex/graph-1st.tex", sanitize = T, height = 5, width = 6,
     documentDeclaration = '\\documentclass[11pt,a4paper,oldfontcommands]{memoir}')
plot(gr.1st, layout=layout_with_sugiyama(gr.1st)$layout, vertex.label=labels, edge.arrow.size=0.5,edge.label.cex=0.75)
dev.off()
