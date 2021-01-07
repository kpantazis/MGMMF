# Generates template 
# from background
# by deleting 
# n-m vertices
library(igraph)
subgraph <- function(gg,nc,nm, nn){
  hh <- list()
  for(i in 1:nc){
    
    hh[[i]] <- as.matrix(gg[["g2"]][[i]])
    #hh[[i]] <- lapply(hh[[i]], as.numeric)
    #as.numeric(unlist(gg[[2]][i]))
    #gg[[2]][i] <- as.matrix(gg[[2]][i])
    hh[[i]] <- graph_from_adjacency_matrix(hh[[i]])
    hh[[i]] <- delete_vertices(hh[[i]], seq(nm+1, nn))
    hh[[i]] <- as_adjacency_matrix(hh[[i]])
    #kk[[i]] <- as.symmetricAdjacencyMatrix(kk[[i]])
    gg[["g2"]][[i]] <- hh[[i]]
  } 
  return(gg[["g2"]])}