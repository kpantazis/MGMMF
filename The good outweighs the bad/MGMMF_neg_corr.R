# Produces Figs 4-6

  savedf <- TRUE

  jobid <- 1

# Set Seed
set.seed(jobid + 12344)

# multigm_neg_corr.R

# # Correllated ER

# More channels need less correlation
# vary correlation and # channels
# include seeds


library(tidyverse)
library(igraph)
library(iGraphMatch)

if (savedf){
  param <- list(
    nc = 10,
    nn = c(100,500,1000,2000),
    nm = c(100),
    npc = 0:10,
    rho = c(.5),
    p = 0.5,
    mc = 1:100,
    ns = 10) %>%
    cross()
} else {
  param <- list(
    nc = 10, 
    nn = c(50), 
    npc = c(0, 5, 10),
    rho = c(.3, .5), 
    p = 0.5, 
    mc = 1:2,
    ns = 10) %>%
    cross()
}


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

sample_correlated_multi <- function(nn, nc, p, rho, npc){
  pair_l <- c(
    rerun(nc - npc, sample_correlated_gnp_pair(nn, rho, p)),
    rerun(npc, sample_correlated_gnp_pair(nn, -rho, p)))
  
  list(g1 = pair_l %>% map(~.x[[1]][]),
       g2 = pair_l %>% map(~.x[[2]][]))
}

run <- function(nn, nc, p, nm, rho, ns, mc, npc){
  cat(format(Sys.time()), "\n")
  cat(mc, nn, nc, rho, npc, "\n")
  gp <- sample_correlated_multi(nn, nc, p, rho, npc)
  if(nm < nn){
    gp$g2 <- subgraph(gp, nc, nm, nn)
    match <- graph_match_FW(gp$g1, gp$g2, 
                            seeds = 1:ns, start = "bari")
    #print("hey")
  }
  else{
    match <- graph_match_FW(gp$g1, gp$g2, 
                            seeds = 1:ns, start = "bari")
    #print("vlaka")
  }
  
  acc <- (sum(match$corr$corr_B == 1:nm)) / nm
  tibble(jobid, mc, nn, nc, p, rho, ns, npc, acc)
}

res_df <- param %>% map_df(lift(run))
if(savedf){
  save(res_df, file = paste0("multigm_neg_corr_job_", jobid, ".RData"))
}



  require(tidyverse)
  all_df <- 1:200 %>% map_df(~{
    load(paste0("multigm_neg_corr_job_", jobid, ".RData"))
    res_df
  })
  
  save(all_df, file = "multigm_neg_corr_all.RData")
  all_df


make_fig <- function(){
  load("multigm_neg_corr_all.RData")
  all_df %>% group_by(nn, npc) %>%
    summarize(acc = mean(acc)) %>%
    ggplot(aes(x = npc, y = acc, color = factor(nn))) +
    geom_point() +
    geom_line() +
    scale_color_discrete("# vertices") + 
    ylab("accuracy") + xlab("# neg. corr. channels") +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = NULL)
  
  all_df %>%
    ggplot(aes(x = npc, y = acc, color = factor(nn))) +
    stat_summary(fun.data = "mean_se", geom = "line") +
    geom_point(aes(x = npc + rho - .2), alpha = .01) +
    scale_color_discrete("# vertices") + 
    ylab("accuracy") + xlab("# neg. corr. channels") +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = NULL)
}
