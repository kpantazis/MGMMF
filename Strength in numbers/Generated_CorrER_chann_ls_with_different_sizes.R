# THE CODE IS WRITTEN BY DANIEL SUSSMAN ASSISTANT PROFESSOR AT BU



#Generates ER-correlated pair of graphs (G,H), where G is the background (n-vertex graph)
#and H is its induced m-vertex subgraph (without vertex permutation).

#Produces Figures 1-3




library(igraph)
library(iGraphMatch)
library(purrr)
library(tidyverse)


param <- list(nc = 1:10, nn = c(100,500,1000,2000),nm = c(100),
              rho = c(.5), p = 0.5, mc = 1:100,
              ns =10) %>%
  cross()



#jobid <- 1


sample_correlated_multi <- function(nn, nc, p, rho){
  pair_l <- rerun(nc,
                  sample_correlated_gnp_pair(nn, rho, p))
  
  #g2 <- pair_l %>% map(~.x[[2]][])
  #subgraph(g2,nc,nm,nn)
  list(g1 = pair_l %>% map(~.x[[1]][]),
       g2 = pair_l %>% map(~.x[[2]][]))
}

#gg <- sample_correlated_multi(nn, nc[10], p, rho[1])

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
##coerce(signature(from = "dgCMatrix", to = "matrix"))
#subgraph(gg,nc[10],nm,nn)

run <- function(nn, nc, nm, p, rho, ns, mc){
  cat(format(Sys.time()), "\n")
  cat(mc, nn, nm, nc, rho, "\n")
  #nm <- 50
  gp <- sample_correlated_multi(nn, nc, p, rho)
  for(i in 1:nc){
  gp$g1[[i]] <- center_graph(as.matrix(gp$g1[[i]]), scheme = c(-1, 1), use_splr = FALSE)}
  #as.matrix(gp$g1[[1]])
  if(nm < nn){
      gp$g2 <- subgraph(gp, nc, nm, nn)
      for(i in 1:nc){
        gp$g2[[i]] <- center_graph(as.matrix(gp$g2[[i]]), scheme = c(-1, 1), use_splr = FALSE)}
      match <- graph_match_FW(gp$g1, gp$g2, 
                          seeds = 1:ns, start = "bari")
      #print("hey")
  }
  else{
    for(i in 1:nc){
      gp$g2[[i]] <- center_graph(as.matrix(gp$g2[[i]]), scheme = c(-1, 1), use_splr = FALSE)}
      match <- graph_match_FW(gp$g1, gp$g2, 
                                seeds = 1:ns, start = "bari")
      #print("vlaka")
  }
  
  acc <- (sum(match$corr$corr_B == 1:nm)) / nm
  tibble(jobid, mc, nn, nc, p, rho, ns, acc)

}
#run(nn, nc[10], nm, p, rho[1], ns, mc)

#nc = 10
#nn = c(500)
#nm = c(100)
##rho = c(.5)
##p = 0.5
#mc = 1
#ns =10
#jobid <- 1





##########################################################
########################################################
##########################################################
#########################################################
# Set total jobs

#cat(date(), "\n")

#totalJobs <- 200

# Get job ID if its available
##if ( Sys.getenv("SLURM_JOB_ID") != "" ){
#  jobid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#  savedf <- FALSE
#} else {
#  jobid <- 1
#  savedf <- TRUE
#}



# multigm_corr_er.R

# # Correllated ER

# More channels need less correlation
# vary correlation and # channels
# include seeds
savedf <- TRUE
# Set Seed
jobid <- 1
set.seed(jobid + 12344)

#if(savedf){
# param <- list(nc = 1:10, nn = c(30),
#               rho = c(.1, .2, .3, .4, .5), p = 0.5, mc = 1,
#                ns = 10) %>%
#    cross()
#} else {
#  param <- list(nc = c(1,10), nn = c(50),
#                rho = c(.3, .5), p = 0.5, mc = 1:2,
#                ns = 10) %>%
##   cross()
#}







res_df <- param %>% map_df(lift(run))

if(savedf){
  save(res_df, file = paste0("multigm_corr_er_job_", jobid, ".RData"))
}



  require(tidyverse)
  all_df <- 1:200 %>% map_df(~{
    load(paste0("multigm_corr_er_job_", jobid, ".RData"))
    res_df
  })
  
  save(all_df, file = "multigm_corr_er_all.RData")
  all_df



  load("multigm_corr_er_all.RData")
  all_df %>% group_by(rho, nc) %>%
    summarize(acc = mean(acc)) %>%
    ggplot(aes(x = nc, y = acc, color = factor(rho))) +
    geom_point() +
    geom_line() +
    scale_color_discrete("correlation") + 
    ylab("accuracy") + xlab("# channels") +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = NULL)
  
  all_df %>% group_by(rho, nc) %>% 
    ggplot(aes(x = nc, y = acc, color = factor(rho))) +
    stat_summary(fun.data = "mean_se", geom = "line") +
    geom_point(aes(x = nc + rho - .2), alpha = .01) +
    scale_color_discrete("correlation") + 
    ylab("accuracy") + xlab("# channels") +
    scale_x_continuous(breaks = 1:10, labels = 1:10, minor_breaks = NULL)


