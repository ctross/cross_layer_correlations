######################################################################## Table 1

 netsum = function(X){
  X = ifelse(X>0,1,0)
  x = c()
  x[1] = vcount(graph_from_adjacency_matrix(X, mode = c("directed")) )
  x[2] = ecount(graph_from_adjacency_matrix(X, mode = c("directed")) )
  x[3] = edge_density(graph_from_adjacency_matrix(X, mode = c("directed")) )
  x[4] = mean_distance(graph_from_adjacency_matrix(X, mode = c("directed")) )
  x[5] = reciprocity(graph_from_adjacency_matrix(X, mode = c("directed")) )
  x[6] = transitivity(graph_from_adjacency_matrix(X, mode = c("directed")) )
  return(x)
  }
                     
                     
 NetProps = matrix(NA, ncol=8, nrow=10)                     
                     
  NetProps[1,] = c("Coastal","Generous rating", netsum(model_dat_bs$Generous))   
  NetProps[2,] = c("Coastal","Selfish rating", netsum(model_dat_bs$Selfish))                  
  NetProps[3,] = c("Coastal","RICH Giving", netsum(model_dat_bs$Give))  
  NetProps[4,] = c("Coastal","RICH Exploitation", netsum(model_dat_bs$Exploit))  
  NetProps[5,] = c("Coastal","RICH Punishment", netsum(model_dat_bs$Reduce))  

  NetProps[6,] = c("Lowland","Generous rating", netsum(model_dat_sc$Generous))   
  NetProps[7,] = c("Lowland","Selfish rating", netsum(model_dat_sc$Selfish))                  
  NetProps[8,] = c("Lowland","RICH Giving", netsum(model_dat_sc$Give))  
  NetProps[9,] = c("Lowland","RICH Exploitation", netsum(model_dat_sc$Exploit))  
  NetProps[10,] = c("Lowland","RICH Punishment", netsum(model_dat_sc$Reduce))  
                          
  NetProps[,5:8] = round(as.numeric(NetProps[,5:8]), 3) 

  colnames(NetProps) = c("Site","Network","Vertices","Edges","Density","Avg. Dist.","Reciprocity","Transitivity")                  
                     
  write.csv(NetProps, "networkproperties.csv", row.names=FALSE, quote = FALSE) 
  print(xtable(NetProps),include.rownames=FALSE)
