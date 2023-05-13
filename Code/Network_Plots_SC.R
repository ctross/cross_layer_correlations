
#######################
color <- shape <- c()
########################
A_Give = model_dat_sc$Give
A_Exploit = model_dat_sc$Exploit
A_Reduce = model_dat_sc$Reduce
A_Selfish = model_dat_sc$Selfish
A_Generous = model_dat_sc$Generous

set.seed(1)
X = ifelse(A_Reduce>0,1,0)
diag(X)=rep(0,nrow(X))
net1 = graph_from_adjacency_matrix(X, mode = c("directed"))
Isolated = which(igraph::degree(net1)==0)
net1 = igraph::delete.vertices(net1, Isolated)

X = ifelse(A_Give>0,1,0)
diag(X)=rep(0,nrow(X))
net2 = graph_from_adjacency_matrix(X, mode = c("directed"))
Isolated = which(igraph::degree(net2)==0)
net2 = igraph::delete.vertices(net2, Isolated)

X = ifelse(A_Exploit==0,0,1)
diag(X)=rep(0,nrow(X))
Nq = nrow(A_Exploit)
X = X * matrix(rbinom(Nq^2,1,0.4), nrow=Nq, ncol=Nq) # Thin out
net3 = graph_from_adjacency_matrix(X, mode = c("directed"))
Isolated = which(igraph::degree(net3)==0)
net3 = igraph::delete.vertices(net3, Isolated)

X = ifelse(A_Selfish>0,1,0)
diag(X)=rep(0,nrow(X))
net4 = graph_from_adjacency_matrix(X, mode = c("directed"))
Isolated = which(igraph::degree(net4)==0)
net4 = igraph::delete.vertices(net4, Isolated)

X = ifelse(A_Generous>0,1,0)
diag(X)=rep(0,nrow(X))
net5 = graph_from_adjacency_matrix(X, mode = c("directed"))
Isolated = which(igraph::degree(net5)==0)
net5 = igraph::delete.vertices(net5, Isolated)

red_mountain = c("#800000FF", "#c26a1c", "#CC8214FF", "#616530FF", "#0F425CFF", 
"#9A5324FF", "#642822FF", "#26561f", "#350E20FF")

calming_como = c("#34261D", "#A96922", "#7D370D", "#1C7262","#114B47")

png("Selfish_SC.png", 1200, 1200)
plot(net4, layout=layout_with_lgl, edge.arrow.size =0.1, edge.curved = 0.3, vertex.frame.color=NA, vertex.color=adjustcolor(calming_como[3], alpha.f = 0.9),
                  vertex.label=NA, vertex.size = 7, edge.color = adjustcolor( "black", alpha.f = 0.7))
dev.off()

png("Exploit_SC.png", 1200, 1200)
plot(net3, layout=layout_with_lgl, edge.arrow.size =0.1, edge.curved = 0.3, vertex.frame.color=NA, vertex.color=adjustcolor(calming_como[2], alpha.f = 0.9),
                  vertex.label=NA, vertex.size = 7, edge.color = adjustcolor( "black", alpha.f = 0.6))
dev.off()

png("Punish_SC.png", 1200, 1200)
plot(net1, layout=layout_with_lgl, edge.arrow.size =0.1, edge.curved = 0.3, vertex.frame.color=NA, vertex.color=adjustcolor(calming_como[1], alpha.f = 0.9),
                  vertex.label=NA, vertex.size = 7, edge.color = adjustcolor( "black", alpha.f = 0.7))
dev.off()
 

png("Generous_SC.png", 1200, 1200)
plot(net5, layout=layout_with_lgl, edge.arrow.size =0.1, edge.curved = 0.3, vertex.frame.color=NA, vertex.color=adjustcolor(calming_como[5], alpha.f = 0.9),
                  vertex.label=NA, vertex.size = 7, edge.color = adjustcolor( "black", alpha.f = 0.7))
dev.off()

png("Give_SC.png", 1200, 1200)
plot(net2, layout=layout_with_lgl, edge.arrow.size =0.1, edge.curved = 0.3, vertex.frame.color=NA, vertex.color=adjustcolor(calming_como[4], alpha.f = 0.9),
                  vertex.label=NA, vertex.size = 7, edge.color = adjustcolor( "black", alpha.f = 0.7))
dev.off()
