
#######################
Give = model_dat_tb$Give
Exploit = model_dat_tb$Exploit
Reduce = model_dat_tb$Reduce
Selfish = model_dat_tb$Selfish
Generous = model_dat_tb$Generous

diag(Give)=0
diag(Exploit)=0
diag(Reduce)=0
diag(Selfish)=0
diag(Generous)=0

gq1 <- graph_from_adjacency_matrix(Give) 
eq1 <- graph_from_adjacency_matrix(Exploit) 
rq1 <- graph_from_adjacency_matrix(Reduce) 

d_g2 <- get.data.frame(gq1)
d_e2 <- get.data.frame(eq1)
d_r2 <- get.data.frame(rq1)

d_g2$Layer = "Give"
d_e2$Layer = "Exploit"
d_r2$Layer = "Reduce"

d_all = rbind(d_g2,d_e2,d_r2)
d_all = d_all[which(d_all$from %in% colnames(Generous) & d_all$to %in% colnames(Generous)),]

calming_como = c("#34261D", "#A96922", "#7D370D", "#1C7262","#114B47","#CF821B","#E4A835")

#######################################################
d_all_r = d_all[which(d_all$Layer == "Reduce"),]

graph <- graph_from_data_frame(d_all_r, directed=TRUE, vertices=colnames(Generous))

Standing = colSums(Generous)-colSums(Selfish)

V(graph)$Standing <- as.character(ifelse(Standing>8,"High",ifelse(Standing < -1, "Low","Average")))
V(graph)$indeg <- Standing #igraph::degree(graph, mode = 'out')
V(graph)$layer <- rep("Punishment",length(Standing))

X = 30

P1 = ggraph(graph, 'hive', axis = layer, sort.by = indeg, normalize=FALSE,use.numeric=FALSE,split.axes="all",split.angle=1,center.size=1,divide.by=Standing, divide.order=c("Low","Average","High")) + 
    geom_edge_hive(color = "#3e3131", alpha=0.18, strength=1) + 
    geom_axis_hive(aes(color = Standing), size = 3) + 
    coord_fixed() + theme(legend.position="none") + 
    theme(strip.text.x = element_text(size = X+2), legend.text = element_text(size = X), legend.title = element_text(size = X))  +
    scale_colour_manual(values = c( "High" = calming_como[7], "Average" = calming_como[6], "Low" = calming_como[3]),name = "Reputation:", guide = guide_legend(override.aes = list(fill=NA,alpha = 0,color = "white") )) + 
    scale_linetype(guide = guide_legend(override.aes = list(alpha = 0) ) )+
    theme(legend.title = element_text(color = "transparent"),
     legend.text = element_text(color = "transparent")) + theme(legend.background=element_blank(),legend.key = element_rect(fill = NA, color = NA),panel.background = element_rect(fill = "white")) 

#######################################################   
d_all_e = d_all[which(d_all$Layer == "Exploit"),]

graph <- graph_from_data_frame(d_all_e, directed=TRUE, vertices=colnames(Generous))

Standing = colSums(Generous)-colSums(Selfish)

V(graph)$Standing <- as.character(ifelse(Standing>8,"High",ifelse(Standing < -1, "Low","Average")))
V(graph)$indeg <- Standing #igraph::degree(graph, mode = 'out')
V(graph)$layer <- rep("Exploitation",length(Standing))


P2 = ggraph(graph, 'hive', axis = layer, sort.by = indeg, normalize=FALSE,use.numeric=FALSE,split.axes="all",split.angle=1,center.size=1,divide.by=Standing, divide.order=c("Low","Average","High")) + 
    geom_edge_hive(color = "#3e3131", alpha=0.014, strength=1) + 
    geom_axis_hive(aes(color = Standing), size = 3) + 
    coord_fixed() + theme(legend.position="none") + 
    theme(strip.text.x = element_text(size = X+2), legend.text = element_text(size = X), legend.title = element_text(size = X),panel.background = element_rect(fill = "white"))  +
    scale_colour_manual(values = c( "High" = calming_como[7], "Average" = calming_como[6], "Low" = calming_como[3]),name = "Reputation:")

#####################################################
d_all_g = d_all[which(d_all$Layer == "Give"),]

graph <- graph_from_data_frame(d_all_g, directed=TRUE, vertices=colnames(Generous))

Standing = colSums(Generous)-colSums(Selfish)

V(graph)$Standing <- as.character(ifelse(Standing>8,"High",ifelse(Standing < -1, "Low","Average")))
V(graph)$indeg <- Standing #igraph::degree(graph, mode = 'out')
V(graph)$layer <- rep("Giving",length(Standing))
V(graph)$site <- rep("Highland",length(Standing))


P3 = ggraph(graph, 'hive', axis = layer, sort.by = indeg, normalize=FALSE,use.numeric=FALSE,split.axes="all",split.angle=1,center.size=1,divide.by=Standing, divide.order=c("Low","Average","High")) + 
    geom_edge_hive(color = "#3e3131", alpha=0.1, strength=1) + 
    geom_axis_hive(aes(color = Standing), size = 3) + 
    coord_fixed() + theme(legend.position="none") + 
    facet_grid(site~., switch="y") + theme(strip.text = element_text(size = X+2), legend.text = element_text(size = X), legend.title = element_text(size = X),panel.background = element_rect(fill = "white"))  +
    scale_colour_manual(values = c( "High" = calming_como[7], "Average" = calming_como[6], "Low" = calming_como[3]),name = "Reputation:", guide = guide_legend(override.aes = list(fill=NA,alpha = 0,color = "white") )) + 
    scale_linetype(guide = guide_legend(override.aes = list(alpha = 0) ) )+
    theme(legend.title = element_text(color = "transparent"),
     legend.text = element_text(color = "transparent")) + theme(legend.background=element_blank(),legend.key = element_rect(fill = NA, color = NA))
        
ggsave("Punish_TB_Hive.png", P1, height=6, width=8)
ggsave("Give_TB_Hive.png", P3, height=6, width=8.5)
ggsave("Exploit_TB_Hive.png", P2, height=6, width=8)        


