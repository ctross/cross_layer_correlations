########################################## DR
rs_m = c(precis(res_bs,pars="D_corr",depth=3)[,1])
rs_l = c(precis(res_bs,pars="D_corr",depth=3)[,3])
rs_h = c(precis(res_bs,pars="D_corr",depth=3)[,4])

rs_m = matrix(rs_m,nrow=10,ncol=10)
rs_l = matrix(rs_l,nrow=10,ncol=10)
rs_h = matrix(rs_h,nrow=10,ncol=10)

rs_m[lower.tri(rs_m)] = NA
diag(rs_m) = NA

rs_l[lower.tri(rs_l)] = NA
diag(rs_l) = NA

rs_h[lower.tri(rs_h)] = NA
diag(rs_h) = NA


rs_type = rs_m
rs_type[which(!is.na(rs_type))] = "Blank"

rs_m[6,7] = NA
rs_m[6,8] = NA
rs_m[6,9] = NA
rs_m[6,10] = NA

rs_m[7,8] = NA
rs_m[7,9] = NA
rs_m[7,10] = NA

rs_m[8,9] = NA
rs_m[8,10] = NA

rs_m[9,10] = NA

rs_m[2,6] = NA

rs_m[3,6] = NA
rs_m[3,7] = NA

rs_m[4,6] = NA
rs_m[4,7] = NA
rs_m[4,8] = NA

rs_m[5,6] = NA
rs_m[5,7] = NA
rs_m[5,8] = NA
rs_m[5,9] = NA

diag(rs_m) = NA

rs_type[1,2] = "Within-Person"
rs_type[1,3] = "Within-Person"
rs_type[1,4] = "Within-Person"
rs_type[1,5] = "Within-Person"
rs_type[2,3] = "Within-Person"
rs_type[2,4] = "Within-Person"
rs_type[2,5] = "Within-Person"
rs_type[3,4] = "Within-Person"
rs_type[3,5] = "Within-Person"
rs_type[4,5] = "Within-Person"

rs_type[1,5+2] = "Between-Person"
rs_type[1,5+3] = "Between-Person"
rs_type[1,5+4] = "Between-Person"
rs_type[1,5+5] = "Between-Person"
rs_type[2,5+3] = "Between-Person"
rs_type[2,5+4] = "Between-Person"
rs_type[2,5+5] = "Between-Person"
rs_type[3,5+4] = "Between-Person"
rs_type[3,5+5] = "Between-Person"
rs_type[4,5+5] = "Between-Person"

rs_type[1,6] = "Dyadic Reciprocity"
rs_type[2,7] = "Dyadic Reciprocity"
rs_type[3,8] = "Dyadic Reciprocity"
rs_type[4,9] = "Dyadic Reciprocity"
rs_type[5,10] = "Dyadic Reciprocity"

rs = c(rs_m)
rs_type = c(rs_type) 

names_outcomes = c("Give(j,k)", "Exploit(j,k)", "Punish(j,k)", "Selfish(j,k)", "Generous(j,k)", 
                   "Give(k,j)", "Exploit(k,j)", "Punish(k,j)", "Selfish(k,j)", "Generous(k,j)")
measure1 = factor(rep(names_outcomes,each=10))
measure2 = factor(rep(names_outcomes,10))
                                               
measure1 = factor(measure1,names_outcomes)
measure2 = factor(measure2,rev(names_outcomes))

names_outcomesb = c("Give\n(j to k)", "Exploit\n(j to k)", "Punish\n(j to k)", "Selfish\n(j to k)", "Generous\n(j to k)", 
                   "Give\n(k to j)", "Exploit\n(k to j)", "Punish\n(k to j)", "Selfish\n(k to j)", "Generous\n(k to j)")
measure1b = factor(rep(names_outcomesb,each=10))
measure2b = factor(rep(names_outcomesb,10))
                                               
measure1b = factor(measure1b,names_outcomesb)
measure2b = factor(measure2b,rev(names_outcomesb))

r_if_sig = ifelse(precis(res_bs,pars="D_corr",depth=3)[,3] > 0 | precis(res_bs,pars="D_corr",depth=3)[,4] < 0, round(rs,2),NA)

df_bs = data.frame(r=c(rs_m), l=c(rs_l), h=c(rs_h), measure1=measure1b, measure2=measure2b, r_if_sig=r_if_sig, rs_type=rs_type)

calming_como = c("#34261D", "#A96922", "#7D370D", "#1C7262","#114B47","#CF821B","#E4A835")

p1 =  ggplot(df_bs, aes(measure1, measure2, fill=r, color=rs_type, label=round(r_if_sig,2))) +
 geom_tile(aes(width=0.96, height=0.96), size=1.5) +
 labs(x = NULL, y = NULL, fill = "Correlation", title="Dyadic reciprocity estimates: Coastal site", subtitle="Only reliable correlation coefficients shown") + 
 scale_fill_gradient2(mid="#FBFEF9",low=calming_como[3],high=calming_como[5], limits=c(-1,1)) +
 geom_text(color="black",size=12*0.36) +
 theme_classic() +
 scale_x_discrete(expand=c(0,0)) +
 scale_y_discrete(expand=c(0,0)) + guides(color = "none") + theme(plot.title = element_text(size = 14))  +
 scale_color_manual(values=c("grey75","grey43","grey15","grey75")) + theme(axis.text = element_text(size = 14))

ggsave("DR_BS.pdf", p1, height=6, width=10)

# Prep for CI figure
substrRight = function(x, n){
  x = as.character(x)
  substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft = function(x, n){
  x = as.character(x)
  substr(x, 1, nchar(x)-n+1)
}

df_q_bs = data.frame(r=c(rs_m), l=c(rs_l), h=c(rs_h), measure1=measure1, measure2=measure2, r_if_sig=r_if_sig, rs_type=rs_type)

df_q_bs$Group2 = paste0(substrLeft(df_q_bs$measure2,1),"_",substrLeft(df_q_bs$measure1,1))
df_q_bs$Group3 = substrLeft(df_q_bs$measure1,5)


 

