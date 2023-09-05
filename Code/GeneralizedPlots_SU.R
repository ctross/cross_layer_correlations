#################################################### GR
rs_m = c(precis(res_su,pars="G_corr",depth=3)[,1])
rs_l = c(precis(res_su,pars="G_corr",depth=3)[,3])
rs_h = c(precis(res_su,pars="G_corr",depth=3)[,4])

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
rs_type[which(!is.na(rs_type))] = "Cross"

rs_type[1,2] = "Sender"
rs_type[1,3] = "Sender"
rs_type[1,4] = "Sender"
rs_type[1,5] = "Sender"
rs_type[2,3] = "Sender"
rs_type[2,4] = "Sender"
rs_type[2,5] = "Sender"
rs_type[3,4] = "Sender"
rs_type[3,5] = "Sender"
rs_type[4,5] = "Sender"

rs_type[5+1,5+2] = "Receiver"
rs_type[5+1,5+3] = "Receiver"
rs_type[5+1,5+4] = "Receiver"
rs_type[5+1,5+5] = "Receiver"
rs_type[5+2,5+3] = "Receiver"
rs_type[5+2,5+4] = "Receiver"
rs_type[5+2,5+5] = "Receiver"
rs_type[5+3,5+4] = "Receiver"
rs_type[5+3,5+5] = "Receiver"
rs_type[5+4,5+5] = "Receiver"

rs_m = c(rs_m)
rs_l = c(rs_l)
rs_h = c(rs_h)

rs_type = c(rs_type) 

names_outcomes = c("Give\n(Sender)", "Exploit\n(Sender)", "Punish\n(Sender)", "Selfish\n(Sender)", "Generous\n(Sender)", 
                   "Give\n(Receiver)", "Exploit\n(Receiver)", "Punish\n(Receiver)", "Selfish\n(Receiver)", "Generous\n(Receiver)")
measure1 = factor(rep(names_outcomes,each=10))
measure2 = factor(rep(names_outcomes,10))

measure1 = factor(measure1,names_outcomes)
measure2 = factor(measure2,rev(names_outcomes))

######### Old
names_outcomesB = c("Give\n(Focal)", "Exploit\n(Focal)", "Punish\n(Focal)", "Selfish\n(Focal)", "Generous\n(Focal)", 
                   "Give\n(Alter)", "Exploit\n(Alter)", "Punish\n(Alter)", "Selfish\n(Alter)", "Generous\n(Alter)")
measure1B = factor(rep(names_outcomesB,each=10))
measure2B = factor(rep(names_outcomesB,10))

measure1B = factor(measure1B,names_outcomesB)
measure2B = factor(measure2B,rev(names_outcomesB))

r_if_sig = ifelse(precis(res_su, pars="G_corr",depth=3)[,3] > 0 | precis(res_su, pars="G_corr",depth=3)[,4] < 0, round(rs_m,2),NA)

df_su = data.frame(r=rs_m, l=rs_l, h=rs_h, measure1=measure1, measure2=measure2, r_if_sig=r_if_sig, rs_type=rs_type, measure1B=measure1B, measure2B=measure2B)



df_su$Group1 = ifelse(substrRight(df_su$measure1,4)=="der)" & substrRight(df_su$measure2,4)=="der)", "Sender",
                   ifelse(substrRight(df_su$measure1,4)=="ver)" & substrRight(df_su$measure2,4)=="ver)", "Receiver",
                   "Cross"
                   ))

df_su$Group2 = paste0(substrLeft(df_su$measure1B,9),"_",substrLeft(df_su$measure2B,9))
df_su$Group3 = substrLeft(df_su$measure1B,9)

calming_como = c("#34261D", "#A96922", "#7D370D", "#1C7262","#114B47","#CF821B","#E4A835")

 p1 = ggplot(df_su, aes(measure1, measure2, fill=r, label=round(r_if_sig,2),color=rs_type)) +
 geom_tile(aes(width=0.96, height=0.96), size=1.69) +
 labs(x = NULL, y = NULL, fill = "Correlation", title="Generalized reciprocity estimates: Altiplano site", subtitle="Only reliable correlation coefficients shown") + 
 scale_fill_gradient2(mid="#FBFEF9",low=calming_como[3],high=calming_como[5], limits=c(-1,1)) +
 geom_text(color="black",size=12*0.36) +
 theme_classic() +
 scale_x_discrete(expand=c(0,0)) +
 scale_y_discrete(expand=c(0,0)) + guides(color = "none") + theme(plot.title = element_text(size = 14))  +
 scale_color_manual(values=c("grey75","grey15","grey15","grey75")) + theme(axis.text = element_text(size = 14))

ggsave("GR_SU.pdf", p1, height=0.6*12, width=12)


############### Prep for merged plots
df2 = df_su[which(df_su$Group1 %in% c("Sender")),]
df2_su_a = df2[which(!is.na(df2$r)),]

df2 = df_su[which(df_su$Group1 %in% c("Receiver")),]
df2_su_b = df2[which(!is.na(df2$r)),]

 