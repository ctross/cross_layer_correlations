
################################################################################ Posteriors
calming_como = c("#34261D", "#A96922", "#7D370D", "#1C7262","#114B47","#CF821B","#E4A835")

 df2a = df_q_bs[which(df_q_bs$rs_type %in% c("Between-Person")),]
 df2a$Site="Coastal"

 df2b = df_q_sc[which(df_q_sc$rs_type %in% c("Between-Person")),]
 df2b$Site="Lowland"

 df2c = df_q_tb[which(df_q_tb$rs_type %in% c("Between-Person")),]
 df2c$Site="Highland"

 df2d = df_q_su[which(df_q_su$rs_type %in% c("Between-Person")),]
 df2d$Site="Altiplano"

 df2 = rbind(df2a,df2b,df2c,df2d)

 dodgewidth = 0.5

 p2a = ggplot(df2, aes(y=Group2,  x=r, xmin=l, xmax=h,color=Site)) +
 geom_linerange(position = position_dodge(width = dodgewidth)) +  geom_vline(xintercept = 0,linetype="dashed") +
 geom_point(position = position_dodge(width = dodgewidth)) +  facet_grid(. ~ rs_type  , space="free") + theme(axis.text = element_text(size = 14))  + 
 theme(axis.title = element_text(size = 14))  + ylab("") + xlab("Correlation") + theme(strip.text.x = element_text(size = 14)) +
 scale_colour_manual(values = c("Coastal" = calming_como[6], "Lowland" = calming_como[3], "Highland" = calming_como[4], "Altiplano" = "grey20"))

ggsave("DR_bp.pdf", p2a, height=3.3, width=6)


 df2a = df_q_bs[which(df_q_bs$rs_type %in% c("Within-Person")),]
 df2a$Site="Coastal"

 df2b = df_q_sc[which(df_q_sc$rs_type %in% c("Within-Person")),]
 df2b$Site="Lowland"

 df2c = df_q_tb[which(df_q_tb$rs_type %in% c("Within-Person")),]
 df2c$Site="Highland"

 df2d = df_q_su[which(df_q_su$rs_type %in% c("Within-Person")),]
 df2d$Site="Altiplano"

 df2 = rbind(df2a,df2b,df2c,df2d)

 p2b = ggplot(df2, aes(y=Group2,  x=r, xmin=l, xmax=h,color=Site)) +
 geom_linerange(position = position_dodge(width = dodgewidth)) +  geom_vline(xintercept = 0,linetype="dashed") +
 geom_point(position = position_dodge(width = dodgewidth)) +  facet_grid(. ~ rs_type  , space="free") + theme(axis.text = element_text(size = 14))  + 
 theme(axis.title = element_text(size = 14))  + ylab("") + xlab("Correlation") + theme(strip.text.x = element_text(size = 14)) +
 scale_colour_manual(values = c("Coastal" = calming_como[6], "Lowland" = calming_como[3], "Highland" = calming_como[4], "Altiplano" = "grey20"))

ggsave("DR_wp.pdf", p2b, height=3.3, width=6)


 df2a = df_q_bs[which(df_q_bs$rs_type %in% c("Dyadic Reciprocity")),]
 df2a$Site="Coastal"

 df2b = df_q_sc[which(df_q_sc$rs_type %in% c("Dyadic Reciprocity")),]
 df2b$Site="Lowland"

 df2c = df_q_tb[which(df_q_tb$rs_type %in% c("Dyadic Reciprocity")),]
 df2c$Site="Highland"

 df2d = df_q_su[which(df_q_su$rs_type %in% c("Dyadic Reciprocity")),]
 df2d$Site="Altiplano"


 df2 = rbind(df2a,df2b,df2c,df2d)

 p2c = ggplot(df2, aes(y=Group2,  x=r, xmin=l, xmax=h,color=Site)) +
 geom_linerange(position = position_dodge(width = dodgewidth)) +  geom_vline(xintercept = 0,linetype="dashed") +
 geom_point(position = position_dodge(width = dodgewidth)) +  facet_grid(. ~ rs_type  , space="free") + theme(axis.text = element_text(size = 14))  + 
 theme(axis.title = element_text(size = 14))  + ylab("") + xlab("Correlation") + theme(strip.text.x = element_text(size = 14)) +
 scale_colour_manual(values = c("Coastal" = calming_como[6], "Lowland" = calming_como[3], "Highland" = calming_como[4], "Altiplano" = "grey20"))

ggsave("DR_dr.pdf", p2c, height=3.3, width=6)
