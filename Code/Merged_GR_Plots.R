################################## Focal
calming_como = c("#34261D", "#A96922", "#7D370D", "#1C7262","#114B47","#CF821B","#E4A835")
 dodgewidth = 0.5

 df2_bs_a$Site="Coastal"
 df2_sc_a$Site="Lowland"
 df2_tb_a$Site="Highland"
 df2_su_a$Site="Altiplano"

 df2 = rbind(df2_bs_a, df2_sc_a, df2_tb_a, df2_su_a)

p2a = ggplot(df2, aes(y=Group2,  x=r, xmin=l, xmax=h,color=Site)) +
 geom_linerange(position = position_dodge(width = dodgewidth)) +  geom_vline(xintercept = 0,linetype="dashed") +
 geom_point(position = position_dodge(width = dodgewidth)) +  facet_grid(. ~ Group1  , space="free")  + theme(axis.text = element_text(size = 14))  + 
 theme(axis.title = element_text(size = 14))  + ylab("") + xlab("Correlation") + theme(strip.text.x = element_text(size = 14))+
 scale_colour_manual(values = c("Coastal" = calming_como[6], "Lowland" = calming_como[3], "Highland" = calming_como[4], "Altiplano" = "grey20")) + coord_cartesian(xlim = c(-0.75, 1))
p2a

ggsave("GR_focal.pdf", p2a, height=3.3, width=5)


################################## Alter
 df2_bs_b$Site="Coastal"
 df2_sc_b$Site="Lowland"
 df2_tb_b$Site="Highland"
 df2_su_b$Site="Altiplano"

 df2 = rbind(df2_bs_b, df2_sc_b, df2_tb_b, df2_su_b)

 p2b = ggplot(df2, aes(y=Group2,  x=r, xmin=l, xmax=h ,color=Site)) +
 geom_linerange(position = position_dodge(width = dodgewidth)) +  geom_vline(xintercept = 0,linetype="dashed") +
 geom_point(position = position_dodge(width = dodgewidth)) +  facet_grid(. ~ Group1  , space="free") + theme(axis.text = element_text(size = 14))  + 
 theme(axis.title = element_text(size = 14))  + ylab("") + xlab("Correlation") + theme(strip.text.x = element_text(size = 14))+
 scale_colour_manual(values = c("Coastal" = calming_como[6], "Lowland" = calming_como[3], "Highland" = calming_como[4], "Altiplano" = "grey20")) 
p2b

ggsave("GR_alter.pdf", p2b, height=3.3, width=5)

