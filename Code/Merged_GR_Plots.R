################################## Focal
calming_como = c("#34261D", "#A96922", "#7D370D", "#1C7262","#114B47","#CF821B","#E4A835")
 dodgewidth = 0.6

 df2_bs_a$Site="Coastal"
 df2_sc_a$Site="Lowland"

 df2 = rbind(df2_bs_a, df2_sc_a)

p2a = ggplot(df2, aes(y=Group2,  x=r, xmin=l, xmax=h,color=Site)) +
 geom_linerange(position = position_dodge(width = dodgewidth)) +  geom_vline(xintercept = 0,linetype="dashed") +
 geom_point(position = position_dodge(width = dodgewidth)) +  facet_grid(. ~ Group1  , space="free")  + theme(axis.text = element_text(size = 14))  + 
 theme(axis.title = element_text(size = 14))  + ylab("") + xlab("Correlation") + theme(strip.text.x = element_text(size = 14))+
 scale_colour_manual(values = c("Coastal" = calming_como[6], "Lowland" = calming_como[3])) + coord_cartesian(xlim = c(-0.75, 1))
p2a

ggsave("GR_focal.pdf", p2a, height=3, width=5)


################################## Alter
 df2_bs_b$Site="Coastal"
 df2_sc_b$Site="Lowland"

 df2 = rbind(df2_bs_b, df2_sc_b)

 p2b = ggplot(df2, aes(y=Group2,  x=r, xmin=l, xmax=h ,color=Site)) +
 geom_linerange(position = position_dodge(width = dodgewidth)) +  geom_vline(xintercept = 0,linetype="dashed") +
 geom_point(position = position_dodge(width = dodgewidth)) +  facet_grid(. ~ Group1  , space="free") + theme(axis.text = element_text(size = 14))  + 
 theme(axis.title = element_text(size = 14))  + ylab("") + xlab("Correlation") + theme(strip.text.x = element_text(size = 14))+
 scale_colour_manual(values = c("Coastal" = calming_como[6], "Lowland" = calming_como[3])) 
p2b

ggsave("GR_alter.pdf", p2b, height=3, width=5)

