#cut from PCA. This is just the black and white version of the PCA plot.

##Black and White Plots
pb<-ggplot(data = s1, aes(Comp.1, Comp.2, shape = Type))+
  geom_point(size = 3.1)+
  scale_shape_manual(values = c(21, 19))+
  theme_bw(base_size = 24)+
  theme(panel.grid = element_blank(), panel.border = element_rect(fill= "transparent"))+
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)+
  xlim(-6,6)+
  ylim(-4.5,4.5)+
  stat_ellipse(type = "norm", aes(color = Type), 
               show.legend = FALSE,size = 1, level = 0.95, linetype = 1)+
  scale_color_manual(values=c("#969696", "#252525"))+
  annotate("label",  x = 3.5, y = 2.7, label = "Cottonmouth", color ="#252525", size = 9)+
  annotate("label",  x = 3, y = -3.5, label = "Habitat", color ="#969696", size = 9)+
  xlab(expression(atop("aquatic sites" %<->% "terrestrial sites", paste("PC-1 (22.6%)" ))))+
  ylab(expression(atop ("PC-2 (11.8%)", paste("low cover " %<->% "high cover"))))+
  theme(legend.position="none", legend.background = element_rect(fill="White",size=0.5),
        axis.title.x = element_text(size = 26),
        axis.title.y = element_text(size = 26),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"))

#Adding Densities
xdensb<-axis_canvas(pb, axis = "x")+
  geom_density(data =group, aes(Comp.1, color = group$Group, fill = group$Group, linetype=group$Group), alpha = 0.95, size =1)+
  scale_fill_manual(values = c("black", "#636363","white","#d9d9d9"))+
  scale_color_manual(values = c("black", "black","black","black"))+
  scale_linetype_manual(values = c("blank", "dashed", "solid", "dotted"))

ydensb<-axis_canvas(pb, axis = "y", coord_flip = TRUE)+
  geom_density(data =group, aes(Comp.2, color = group$Group, fill = group$Group, linetype=group$Group), alpha = 0.83, size =1)+
  scale_fill_manual(values = c("black", "#636363","white","#d9d9d9"))+
  scale_color_manual(values = c("black", "black","black","black"))+
  scale_linetype_manual(values = c("blank", "dashed", "solid", "dotted"))+
  coord_flip()+
  annotate("label", x = -1.9, y = 0.15, label = "FEMALE", color ="#252525", size = 3.2)+
  annotate("label", x = 2.5, y = 0.15, label = "JUVENILE", color ="#252525", size = 3.2, alpha = 0.28)+
  annotate("label", x = 1.2, y = 0.26, label = "MALE", color ="#252525", size = 3.2)+
  annotate("label", x = 3.3, y = 0.17, label = "NEONATE", color ="#252525",  size = 3.2)
pb1<-insert_xaxis_grob(pb, xdensb, grid::unit(0.2, "null"), position= "top")
pb2<-insert_yaxis_grob(pb1,ydensb, grid::unit(0.2, "null"), position = "right")

twogroupb<-ggdraw(pb2)

#Intraspecific Group
gg_scatter1b<-ggplot(data = s1, aes(Comp.1, Comp.2, shape = Type))+
  scale_shape_manual(values = c(15,21,19,24,18, 19))+
  theme_bw(base_size = 24)+
  theme(panel.grid = element_blank(), panel.border = element_rect(fill= "transparent"))+
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)+
  xlim(-6,6)+
  ylim(-4.5,4.5)+
  stat_ellipse(type = "norm", aes(color = Type), 
               show.legend = FALSE,size = 1, level = 0.95, linetype = 1)+
  scale_color_manual(values=c("black","#969696","#636363","black","#d9d9d9","#252525"))+
  annotate("label",  x = 3.2, y = 2.7, label = "Cottonmouth", color ="black", size = 9)+
  annotate("label",  x = 3, y = -3.5, label = "Habitat", color ="grey44", size = 9)+
  xlab(expression(atop("aquatic sites" %<->% "terrestrial sites", paste("PC-1 (22.6%)" ))))+
  ylab(expression(atop ("PC-2 (11.8%)", paste("low cover " %<->% "high cover"))))

ppb<-gg_scatter1b+
  geom_point(data =pcomp, aes(pcomp$Comp.1, pcomp$Comp.2, shape = Group, color = Group, fill = Group), size = 8, alpha= 1)+
  scale_fill_manual(values = c("black","#969696","white","black","#d9d9d9","#252525"))+
  annotate("label", x = 0.8, y = 0.5, label = "FEMALE", color ="#252525", size = 6)+
  annotate("label", x = -1.5, y = 1.2, label = "JUVENILE", color ="#252525", size = 6)+
  annotate("label", x = -0.7, y = -0.15, label = "MALE", color ="#252525", size = 6)+
  annotate("label", x = 0.8, y = 1.7, label = "NEONATE", color ="#252525",  size = 6)+
  theme(legend.position="none", legend.background = element_rect(fill="White",size=0.5),
        axis.title.x = element_text(size = 26),
        axis.title.y = element_text(size = 26),
        axis.text.x = element_text(size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"))

#Adding Densities for Intraspecific Groups
xdensb1<-axis_canvas(ppb, axis = "x")+
  geom_density(data =group, aes(Comp.1, color = group$Group, fill = group$Group, linetype=group$Group), alpha = 0.95, size =1)+
  scale_fill_manual(values = c("black", "#636363","white","#d9d9d9"))+
  scale_color_manual(values = c("black", "black","black","black"))+
  scale_linetype_manual(values = c("blank", "dashed", "solid", "dotted"))

ydensb1<-axis_canvas(ppb, axis = "y", coord_flip = TRUE)+
  geom_density(data =group, aes(Comp.2, color = group$Group, fill = group$Group, linetype=group$Group), alpha = 0.83, size =1)+
  scale_fill_manual(values = c("black", "#636363","white","#d9d9d9"))+
  scale_color_manual(values = c("black", "black","black","black"))+
  scale_linetype_manual(values = c("blank", "dashed", "solid", "dotted"))+
  coord_flip()+
  annotate("label", x = -1.9, y = 0.15, label = "FEMALE", color ="#252525", size = 3.2)+
  annotate("label", x = 2.5, y = 0.15, label = "JUVENILE", color ="#252525", size = 3.2, alpha = 0.28)+
  annotate("label", x = 1.2, y = 0.26, label = "MALE", color ="#252525", size = 3.2)+
  annotate("label", x = 3.3, y = 0.17, label = "NEONATE", color ="#252525",  size = 3.2)
ppb1<-insert_xaxis_grob(ppb, xdensb1, grid::unit(0.2, "null"), position= "top")
ppb2<-insert_yaxis_grob(ppb1,ydensb1, grid::unit(0.2, "null"), position = "right")


fourgroupb<-ggdraw(ppb2)

bw<-plot_grid(twogroupb,fourgroupb, nrow = 1, align = "h")