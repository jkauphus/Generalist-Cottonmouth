#Script to create PCA Plots

# Cottonmouth and random sites Dataset;
study<-read.csv("data/Cottonmouth-Datasheet.csv")
glimpse(study)
s<-study[,c(3:24)]
s<-na.omit(s)
glimpse(s)

#Averaged surface and ambient temperature variables
Temp<-(s$Surface.Temp+s$Ambient.Temp)/2
s<-cbind(s,Temp)

#Removed correlated variables
s<-s[,c(1,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23)]
glimpse(s)

#Multivariate Analyses (PCA, PERMANOVA, PERMDISP)
s1<-s[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
glimpse(s1)
pc<-princomp(s1[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19)], cor = T, scores = T)

#Setting- Up ANOVA
comp<-pc$scores[,1:3]
s1<-cbind(s1, comp)
#Snake Averages
group<- s1 %>% subset(Type == "SNAKE")

pcomp<-group_by(group, Group)%>%
  summarise(Comp.1 = mean(Comp.1),
            Comp.2 = mean(Comp.2),
            Comp.3 = mean(Comp.3))

##Colored Plots 
p<-ggplot(data = s1, aes(Comp.1, Comp.2, shape = Type))+
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
  annotate("label",  x = 3.2, y = 2.7, label = "Cottonmouth", color ="#252525", size = 9)+
  annotate("label",  x = 3, y = -3.5, label = "Habitat", color ="#969696", size = 9)+
  xlab(expression(atop("aquatic sites" %<->% "terrestrial sites", paste("PC-1 (22.6%)" ))))+
  ylab(expression(atop ("PC-2 (11.8%)", paste("low cover " %<->% "high cover"))))+
  theme(legend.position="none", legend.background = element_rect(fill="White",
                                                                 size=0.5))
#Adding Densities
xdens<-axis_canvas(p, axis = "x")+
  geom_density(data =group, aes(Comp.1, color = group$Group, fill = group$Group), alpha = 0.15, size =1, linetype= "dashed")+
  scale_fill_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  scale_color_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))

ydens<-axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data =group, aes(Comp.2, color = group$Group, fill = group$Group), alpha = 0.15, size =1, linetype= "dashed")+
  scale_fill_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  scale_color_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  coord_flip()

p1<-insert_xaxis_grob(p, xdens, grid::unit(0.2, "null"), position= "top")
p2<-insert_yaxis_grob(p1,ydens, grid::unit(0.2, "null"), position = "right")

twogroup<-ggdraw(p2)

#Colored Intraspecific Group
gg_scatter1<-ggplot(data = s1, aes(Comp.1, Comp.2, shape = Type))+
  scale_shape_manual(values = c(15,21,16,17,18, 19))+
  theme_bw(base_size = 24)+
  theme(panel.grid = element_blank(), panel.border = element_rect(fill= "transparent"))+
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)+
  xlim(-6,6)+
  ylim(-4.5,4.5)+
  stat_ellipse(type = "norm", aes(color = Type), 
               show.legend = FALSE,size = 1, level = 0.95, linetype = 1)+
  scale_color_manual(values=c("#F8766D","#969696","#7CAE00","#00BFC4", "#C77CFF", "#252525"))+
  annotate("label",  x = 3.2, y = 2.7, label = "Cottonmouth", color ="#252525", size = 9)+
  annotate("label",  x = 3, y = -3.5, label = "Habitat", color ="#969696", size = 9)+
  xlab(expression(atop("aquatic sites" %<->% "terrestrial sites", paste("PC-1 (22.6%)" ))))+
  ylab(expression(atop ("PC-2 (11.8%)", paste("low cover " %<->% "high cover"))))+
  theme(legend.position="none", legend.background = element_rect(fill="White",
                                                                 size=0.5))
pp<-gg_scatter1+
  geom_point(data =pcomp, aes(pcomp$Comp.1, pcomp$Comp.2, shape = Group, color = Group), size = 7, alpha= 0.8)+
  annotate("label", x = 0.5, y = 0.5, label = "FEMALE", color ="#F8766D")+
  annotate("label", x = -1.1, y = 1.2, label = "JUVENILE", color ="#7CAE00")+
  annotate("label", x = -0.5, y = -0.1, label = "MALE", color ="#00BFC4")+
  annotate("label", x = 0.15, y = 1.65, label = "NEONATE", color ="#C77CFF")

#Adding Densities for Intraspecific Groups
xdens1<-axis_canvas(pp, axis = "x")+
  geom_density(data =group, aes(Comp.1, color = group$Group, fill = group$Group), alpha = 0.3, size =1, linetype= "dashed")+
  scale_fill_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  scale_color_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))

ydens1<-axis_canvas(pp, axis = "y", coord_flip = TRUE)+
  geom_density(data =group, aes(Comp.2, color = group$Group, fill = group$Group), alpha = 0.3, size =1, linetype= "dashed")+
  scale_fill_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  scale_color_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  coord_flip()

p11<-insert_xaxis_grob(pp, xdens1, grid::unit(0.2, "null"), position= "top")
p22<-insert_yaxis_grob(p11,ydens1, grid::unit(0.2, "null"), position = "right")

fourgroup<-ggdraw(p22)

colored<-plot_grid(twogroup,fourgroup, nrow = 1, align = "h")

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
  theme(legend.position="none", legend.background = element_rect(fill="White",size=0.5))

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
  annotate("label", x = 0.6, y = 0.5, label = "FEMALE", color ="#252525", size = 4.5)+
  annotate("label", x = -1.2, y = 1.1, label = "JUVENILE", color ="#252525", size = 4.5)+
  annotate("label", x = -0.5, y = -0.1, label = "MALE", color ="#252525", size = 4.5)+
  annotate("label", x = 0.15, y = 1.65, label = "NEONATE", color ="#252525",  size = 4.5)+
  theme(legend.position="none", legend.background = element_rect(fill="White",size=0.5))

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
bw
