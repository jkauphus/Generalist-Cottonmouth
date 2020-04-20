# The script for the analyses for the research titled "Generalist habitat selection by cottonmouths (Agkistrodon piscivorus) in a
# hydrologically-dynamic wetland

#Libraries used
library(dplyr)
library(ggplot2)
library(cowplot)
library(vegan)
library(randomForest)
library(randomForestExplainer)

# Cottonmouth and random sites Dataset;
study<-read.csv("data/Cottonmouth-Datasheet.csv")
glimpse(study)
s<-study[,c(3:24)]
s<-na.omit(s)
glimpse(s)

#Correlation Check
corrrel<-cor(s[,c(2:20)])
findCorrelation(corrrel, cutoff = 0.7)

#Averaged surface and ambient temperature variables
Temp<-(s$Surface.Temp+s$Ambient.Temp)/2
s<-cbind(s,Temp)

#Removed correlated variables
s<-s[,c(1,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23)]
glimpse(s)

#Hydrologic analysis from the USGS Hydrologic data of Sycamore Creek and Cumberland River under the Cheatham Dam
gauge<-read.csv("data/GaugeData-USGS.csv")
gauge[,2]<-as.Date(gauge[,2])
gauge[,4]<-gauge[,3]*0.3048

p1<-ggplot(gauge, aes(x =gauge$datetime, y= gauge$V4))+
  geom_line(color = "blue")+
  xlab("")+
  ylab("Gauge Height (m)")+
  ggtitle("USGS 03435000 Cumberland River Below Cheatham Dam, TN")+
  theme_classic(base_size = 14)

p1<-p1+scale_x_date(breaks=date_breaks("6 months"), labels = date_format("%b %y"))

discharge<-read.csv("data/SycamoreData-USGS.csv")
discharge[,2]<-as.Date(discharge[,2])
discharge[,4]<-discharge[,3]*0.0283168

p2<-ggplot(discharge, aes(x =discharge$datetime, y= discharge$V4))+
  geom_line(color = "steelblue3")+
  xlab("")+
  ylab("Discharge (cubic meters per second)")+
  ggtitle("USGS 03431800 Sycamore Creek Near Ashland City, TN")+
  theme_classic(base_size = 14)

p2<-p2+scale_x_date(breaks=date_breaks("6 months"), labels = date_format("%b %y"))

plot_grid(p1,p2, ncol = 1, nrow = 2)

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
#Plotting PCA
gg_scatter1<-ggplot(data = s1, aes(Comp.1, Comp.2, color = Type))+
  geom_point(size = 3.1)+
  scale_color_manual(values = c("#F8766D","#252525","#7CAE00","#00BFC4", "#C77CFF","#969696"))+
  theme_bw(base_size = 24)+
  theme(panel.grid = element_blank(), panel.border = element_rect(fill= "transparent"))+
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2)+
  xlim(-6,6)+
  ylim(-4.5,4.5)+
  stat_ellipse(type = "norm", aes(fill = Type), 
               show.legend = FALSE,size = 1, level = 0.95, linetype = 2)+
  scale_fill_manual(values=c("#252525", "#969696"))+
  geom_text(aes(4.3,2.7), label = paste("Cottonmouth"), col ="#969696", size = 9)+
  geom_text(aes(3,-3.7), label = paste("Habitat"), col = "#252525", size = 9)+
  xlab(expression(atop("aquatic sites" %<->% "terrestrial sites", paste("PC-1 (22.6%)" ))))+
  ylab(expression(atop ("PC-2 (11.8%)", paste("low cover " %<->% "high cover"))))+
  theme(legend.position="none", legend.background = element_rect(fill="White",
                                                                 size=0.5))

p<-gg_scatter1+ geom_point(data = pcomp, aes(pcomp$Comp.1, pcomp$Comp.2, shape = Group, color =Group), size = 7, alpha= 0.8)+
  scale_shape_manual(values = c(15,16,17,18))+
  annotate("label", x = 0.5, y = 0.5, label = "FEMALE", color ="#F8766D")+
  annotate("label", x = -1.1, y = 1.2, label = "JUVENILE", color ="#7CAE00")+
  annotate("label", x = -0.5, y = -0.1, label = "MALE", color ="#00BFC4")+
  annotate("label", x = 0.15, y = 1.65, label = "NEONATE", color ="#C77CFF")

xdens<-axis_canvas(p, axis = "x")+
  geom_density(data =group, aes(Comp.1, color = group$Group, fill = group$Group), alpha = 0.3, size =1, linetype= "dashed")+
  scale_fill_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  scale_color_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))

ydens<-axis_canvas(p, axis = "y", coord_flip = TRUE)+
  geom_density(data =group, aes(Comp.2, color = group$Group, fill = group$Group), alpha = 0.3, size =1, linetype= "dashed")+
  scale_fill_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  scale_color_manual(values = c("#F8766D","#7CAE00","#00BFC4", "#C77CFF"))+
  coord_flip()

p1<-insert_xaxis_grob(p, xdens, grid::unit(0.2, "null"), position= "top")
p2<-insert_yaxis_grob(p1,ydens, grid::unit(0.2, "null"), position = "right")

ggdraw(p2)

