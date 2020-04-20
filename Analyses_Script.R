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

#Creating PCA Loadings Table
Description<- c("Percentage of canopy above quadrat", "Coverage within quadrat","Coverage within quadrat","Coverage within quadrat","Coverage within quadrat", "Height of largest living woody stem", "Height of cover object closest to snake point", "Distance to nearest cover object", "Diameter of largest log within quadrat", "Distance to nearest water source", "Water depth measured within quadrat", "Coverage within quadrat","Coverage within quadrat","Coverage within quadrat", "Coverage within quadrat","Mean temperature within quadrat", "", "")

loadings<-pc$loadings[,1:3]
sdev<-t(data.frame(pc$sdev))
eigen<-sdev[,1:3]
Variance<-c(22.60, 11.81, 9.27)
loadings<-rbind(loadings, eigen, Variance)
loadings<-round(loadings, digits = 3)
names(loadings)<-c("PC-1", "PC-2", "PC-3")
loadings<-round(loadings, digits = 3)
loadings<-cbind(Description, loadings)

row.names(loadings)<-c("Canopy Closure (%)", "Rock Cover (%)", "Leaf Cover (%)", "Vegetative Cover (%)", "Fallen Log Cover (%)","Woody Stem Height (cm)", "Cover Height (cm)", "Distance To Cover (cm)","Log Diameter (cm)", "Distance To Water (cm)", "Water Depth (cm)", "Percent Water (%)", "Percent Woody Debris (%)", "Percent Floating Vegetation (%)", "Percent Bare Soil (%)", "Mean Temperature (C)", "Eigenvalue", "Explained Variance")

gt(loadings, rownames_to_stub = TRUE, )%>%
  tab_stubhead(label = "Variable (units)")%>%
  cols_label(
    Description = "Description",
    Comp.1 = "PC-1",
    Comp.2 = "PC-2",
    Comp.3 = "PC-3")%>%
  tab_row_group( group = "",
                 rows = 1:16
  )%>%
  tab_row_group(
    group = "PCA Statistics",rows = c(17,18))
#ANOVA for Conspecific Differences
pcomp<-pcomp[,2:4]
names(pcomp)<- c( "PC-1","PC-2", "PC-3")
row.names(pcomp)<-c("Female", "Juvenile", "Male", "YOY")
pcomp[,1:3]<-round(pcomp[,1:3], digits = 3)

C1<-aov(Comp.1~Group, group)
C2<-aov(Comp.2~Group, group)
C3<-aov(Comp.3~Group, group)

F.value<-c(anova(C1)$"F value"[1], anova(C2)$"F value"[1], anova(C3)$"F value"[1])
p.value<-c(anova(C1)$"Pr(>F)"[1],anova(C2)$"Pr(>F)"[1],anova(C3)$"Pr(>F)"[1])

stats<-data.frame(t(cbind(F.value, p.value)))
names(stats)<-c( "PC-1","PC-2", "PC-3")
pcomp<-rbind(pcomp,stats)

row.names(pcomp)<-c("Female", "Juvenile", "Male", "YOY", "F", "P-value")
pcomp<-round(pcomp, digits = 3)

gt(pcomp, rownames_to_stub = TRUE)%>%
  tab_stubhead(label = "Cottonmouth conspecific")%>%
  tab_row_group( group = "",
                 rows = 1:4
  )%>%
  tab_row_group(
    group = "ANOVA",rows = c(5,6))
#Tukey HDS for PCs
#PC-1
TukeyHSD(C1)
#PC-2
TukeyHSD(C2)
#PC-3
TukeyHSD(C3)

#Linear Regression for Ontegenetic Shift
mod<-lm(SVL~Comp.1,group)
mod2<-lm(SVL~Comp.2,group)
mod3<-lm(SVL~Comp.3,group)

LR1<-ggplot(group, aes(x= SVL, y= Comp.1))+
  geom_point(size = 2)+
  xlim(209, 1061)+
  geom_smooth(method = "lm", color= "black")+
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 600, lty = 2)+
  theme_bw(base_size = 24)+
  #geom_text(aes(900, 6, label = paste("R-squared = 0.034")), color="black")+
  geom_text(aes(900, 2.95, label = paste("P= 0.023*")), color = "black", size=8)+
  xlab("Snout-vent length (mm)")+
  ylab(expression(atop("aquatic site" %<->% "terrestrial sites", paste("PC-1 (22.6%)" ))))+
  ylim(-3,3)+
  guides(shape = "none")+
  theme(panel.grid = element_blank(), panel.border = element_rect(fill= "transparent"))

LR2<-ggplot(group, aes(x= SVL, y= Comp.2))+
  geom_point(size = 2)+
  xlim(209, 1061)+
  geom_smooth(method = "lm", color= "black")+
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 600, lty = 2)+
  theme_bw(base_size = 24)+
  #geom_text(aes(900, 6, label = paste("R-squared = 0.045")), color="black")+
  geom_text(aes(900, 2.95, label = paste("P= 0.008*")), color = "black", size = 8)+
  xlab("Snout-vent length (mm)")+
  ylab(expression(atop("low cover" %<->% "high cover", paste("PC-2 (11.8%)" ))))+
  ylim(-3,3)+
  guides(shape = "none")+
  theme(panel.grid = element_blank(), panel.border = element_rect(fill= "transparent"))

LR3<-ggplot(group, aes(x= SVL, y= Comp.3))+
  geom_point(size = 2)+
  xlim(209, 1061)+
  geom_smooth(method = "lm", color= "black")+
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 600, lty = 2)+
  theme_bw(base_size = 24)+
  #geom_text(aes(900, 6, label = paste("R-squared = 0.045")), color="black")+
  geom_text(aes(900, 2.95, label = paste("P= 0.49")), color = "black", size = 8)+
  xlab("Snout-vent length (mm)")+
  ylab(expression(atop("low vegetation" %<->% "high vegetation", paste("PC-3 (9.2%)" ))))+
  ylim(-3,3)+
  guides(shape = "none")+
  theme(panel.grid = element_blank(), panel.border = element_rect(fill= "transparent"))

plot_grid(LR1, LR2, LR3, labels=c("A)", "B)", "C)"), ncol = 3, nrow = 1)
#PC-1
summary(mod)
#PC-2
summary(mod2)
#PC-3
summary(mod3)

#Random Forest Modeling

#MIR Ratio for feature reduction
rf<-s1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22)]
set.seed(111)
#Model Improvement Ratio by Cutler et al. 2010
#Train & Test
set.seed(123)
ind<-sample(2,nrow(rf),replace = T, prob = c(0.75,0.25))
train<-rf[ind==1,]
test<-rf[ind==2,]

#Random Forest
RF<-randomForest(Type~.,train, ntree= 6000, importance= T)
RF

#OOB = 16.12; class = 0.151/0.1727

# MIR > 0
A<-data.frame(randomForest::importance(RF))
A
A<-data.frame(A[,2])
sum<-sum(A)
A<-A/sum
A
#Remove 8 Cover Height

#MIR > 0.02
rf<-s[,c(1,2,3,4,5,6,7,9,10,11,12,13,14,15,16,19)]

#Train & Test
set.seed(123)
ind<-sample(2,nrow(rf),replace = T, prob = c(0.75,0.25))
train<-rf[ind==1,]
test<-rf[ind==2,]

#Random Forest
RF<-randomForest(Type~.,train, ntree= 6000, importance= T)
RF

#OOB = 16.94, class= 0.151/0.190
A<-data.frame(randomForest::importance(RF))
A
A<-data.frame(A[,2])
sum<-sum(A)
A<-A/sum
A
# Remove Canopy Cover 1

#MIR > 0.04

rf<-s[,c(1,3,4,5,6,7,9,10,11,12,13,14,15,16,19)]

set.seed(123)
ind<-sample(2,nrow(rf),replace = T, prob = c(0.75,0.25))
train<-rf[ind==1,]
test<-rf[ind==2,]

#Random Forest
RF<-randomForest(Type~.,train, ntree= 6000, importance= T)
RF

#OOB = 17.36, class= 0.151/0.20
A<-data.frame(randomForest::importance(RF))
A
A<-data.frame(A[,2])
sum<-sum(A)
A<-A/sum
A

#Remove 5 WSH, 6 dist, 7 log diam, 12 floating veg

#MIR > 0.06
rf<-s[,c(1,3,4,5,6,11,12,13,14,16,19)]

set.seed(123)
ind<-sample(2,nrow(rf),replace = T, prob = c(0.75,0.25))
train<-rf[ind==1,]
test<-rf[ind==2,]

#Random Forest
RF<-randomForest(Type~.,train, ntree= 6000, importance= T)
RF

#OOB = 18.18, class= 0.189/0.172
A<-data.frame(randomForest::importance(RF))
A
A<-data.frame(A[,2])
sum<-sum(A)
A<-A/sum
A
# Remove Veg, Depth, & Woody Debris

#MIR > 0.08
rf<-s[,c(1,3,5,6,11,13,16,19)]

set.seed(123)
ind<-sample(2,nrow(rf),replace = T, prob = c(0.75,0.25))
train<-rf[ind==1,]
test<-rf[ind==2,]

#Random Forest
RF<-randomForest(Type~.,train, ntree= 6000, importance= T)
RF

#OOB = 21.07, class= 0.181/0.245
A<-data.frame(randomForest::importance(RF))
A
A<-data.frame(A[,2])
sum<-sum(A)
A<-A/sum
A

#Removed Veg, and Fallen Log Cover

#MIR = 0.1
rf<-s[,c(1,3,11,13,16,19)]

set.seed(123)
ind<-sample(2,nrow(rf),replace = T, prob = c(0.75,0.25))
train<-rf[ind==1,]
test<-rf[ind==2,]

#Random Forest
RF<-randomForest(Type~.,train, ntree= 6000, importance= T)
RF

#OOB = 22.31, class= 0.242/0.20
A<-data.frame(randomForest::importance(RF))
A
A<-data.frame(A[,2])
sum<-sum(A)
A<-A/sum
A

#Went with the Full Model
rf<-s1[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,19)]
head(rf)
#Setting up Cross Validation Training and Validation sets
set.seed(123)
ind<-sample(2,nrow(rf),replace = T, prob = c(0.9,0.1))
train<-rf[ind==1,]
test<-rf[ind==2,]

#30-iterations of RF Models
set.seed(123)
a1<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a2<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a3<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a4<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a5<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a6<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a7<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a8<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a9<- randomForest(Type~., data = train, ntree = 6000, importance = T)
a10<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b1<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b2<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b3<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b4<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b5<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b6<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b7<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b8<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b9<- randomForest(Type~., data = train, ntree = 6000, importance = T)
b10<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c1<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c2<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c3<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c4<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c5<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c6<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c7<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c8<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c9<- randomForest(Type~., data = train, ntree = 6000, importance = T)
c10<- randomForest(Type~., data = train, ntree = 6000, importance = T)

#Error rate of a Random Forest model
plot(a1)

#Out of Bag Error rates for 30 models to calculate mean, min, and max
OOB1<-c(a1$err.rate[6000,1],a2$err.rate[6000,1],a3$err.rate[6000,1],a4$err.rate[6000,1],a5$err.rate[6000,1],a6$err.rate[6000,1],
        a7$err.rate[6000,1],a8$err.rate[6000,1],a9$err.rate[6000,1],a10$err.rate[6000,1],b1$err.rate[6000,1],b2$err.rate[6000,1],
        b3$err.rate[6000,1],b4$err.rate[6000,1],b5$err.rate[6000,1],b6$err.rate[6000,1],b7$err.rate[6000,1],b8$err.rate[6000,1],
        b9$err.rate[6000,1],b10$err.rate[6000,1],c1$err.rate[6000,1],c2$err.rate[6000,1],c3$err.rate[6000,1],c4$err.rate[6000,1],
        c5$err.rate[6000,1],c6$err.rate[6000,1],c7$err.rate[6000,1],c8$err.rate[6000,1],c9$err.rate[6000,1],c10$err.rate[6000,1])
OOB<-c(1-min(OOB1),1-mean(OOB1), 1-max(OOB1))
OOB1

#10-fold Cross Validation metrics for 30 models to calculate mean, min, and max
files<-list(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)

cv<-(function(x){
  p<-predict(x, test)
  cm<-caret::confusionMatrix(p, positive = "SNAKE",test$Type)
  
  CVTABLE<-t(data.frame(cm$overall))
  CV2TABLE<-t(data.frame(cm$byClass))
  
  cv_table<-cbind(CVTABLE,CV2TABLE)
  cv_table<-cv_table[,c(1,2,8,9)]
  cv_table
})

cv_table<-t(data.frame(lapply(files, cv)))

cv_max<-c(max(cv_table[,1]), max(cv_table[,2]), max(cv_table[,3]), max(cv_table[,4]))
cv_mean<-c(mean(cv_table[,1]), mean(cv_table[,2]), mean(cv_table[,3]), mean(cv_table[,4]))
cv_min<-c(min(cv_table[,1]), min(cv_table[,2]), min(cv_table[,3]), min(cv_table[,4]))

cv_table<-data.frame(rbind(cv_max,cv_mean, cv_min))

RandomForest_Model<-c("Maximum", "Mean", "Minimum")

#AUC for 30 models to calculate mean, min, and max
AUC<- function(z){
  pred<-predict(z, test, type = "prob")
  pred<-pred[,2]
  pred<-prediction(pred, test$Type)
  
  #AUC (Area Under The Curve)
  auc<-performance(pred, "auc")
  auc<-unlist(slot(auc, "y.values"))
  
}

AUC<-t(data.frame(lapply(files, AUC)))

AUC<-c(max(AUC), mean(AUC), min(AUC))

#Performance Metrics Table
models<-cbind(cv_table, OOB, AUC)
models<-round(models, digits = 3)
models<-cbind(RandomForest_Model,models)
gt(models)%>%
  cols_label(
    RandomForest_Model = "Random Forest Models",
    X1 = "Accuracy",
    X2 = "Kappa",
    X3 = "Sensitivity",
    X4 = "Specificity")%>%
  tab_source_note(
    source_note = "N(trees) = 6000")%>%
  tab_source_note(
    source_note = "OOB = Out of Bag Accuracy")%>%
  tab_source_note(
    source_note = "AUC = Area Under the Curve")
a7 # Maximum Accuracy
a1 #Mean Accuracy

#Variable Importance Metrics using Mean Decrease Accuracy
MDA<-randomForest::importance(a7, class = "SNAKE")
MDA<-data.frame(MDA[,c(1,2)])
gen<-sum(MDA[,1])
sna<-sum(MDA[,2])
#Calculating relative importance of MDA
MDA[,1]<-(MDA[,1]/gen)*100
MDA[,2]<-(MDA[,2]/gen)*100

#Plotting
Habitat_Variables <- c("Canopy Closure", "Rock Cover", "Leaf Cover", "Vegetative Cover","Fallen Log Cover","Woody Stem Height", "Cover Height", "Distance To Cover","Log Diameter","Distance To Water", "Water Depth", "Percent Water", "Percent Woody Debris", "Percent Floating Vegetation", "Percent Bare Soil", "Mean Temperature")

D<-cbind(Habitat_Variables,MDA)
row.names(D)<-NULL

Q=ggplot(D, aes(x = reorder(Habitat_Variables, D$SNAKE), y = D$SNAKE))+
  geom_col(aes(fill=D$SNAKE), color = "black", width = 0.9)+
  geom_text(label=round(D$SNAKE,1), nudge_y = 0.5)+
  ylab("Relative Importance")+
  xlab("Environmental Variables")+
  coord_flip()+
  theme_classic(base_size = 12)+ theme(legend.position = "none")

Q<-Q+scale_fill_gradient2(low="white", high="black")

#Partial Dependence Plots for maximum RF model
plotx<-partial(a7, pred.var = "Dist..To.Water", pdp = T, center = T, plot = T, plot.engine = "ggplot2", type = "classification", which.class = "SNAKE", train = train, rug = T)

ploty<-partial(a7, pred.var = "X.Bare.soil", pdp = T, center = T, plot = T, plot.engine = "ggplot2", type = "classification", which.class = "SNAKE", train = train, rug = T)

plotz<-partial(a7, pred.var = "Rock.Cover", pdp = T, center = T, plot = T, plot.engine = "ggplot2", type = "classification", which.class = "SNAKE", train = train, rug = T)

plotz1<-partial(a7, pred.var = "Depth", pdp = T, center = T, plot = T, plot.engine = "ggplot2", type = "classification", which.class = "SNAKE", train = train, rug = T)

plotz2<-partial(a7, pred.var = "X.Water", pdp = T, center = T, plot = T, plot.engine = "ggplot2", type = "classification", which.class = "SNAKE", train = train, rug = T)

plotz3<-partial(a7, pred.var = "Dist..To.Cover", pdp = T, center = T, plot = T, plot.engine = "ggplot2", type = "classification", which.class = "SNAKE", train = train, rug = T)

#Plots for top 6 variables
plot1<-ploty+ geom_line(size = 1.5)+
  xlab("Bare soil cover (%)")+
  ylab("Relative probability")+
  xlim(0,105)+
  theme_light(base_size = 12)

plot2<-plotx+geom_line(size = 1.5)+
  xlab("Distance to Water (cm)")+
  ylab("")+
  ylim(-1.05,0.35)+
  theme_light(base_size = 12)

plot3<-plotz+ geom_line(size = 1.5)+
  xlab("Rock cover (%)")+
  ylab("")+
  theme_light(base_size = 12)

plot4<-plotz1+ geom_line(size = 1.5)+
  xlab("Water Depth (cm)")+
  ylab("Relative probability")+
  xlim(0,105)+
  theme_light(base_size = 12)

plot5<-plotz2+ geom_line(size = 1.5)+
  xlab("Water Cover (%)")+
  ylab("")+
  xlim(0,105)+
  ylim(-0.80, 0.25)+
  theme_light(base_size = 12)

plot6<-plotz3+ geom_line(size = 1.5)+
  xlab("Distance to Cover (cm)")+
  ylab("")+
  ylim(-0.65,-0.2)+
  theme_light(base_size = 12)

#Variable Importance by MDA
Q

#Partial Dependence Plots of 6 most predictive variables for cottonmouth occurence
plot_grid(plot1, plot2,plot3,plot4,plot5,plot6, labels=c("A ", "B ", "C ","D ", "E ", "F "), ncol = 3, nrow = 2)