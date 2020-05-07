gauge<-read.csv("data/GaugeData-USGS.csv")
gauge[,2]<-as.Date(gauge[,2])
gauge[,4]<-gauge[,3]*0.3048

discharge<-read.csv("data/SycamoreData-USGS.csv")
discharge[,2]<-as.Date(discharge[,2])
discharge[,4]<-discharge[,3]*0.0283168



p1<-ggplot(gauge, aes(x =gauge$datetime, y= gauge$V4))+
  geom_line(color = "black")+
  xlab("")+
  ylab("Gauge Height (m)")+
  ggtitle("USGS 03435000 Cumberland River Below Cheatham Dam, TN")+
  theme_classic()


p1<-p1+scale_x_date(breaks=date_breaks("6 months"), labels = date_format("%b %y"))+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

p2<-ggplot(discharge, aes(x =discharge$datetime, y= discharge$V4))+
  geom_line(color = "black")+
  xlab("")+
  ylab("Discharge (cubic meters per second)", size = 14)+
  ggtitle("USGS 03431800 Sycamore Creek Near Ashland City, TN", size = 14)+
  theme_classic(base_size = 20)

p2<-p2+scale_x_date(breaks=date_breaks("6 months"), labels = date_format("%b %y"))+
  theme(
    plot.title = element_text(size = 20),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

hydrogrid1<-plot_grid(p1,p2, ncol = 1, align = "v")
hydrogrid1
