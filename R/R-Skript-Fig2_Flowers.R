require(nlme)
require(multcomp)
require(emmeans)
require(ggeffects)
require(plyr)
require(ggplot2)
require(ggbeeswarm)
require(gridExtra)

flowers<-read.table("data_processed_inn.csv",header=TRUE,sep=";",dec=".")
flowers$month <- factor(flowers$month,levels = c("May","Jun",
                                                 "Jul","Aug","Sep"))

#Analysis
##A Effect of different cutting times on the proportion of flowering plant species in the total number of plant species (%)
m.flow.spec<-lme(prop.flow.spec~treatment*month,data = flowers,random=~1|area/subplot)
summary(m.flow.spec)
plot(m.flow.spec)
qqnorm(m.flow.spec)
summary(glht(m.flow.spec,lsm(pairwise~treatment|month),by=NULL))

##B Effect of different cutting times on the flowercover (cm²)
m.flow.cov<-lme(log1p(flow.cov)~treatment*month,data = flowers,random=~1|area/subplot)
summary(m.flow.cov)
plot(m.flow.cov)
qqnorm(m.flow.cov)
summary(glht(m.flow.cov,lsm(pairwise~treatment|month),by=NULL))

#Plot
##A Effect of different cutting times on the proportion of flowering plant species in the total number of plant species (%)
p.flow.spec <- ggeffect(m.flow.spec, terms = c("treatment", "month"), type = "fe")
p.flow.spec <- rename(p.flow.spec, replace=c("predicted"="prop.flow.spec",
                                 "x"="treatment","group"="month"))
pd <- position_dodge(.6)

fig2A<-ggplot()+
  geom_beeswarm(data=flowers,aes(month,prop.flow.spec,shape=treatment,color=treatment),
                dodge.width = .6,size=1)+scale_color_manual(values=c("grey","grey60"))+
  guides(color="none")+
  geom_errorbar(data=p.flow.spec,aes(month, prop.flow.spec, shape=treatment, ymin = conf.low, 
                               ymax = conf.high),position = pd,width=0.0,size=0.4)+
  geom_point(data=p.flow.spec,aes(month, prop.flow.spec, shape = treatment),position = pd,size=2)+
  annotate("text", label =c("n.s.","n.s.","***","***","n.s."), x = c(1,2,3,4,5), 
           y = c(6,12,19,16,11))+
  labs(x="Month",y="Proportion of flowering species (%)",shape="Mowing date")+
  ggtitle("A")+
  theme(panel.background = element_rect(fill = 'white'),axis.line.y = element_line(),
        axis.line.x = element_line(),legend.position = "bottom",
        legend.key = element_rect(fill = "white"),
        legend.margin = margin(0, 0, 0, 0, "cm"),
        plot.margin = margin(0, 0.3, 0, 0, "cm"))
  

##B Effect of different cutting times on the flowercover (cm²)
p.flow.cov <- ggeffect(m.flow.cov, terms = c("treatment", "month"))
p.flow.cov <- rename(p.flow.cov, replace=c("predicted"="flow.cov",
                                 "x"="treatment","group"="month"))
pd <- position_dodge(.6)

fig2B<-ggplot()+
  geom_beeswarm(data=flowers,aes(month,log1p(flow.cov),shape=treatment,color=treatment),
                dodge.width = .6,size=1)+scale_color_manual(values=c("grey","grey60"))+
  guides(color="none")+
  geom_errorbar(data=p.flow.cov,aes(month, flow.cov, shape=treatment, ymin = conf.low, 
                               ymax = conf.high),position = pd,width=0.0,size=0.4)+
  geom_point(data=p.flow.cov,aes(month, flow.cov, shape = treatment),position = pd,size=2)+
  annotate("text", label =c("n.s.","n.s.","***","***","n.s."), x = c(1,2,3,4,5), 
           y = c(1.5,3.3,4,3.8,2.5))+
  labs(x="Month",y="Flower cover (cm², log-transformed)",shape="Mowing date")+
  ggtitle("B")+
  guides(shape="none")+
  theme(panel.background = element_rect(fill = 'white'),axis.line.y = element_line(),
        axis.line.x = element_line(),legend.position = "bottom",
        legend.key = element_rect(fill = "white"),
        legend.margin = margin(0, 0, 0, 0, "cm"),
        plot.margin = margin(0, 0, 1, 0.3, "cm"))

##A+B
fig2<-grid.arrange(fig2A,fig2B,ncol=2)


  
  