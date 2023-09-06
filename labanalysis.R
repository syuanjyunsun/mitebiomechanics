#set working directory
setwd("/Users/sun/Dropbox/Sun's data/Biomechanics")
setwd("/Users/syuan-jyunsun/Library/CloudStorage/Dropbox/Sun's data/Biomechanics")

#lab space competition
library(lme4)
library(car)
library(dplyr)
library(emmeans)

####analysis for intra- and inter-specific competition####
data=read.csv("labspacecompetition.csv")
data$body<-as.factor(data$body)
data$id<-as.factor(data$id)
data$bignum<-as.numeric(data$bignum)
data$smallnum<-as.numeric(data$smallnum)
data$body <- factor(data$body, levels=c("head", "thorax", "pronotum", "elytra","abdomen"))

#specify mite P. carabi
datacb=data[data$mite=="P. carabi",]
#specify mite M. nataliae
datacs=data[data$mite=="M. nataliae",]

####specify treatments with low densities of M. nataliae/ P. carabi only####
datacb<-datacb[datacb$smallnum=="1",]
datacs<-datacs[datacs$bignum=="1",]

#newintensity is the density of mites (for P. carabi or M. nataliae) on each specific body part
#log transform newintensity as anewintensity for P. carabi
newdatacb<-datacb %>%
  mutate(anewintensity=log10((newintensity+0.01)+1))
#log transform newintensity as anewintensity for M. nataliae
newdatacs<-datacs %>%
  mutate(anewintensity=log10((newintensity+0.01)+1))

#analysis for P. carabi
model=glmer(anewintensity~(poly(bigintensity,degree=2)[,2]+poly(bigintensity,degree=2)[,1])*body+(1|id),data=newdatacb)
Anova(model,type=3)

#post-hoc analysis
model.lst<-lstrends(model, specs = c('body'), var = 'bigintensity')
pairs(model.lst)

#analysis for M. nataliae
model=glmer(anewintensity~(poly(smallintensity,degree=2)[,2]+poly(smallintensity,degree=2)[,1])*body+(1|id),data=newdatacs)
Anova(model,type=3)

#post-hoc analysis
model.lst<-lstrends(model, specs = c('body'), var = 'smallintensity')
pairs(model.lst)

#specify mite P. carabi
datacb=data[data$mite=="P. carabi",]
#specify mite M. nataliae
datacs=data[data$mite=="M. nataliae",]

#newintensity is the density of mites (for P. carabi or M. nataliae) on each specific body part
#log transform newintensity as anewintensity for P. carabi
newdatacb<-datacb %>%
  mutate(anewintensity=log10((newintensity+0.01)+1))
#log transform newintensity as anewintensity for M. nataliae
newdatacs<-datacs %>%
  mutate(anewintensity=log10((newintensity+0.01)+1))



#1. compare 1 nataliae x 1,5,10,25,50 P. carabi
model=glmer(anewintensity~(poly(bigintensity,degree=2)[,2]+poly(bigintensity,degree=2)[,1])*body+(1|id),data=newdatacb)

Anova(model,type=3)

model.lst<-lstrends(model, specs = c('body'), var = 'bigintensity')
pairs(model.lst)

#Prop of P. carabi on each body part

big<-ggplot(newdatacb,aes(x=bigintensity,y=num/bignum,color=body, linetype=body))+
  geom_point()+
  geom_smooth(method=glm,method.args=list(family=binomial(link="logit")),se=FALSE,alpha=0.3,size=1)+
  scale_linetype_manual(values=c("solid","dashed","dashed","solid","dashed"))+
  xlab(expression('Total '*italic(P.carabi)*' density (no./mm^2)'))+
  ylab(expression('Proportion of '*italic(P.carabi)*' on each body part'))+
  scale_color_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  scale_fill_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  theme_classic()+
  scale_y_continuous(limits=c(0,1))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank(),legend.key.size=unit(2,"lines"),legend.text=element_text(size=12),
  )
big

#2. compare 1 carabi x 1,3,5,10,25,50 nataliae
model2=glmer(anewintensity~(poly(smallintensity,degree=2)[,2]+poly(smallintensity,degree=2)[,1])*body+(1|id),data=newdatacs)
Anova(model2,type=3)

model.lst<-lstrends(model2, specs = c('body'), var = 'smallintensity')
pairs(model.lst)

newdatacs<-newdatacs[newdatacs$bignum=="1",]
small<-ggplot(newdatacs,aes(x=smallintensity,y=num/smallnum,color=body,linetype=body))+
  geom_point()+
  geom_smooth(method=glm,method.args=list(family=binomial(link="logit")),se=FALSE,alpha=0.3)+
  scale_linetype_manual(values=c("dashed","solid","dashed","dashed","solid"))+
  xlab(expression('Total '*italic(M.nataliae)*' density (no./mm^2)'))+
  ylab(expression('Proportion of '*italic(M.nataliae)*' on each body part'))+
  scale_color_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  scale_fill_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank(),legend.key.size=unit(2,"lines"),legend.text=element_text(size=12),
  )
small

ggarrange(big,small, labels= c("A", "B"),ncol = 2, nrow = 1,widths=c(1,1),common.legend = TRUE,legend="bottom")

#the combined effect of intra-specific and inter-specific competition
#compare 1,5,10,25,50 P.carabi x 1,3,5 M.nataliae to see the role of intra- and inter-specific competition
datanaturalsetting=data[data$naturalsetting=="1",]
datanaturalsetting$body <- factor(datanaturalsetting$body, levels=c("head", "thorax", "pronotum", "elytra","abdomen"))
#log transform newintensity
newdatanaturalsetting<-datanaturalsetting %>%
  mutate(anewintensity=log10((newintensity+0.01)+1))

#specify P.carabi and M.nataliae mite
snewdatanaturalsetting=newdatanaturalsetting[newdatanaturalsetting$mite=="M. nataliae",]
bnewdatanaturalsetting=newdatanaturalsetting[newdatanaturalsetting$mite=="P. carabi",]

#analyse density of M. nataliae
model=glmer(anewintensity~(poly(bigintensity,degree=2)[,2]+poly(bigintensity,degree=2)[,1]+poly(smallintensity,degree=2)[,2]+poly(smallintensity,degree=2)[,1])*body+(1|id),data=snewdatanaturalsetting)
#post-hoc analysis
model.lst<-lstrends(model, specs = c('body'), var = 'smallintensity')
pairs(model.lst)

#analyse density of P.carabi
model=glmer(anewintensity~(poly(bigintensity,degree=2)[,2]+poly(bigintensity,degree=2)[,1]+poly(smallintensity,degree=2)[,2]+poly(smallintensity,degree=2)[,1])*body+(1|id),data=bnewdatanaturalsetting)
Anova(model,type=3)

#post-hoc analysis
model.lst<-lstrends(model, specs = c('body'), var = 'bigintensity')
pairs(model.lst)


big<-ggplot(bnewdatanaturalsetting,aes(x=bigintensity,y=newintensity,color=body,fill=body))+
  geom_point()+
  geom_smooth(method=lm,formula = y~poly(x,2),se=TRUE,alpha=0.3)+
  xlab(expression('Total '*italic(P.carabi)*' density (no./mm^2)'))+
  ylab(expression('Local '*italic(P.carabi)*' density (no./mm^2)'))+
  scale_color_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  scale_fill_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank()
  )

big

big2<-ggplot(bnewdatanaturalsetting,aes(x=smallintensity,y=newintensity,color=body,fill=body))+
  geom_point()+
  geom_smooth(method=lm,formula = y~poly(x,1),se=FALSE,alpha=0.3,linetype="dashed")+
  xlab(expression('Total '*italic(M.nataliae)*' density (no./mm^2)'))+
  ylab(expression('Local '*italic(P.carabi)*' density (no./mm^2)'))+
  scale_color_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  scale_fill_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank()
  )

big2

small<-ggplot(snewdatanaturalsetting,aes(x=smallintensity,y=newintensity,color=body,fill=body))+
  geom_point()+
  geom_smooth(method=lm,formula = y~poly(x,1),se=TRUE,alpha=0.3)+
  xlab(expression('Total '*italic(M.nataliae)*' density (no./mm^2)'))+
  ylab(expression('Local '*italic(M.nataliae)*' density (no./mm^2)'))+
  scale_color_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  scale_fill_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank()
  )

small

small2<-ggplot(snewdatanaturalsetting,aes(x=bigintensity,y=newintensity,color=body,fill=body))+
  geom_point()+
  geom_smooth(method=lm,formula = y~poly(x,1),se=FALSE,alpha=0.3,linetype="dashed")+
  xlab(expression('Total '*italic(P.carabi)*' density (no./mm^2)'))+
  ylab(expression('Local '*italic(M.nataliae)*' density (no./mm^2)'))+
  scale_color_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  scale_fill_manual(values=c("#d53e4f","#fdae61","#fee08b","#abdda4","#3288bd"))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank()
  )

small2


ggarrange(big,big2,small,small2, labels= c("A", "B", "C", "D"),ncol = 2, nrow = 2,widths=c(1,1),common.legend = TRUE,legend="bottom")


model=glmer(anewintensity~bigintensity*smallintensity*body+(1|id),data=newdatanaturalsetting)
model=glmer(anewintensity~bigintensity*smallintensity*body+(1|id),data=newdatanaturalsetting)
model=glmer(anewintensity~(bigintensity+smallintensity)*body+bigintensity*smallintensity+(1|id),data=newdatanaturalsetting)
model=glmer(anewintensity~(smallintensity)*body+bigintensity*smallintensity+(1|id),data=newdatanaturalsetting)
model=glmer(anewintensity~(smallintensity)*body+bigintensity+smallintensity+(1|id),data=newdatanaturalsetting)

Anova(model,type=3)

model.lst<-lstrends(model, specs = c('body'), var = 'smallintensity')
model.lst<-lstrends(model, specs = c('body'), var = 'bigintensity')
pairs(model.lst)

####mite reproduction####
data=read.csv("mite reproduction.csv")
data$mitetr <- factor(data$mitetr, levels=c("P. carabi", "M. nataliae"))
model=glmer(mitenum~mitetr+wt+(1|bl),poisson,data=data)
Anova(model,type=3)
datab=data[data$mitetr=="P. carabi",]
datas=data[data$mitetr=="M. nataliae",]
mean(datab$mitenum)
mean(datas$mitenum)

#detection of overdispersion
sum(resid(model, type = "pearson")^2)/model$df.resid

#negative binomial distribution
model=glmer.nb(mitenum~mitetr+wt+(1|bl),data=data)
Anova(model,type=3)

install.packages("palmerpenguins")
library(palmerpenguins)
ggboxplot(data ,
          x = "mitetr",
          y = "mitenum",
          xlab = "Species of mite",
          ylab = "Number of mite offspring",
          color = "mitetr",
          palette = "npg",
          add = "jitter",
          shape = "mitetr",legend="FALSE")

####quantification of hair proportion####
data=read.csv("hair quantification on abdomen.csv")
model=glmer(num~type+(1|id),poisson,data=data)
Anova(model,type=3)

####analysis for long hair density between thorax and abdomen####
data=read.csv("longhairnum.csv")
model=glmer(num~body+(1|id),poisson,data=data)
Anova(model,type=3)

data=data[data$hairdensity=="TRUE",]
model=glmer(num~body+(1|id),poisson,data=data)

datat=data[data$body=="thorax",]
dataa=data[data$body=="abdomen",]

####pad area####
data=read.csv("padarea.csv")
datab=data[data$mite=="P. carabi",]
datas=data[data$mite=="M. nataliae",]
model=glm(log(area)~mite,data=data)
Anova(model,type=3)

####force analysis####
data=read.csv("force.csv")
data$tr<-as.factor(data$tr)
data$angles<-as.factor(data$angles)
datac=data[data$substrate=="substrate",]
datacl=datac[datac$mite=="P. carabi",]
datacs=datac[datac$mite=="M. nataliae",]
datab=data[data$substrate=="beetle",]
datab$body <- factor(datab$tr, levels=c("thorax", "abdomen","pronotum", "elytra"))
databl=datab[datab$mite=="P. carabi",]
databl$tr <- factor(databl$tr, levels=c("thorax", "abdomen","pronotum", "elytra"))
databs=datab[datab$mite=="M. nataliae",]
databs$tr <- factor(databs$tr, levels=c("thorax", "abdomen","pronotum", "elytra"))
model=glmer(log(force+0.01)~tr*(mite+angles)+(1|id),gaussian,data=datac)
Anova(model,type=3)

model=glmer(log(force+0.01)~tr*(angles)+(1|id),gaussian,data=datacl)
Anova(model,type=3)
model.lst=lsmeans(model, pairwise~tr|angles, adjust="tukey")
pairs(model.lst)

model.lst=lsmeans(model, pairwise~angles|tr, adjust="tukey")
pairs(model.lst)

model=glmer(log(force+0.01)~tr*(angles)+(1|id),gaussian,data=datacs)
Anova(model,type=3)
model.lst=lsmeans(model, pairwise~tr|angles, adjust="tukey")
pairs(model.lst)

model.lst=lsmeans(model, pairwise~angles|tr, adjust="tukey")
pairs(model.lst)

####force measurement on beetles####
model=glmer(log(force+0.01)~tr*mite*angles+(1|id),gaussian,data=datab)
Anova(model,type=3)

model.lst=lsmeans(model, pairwise~tr|angles+mite, adjust="tukey")
pairs(model.lst)

model.lst=lsmeans(model, pairwise~angles|tr+mite, adjust="tukey")
pairs(model.lst)

model.lst=lsmeans(model, pairwise~mite|tr+angles, adjust="tukey")
pairs(model.lst)
