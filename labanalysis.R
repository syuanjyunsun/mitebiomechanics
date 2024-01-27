#set working directory
setwd("/Users/sun/Dropbox/Sun's data/Biomechanics")
setwd("/Users/syuan-jyunsun/Library/CloudStorage/Dropbox/Sun's data/Biomechanics/Biomechanics data and code")
setwd("/Users/sun/Library/CloudStorage/Dropbox/Sun's data/Biomechanics/Biomechanics data and code")
#lab space competition
library(lme4)
library(car)
library(dplyr)
library(emmeans)
library(ggplot2)

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
model=glmer(cbind(num,bignum-num)~bigintensity*body+(1|id),family=binomial(link="logit"),data=newdatacb)
Anova(model,type=3)


big<-ggplot(newdatacb,aes(x=bigintensity,y=num/bignum,color=body, linetype=body))+
  geom_point(size=2,alpha=0.8)+
  geom_smooth(method=glm,method.args=list(family=binomial(link="logit")),se=FALSE,alpha=0.3,size=1)+
  scale_linetype_manual(values=c("solid","dashed","dashed","solid","dashed"))+
  xlab(expression('Total '*italic(P.carabi)*' density (no./mm^2)'))+
  ylab(expression('Proportion of '*italic(P.carabi)*' on each body part'))+
  scale_color_manual(values=c("#ef476f","#ffb703","#7dcfb6","#AAB8C2","#14213d"))+
  scale_fill_manual(values=c("#ef476f","#ffb703","#7dcfb6","#AAB8C2","#14213d"))+
  theme_classic()+
  scale_y_continuous(limits=c(0,1))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank(),legend.key.size=unit(2,"lines"),legend.text=element_text(size=12),
  )
big

#compare 1 carabi x 1,3,5,10,25,50 nataliae

newdatacs<-newdatacs[newdatacs$bignum=="1",]
model=glmer(cbind(num,smallnum-num)~smallintensity*body+(1|id),family=binomial(link="logit"),data=newdatacs)
Anova(model,type=3)

small<-ggplot(newdatacs,aes(x=smallintensity,y=num/smallnum,color=body,linetype=body))+
  geom_point(size=2,alpha=0.8)+
  geom_smooth(method=glm,method.args=list(family=binomial(link="logit")),se=FALSE,alpha=0.3)+
  scale_linetype_manual(values=c("dashed","solid","dashed","dashed","solid"))+
  xlab(expression('Total '*italic(M.nataliae)*' density (no./mm^2)'))+
  ylab(expression('Proportion of '*italic(M.nataliae)*' on each body part'))+
  scale_color_manual(values=c("#ef476f","#ffb703","#7dcfb6","#AAB8C2","#14213d"))+
  scale_fill_manual(values=c("#ef476f","#ffb703","#7dcfb6","#AAB8C2","#14213d"))+
  theme_classic()+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank(),legend.key.size=unit(2,"lines"),legend.text=element_text(size=12),
  )
small

#the effect of inter-specific competition controlling for intra-specific competition
#compare 1,5,10,25,50 P.carabi x 1,3,5 M.nataliae to see the role of intra- and inter-specific competition
datanaturalsetting=data[data$naturalsetting=="1",]
datanaturalsetting$body <- factor(datanaturalsetting$body, levels=c("head", "thorax", "pronotum", "elytra","abdomen"))
#log transform newintensity
newdatanaturalsetting<-datanaturalsetting %>%
  mutate(anewintensity=log10((newintensity+0.01)+1))

#specify P.carabi and M.nataliae mite
snewdatanaturalsetting=newdatanaturalsetting[newdatanaturalsetting$mite=="M. nataliae",]
bnewdatanaturalsetting=newdatanaturalsetting[newdatanaturalsetting$mite=="P. carabi",]

#analyse how M. nataliae affect P. carabi
model=glmer(cbind(num,bignum-num)~(bigintensity+smallintensity)*body+(1|id),family=binomial(link="logit"),data=bnewdatanaturalsetting)

bnewdatanaturalsetting_h=bnewdatanaturalsetting[bnewdatanaturalsetting$body=="head",]
bnewdatanaturalsetting_t=bnewdatanaturalsetting[bnewdatanaturalsetting$body=="thorax",]
bnewdatanaturalsetting_p=bnewdatanaturalsetting[bnewdatanaturalsetting$body=="pronotum",]
bnewdatanaturalsetting_e=bnewdatanaturalsetting[bnewdatanaturalsetting$body=="elytra",]
bnewdatanaturalsetting_a=bnewdatanaturalsetting[bnewdatanaturalsetting$body=="abdomen",]

model_h=glmer(cbind(num,bignum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=bnewdatanaturalsetting_h)
model_t=glmer(cbind(num,bignum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=bnewdatanaturalsetting_t)
model_p=glmer(cbind(num,bignum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=bnewdatanaturalsetting_p)
model_e=glmer(cbind(num,bignum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=bnewdatanaturalsetting_e)
model_a=glmer(cbind(num,bignum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=bnewdatanaturalsetting_a)

Anova(model_h,type=3)
Anova(model_t,type=3)
Anova(model_p,type=3)
Anova(model_e,type=3)
Anova(model_a,type=3)

big2<-ggplot(bnewdatanaturalsetting,aes(x=smallintensity,y=num/bignum,color=body, linetype=body))+
  geom_point()+
  geom_smooth(method=glm,method.args=list(family=binomial(link="logit")),se=FALSE,alpha=0.3,size=1)+
  scale_linetype_manual(values=c("dashed","solid","dashed","dashed","dashed"))+
  xlab(expression('Total '*italic(M.nataliae)*' density (no./mm^2)'))+
  ylab(expression('Proportion of '*italic(P.carabi)*' on each body part'))+
  scale_color_manual(values=c("#ef476f","#ffb703","#7dcfb6","#AAB8C2","#14213d"))+
  scale_fill_manual(values=c("#ef476f","#ffb703","#7dcfb6","#AAB8C2","#14213d"))+
  theme_classic()+
  scale_y_continuous(limits=c(0,1))+
  theme(axis.text=element_text(size=12),axis.title = element_text(size = 14),legend.title=element_blank(),legend.key.size=unit(2,"lines"),legend.text=element_text(size=12),
  )
big2

#analyse how M. nataliae affect P. carabi
model=glmer(cbind(num,smallnum-num)~(bigintensity+smallintensity)*body+(1|id),family=binomial(link="logit"),data=snewdatanaturalsetting)
Anova(model,type=3)

model=glmer(cbind(num,smallnum-num)~(bigintensity+smallintensity)+body+(1|id),family=binomial(link="logit"),data=snewdatanaturalsetting)
Anova(model,type=3)

snewdatanaturalsetting_h=snewdatanaturalsetting[snewdatanaturalsetting$body=="head",]
snewdatanaturalsetting_t=snewdatanaturalsetting[snewdatanaturalsetting$body=="thorax",]
snewdatanaturalsetting_p=snewdatanaturalsetting[snewdatanaturalsetting$body=="pronotum",]
snewdatanaturalsetting_e=snewdatanaturalsetting[snewdatanaturalsetting$body=="elytra",]
snewdatanaturalsetting_a=snewdatanaturalsetting[snewdatanaturalsetting$body=="abdomen",]

model_h=glmer(cbind(num,smallnum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=snewdatanaturalsetting_h)
model_t=glmer(cbind(num,smallnum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=snewdatanaturalsetting_t)
model_p=glmer(cbind(num,smallnum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=snewdatanaturalsetting_p)
model_e=glmer(cbind(num,smallnum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=snewdatanaturalsetting_e)
model_a=glmer(cbind(num,smallnum-num)~(bigintensity+smallintensity)+(1|id),family=binomial(link="logit"),data=snewdatanaturalsetting_a)

Anova(model_h,type=3)
Anova(model_t,type=3)
Anova(model_p,type=3)
Anova(model_e,type=3)
Anova(model_a,type=3)

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

model=glmer.nb(mitenum~mitetr+wt+(1|bl),data=data)

datab=data[data$mitetr=="P. carabi",]
datas=data[data$mitetr=="M. nataliae",]

mean(datab$mitenum)
mean(datas$mitenum)

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
