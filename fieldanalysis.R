#set working directory
setwd("~/Dropbox/Sun's data/Biomechanics")
library(lme4)
library(car)
library(dplyr)
library(lsmeans)
data=read.csv("fieldanalysis.csv")
data$year<-as.factor(data$year)
data$body<-as.factor(data$body)
data$id<-as.factor(data$id)

#to focus on data only in the presence of P. carabi
datacb=data[data$bignum!="0",]
#to focus on data only in the presence of M. nataliae
datacs=data[data$smallnum!="0",]

#this set of codes is to log transform the densities of mite used, including local P. carabi density on each body part, total density of P. carabi, and total density of M. nataliae; the final dataset being used is newdatacb3
newdatacb<-datacb %>%
  mutate(anewbigmiteintensity=log10(newbigmiteintensity+0.01)+2)
newdatacb2<-newdatacb %>%
  mutate(anewbigintensity=log10(oldbigintensity+0.01)+2)
newdatacb3<-newdatacb2 %>%
  mutate(anewsmallintensity=log10(oldsmallintensity+0.01)+2)

#this set of codes is to log transform the densities of mite used, including local M. nataliae density on each body part, total density of M. nataliae, and total density of P. carabi; the final dataset being used is newdatacs3

newdatacs<-datacs %>%
  mutate(anewsmallmiteintensity=log10(newsmallmiteintensity+0.01)+2)
newdatacs2<-newdatacs %>%
  mutate(anewbigintensity=log10(oldbigintensity+0.01)+2)
newdatacs3<-newdatacs2 %>%
  mutate(anewsmallintensity=log10(oldsmallintensity+0.01)+2)

#to look at how P. carabi was affected by its overall density
model1=glmer(anewbigmiteintensity~anewbigintensity*body+Sex+site+(1|id),data=newdatacb3)
#to look at how P. carabi was affected by M. nataliae overall density
model2=glmer(anewbigmiteintensity~anewsmallintensity+body+Sex+site+(1|id),data=newdatacb3)
#to look at how M. nataliae was affected by its overall density
model3=glmer(anewsmallmiteintensity~anewsmallintensity*body+Sex+site+(1|id),data=newdatacs3)
#to look at how M. nataliae was affected by P. carabi overall density
model4=glmer(anewsmallmiteintensity~anewbigintensity+body+Sex+site+(1|id),data=newdatacs3)

#post-hoc for model1
model.lst<-lstrends(model1, "body", var="anewbigintensity")
pairs(model.lst)

#post-hoc for model3
model.lst<-lstrends(model3, "body", var="anewsmallintensity")
pairs(model.lst)
