setwd("~/Desktop/innovation paper/access")
install.packages(Matching)
library(pacman)
library(lmtest)
library("sandwich")
library(rbounds)
library(rgenoud)
library(survey)
library(tableone)
library(dplyr)
library(Matching)
library(cobalt)
library(stats)
library(optmatch) 
library(foreign)
library(devtools)
devtools::install_github("Ngendahimana/SensitivityR5")
library(SensitivityR5)

rm(list = ls(all.names = TRUE))
p_load(haven, janitor, here, tidyverse, readxl, tableone,Hmisc, MatchIt, cobalt, Zelig, rbounds, EValue, sandwich)
data<-read_dta("/Users/preciousallor/Desktop/innovation paper/access/final4.dta")
summary(data)
data<-na.omit(data)
datasvy<-svydesign(id=~1, data = data, weights = ~wht)
data$tr<-data$access
data$tr<- as.factor(data$tr)
data$region<-as.factor(data$region)
data$linc2=data$linc^2
data$age2=data$age^2
summary(data$tr)
data$momo<- as.factor(data$momo)
data$headmale<- as.factor(data$headmale)
write.dta(data,"/Users/preciousallor/Dropbox/Mac/Desktop/innovation paper/access/mydata.dta" )
summary(data$momo)
summary(data$headmale)
#nearest neigbor
nearest.match<-matchit(tr ~ male+age + primary + secondary + tertiary + married+ employed + christian+islam+traditional+labforce+hsize +region
                        , data=data, method="nearest", weights= ~wht, estimand= "ATT",replace=FALSE)

summary(nearest.match)

bal.tab(nearest.match, m.threshold=0.25, un=TRUE)
bal.tab(nearest.match, v.threshold=2)
plot(nearest.match,type="jitter", interactive=FALSE)
plot(nearest.match,type="hist")
love.plot(bal.tab(nearest.match, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(nearest.match, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
pre.balance1<-summary(nearest.match)$sum.all
post.balance1<-summary(nearest.match)$sum.match
nearest.data<-match.data(nearest.match)
summary(nearest.data)
#regression

nearest.model<-glm(notpoor ~access+male+ age + age2 + primary + secondary + tertiary +married+ employed +christian+islam+traditional+labforce+hsize + linc2 +region 
                   ,data=nearest.data, weights=wht)

summary(nearest.model)
coeftest(nearest.data, vcov. = vcovCL, cluster = ~subclass)


#full matching
full.match<-matchit(tr ~ male+age + primary + secondary + tertiary +married + employed + christian+islam+traditional+labforce+hsize + momo +region + dist
                    , data=data, method="full", distance = "glm", link = "logit", s.weights= ~wht, estimand = "ATT")

summary(full.match)
bal.tab(full.match, m.threshold=0.25, un=TRUE)
bal.tab(full.match, v.threshold=2)
plot(full.match,type="jitter", interactive=FALSE)
plot(full.match,type="hist")
love.plot(bal.tab(full.match, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(full.match, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
pre.balance2<-summary(full.match)$sum.all
post.balance2<-summary(full.match)$sum.match
full.data<-match.data(full.match)

#regression
full.model<-glm(notpoor ~access+male+age + primary + secondary + tertiary+married+ employed+christian+islam+traditional+labforce+hsize + linc + region+dist 
                ,data=full.data, weights=wht)
summary(full.model)

#optimal matching

opt.match<-matchit(tr ~ male+age+ age + primary + secondary + tertiary + married+ employed+christian+islam+traditional+labforce+hsize+linc + region+dist
                   , data=data, method="optimal", s.weights= ~wht, estimand= "ATT",replace=FALSE, ratio=2)

summary(opt.match)
bal.tab(opt.match, m.threshold=0.25, un=TRUE)
bal.tab(opt.match, v.threshold=2)
plot(opt.match,type="jitter", interactive=FALSE)
plot(opt.match,type="hist")
love.plot(bal.tab(opt.match, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(opt.match, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
pre.balance3<-summary(opt.match)$sum.all
post.balance3<-summary(opt.match)$sum.match
opt.data<-match.data(opt.match)

#regression
opt.model<-glm(notpoor ~access+male+age + primary + secondary + tertiary+married + employed +christian+islam+traditional+labforce+hsize+linc+region+dist 
               ,data=opt.data, weights=wht)
summary(opt.model)

#genetic matching

gen.match<-matchit(tr ~ male+ age + primary + secondary + tertiary +married+ employed+christian+islam+traditional+labforce+hsize+ linc + region+dist
                   , data=data, method="genetic", weights= ~wht, distance = "glm", estimand= "ATT",replace=FALSE, pop.size=120)
summary(gen.match)
bal.tab(gen.match, m.threshold=0.25, un=TRUE)
bal.tab(gen.match, v.threshold=2)
plot(gen.match,type="jitter", interactive=FALSE, distance = "mahalanobis")
plot(gen.match,type="hist")
love.plot(bal.tab(gen.match, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(gen.match, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
pre.balance4<-summary(gen.match)$sum.all
post.balance4<-summary(gen.match)$sum.match
gen.data<-match.data(gen.match)

#regression

gen.model<-glm(notpoor ~access+male+age+primary + secondary + tertiary+married+ employed + christian+islam+traditional+labforce+hsize+linc+region + dist 
               ,data=gen.data, weights=wht)
summary(gen.model)

#Logit

logit.model<-glm(notpoor ~access+male+age+primary + secondary + tertiary+married+ employed + christian+islam+traditional+labforce+hsize+linc+region + dist 
               ,data=data, weights=wht, family = binomial(link = "logit"))
summary(logit.model)
marginlogit<-margins(logit.model)
summary(marginlogit)
print(summary(marginlogit),digits=15)

probit.model<-glm(notpoor ~access+male+age+primary + secondary + tertiary+married+ employed + christian+islam+traditional+labforce+hsize+linc+region + dist 
                 ,data=data, weights=wht, family = binomial(link = "probit"))
summary(probit.model)
marginprobit<-margins(probit.model)
summary(marginprobit)


#Sensitivity
binarysens2(x=nearest.match,y="notpoor", Gamma=2, GammaInc = 0.02)
binarysens2(x=opt.match,y="notpoor", Gamma=2, GammaInc = 0.01)
binarysens2(x=full.match,y="notpoor", Gamma=1.5, GammaInc = 0.01)
binarysens2(x=gen.match,y="notpoor", Gamma=2, GammaInc = 0.01)


#Biprobit 
biprobit(notpoor=access male age) (access=momo access male age)



