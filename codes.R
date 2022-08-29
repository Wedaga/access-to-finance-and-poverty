setwd("~/Desktop/innovation paper/access")
install.packages(Matching)
library(pacman)
library(lmtest)
library(rbounds)
library(rgenoud)
library(survey)
library(tableone)
library(dplyr)
library(Matching)
library(cobalt)
library(stats)
library(biprobit)
p_load(haven, janitor, here, tidyverse, readxl, tableone,Hmisc, MatchIt, cobalt, Zelig, rbounds, EValue, sandwich)
data3<-read_dta("/Users/preciousallor/Desktop/innovation paper/access/final2.dta")
head(data1)
summary(data1)
data2<-na.omit(data3)
head(data2)
datasvy1<-svydesign(id=~1, data = data2, weights = ~wht)
datasvy1

table1<-svyCreateTableOne(vars = c("access","notpoor", "age", "agesq", "male", "education","spous_present","married","christian","islam"
                                   ,"traditional", "hsize","labforce","region"), strata="access", data= datasvy1
                          ,factorVars=c("notpoor","male", "region", "spous_present", "married", "christian","islam","traditional","labforce"
                                        ,"region"))
data2$tr<-data2$access
data2$tr<- as.factor(data2$tr)

data2$region<-as.factor(data2$region)
data2$edusq<-data2$education^2
nearest.match1<-matchit(tr ~ male+age + education + edusq + spous_present+married+christian+islam+traditional+labforce+hsize+region+dist
                       , data=data2, method="nearest", weights= ~wht, estimand= "ATT",replace=FALSE)

summary(nearest.match1)
bal.tab(nearest.match1, m.threshold=0.25, un=TRUE)
bal.tab(nearest.match1, v.threshold=2)
plot(nearest.match1,type="jitter", interactive=FALSE)
plot(nearest.match1,type="hist")
love.plot(bal.tab(nearest.match1, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(nearest.match1, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
data.matched<-match.data(nearest.match)
pre.balance<-summary(nearest.match)$sum.all
post.balance<-summary(nearest.match)$sum.match
pre.balance
nearest.data<-match.data(nearest.match)

#regressions
nearest.model<-glm(notpoor ~access+male+age+education+spous_present+married+christian+islam+traditional+labforce+hsize+region+dist 
                    ,data=nearest.data, weights=wht)
summary(nearest.model)


full.match<-matchit(tr ~ male+age+education+spous_present+married+christian+islam+traditional+labforce+hsize+region+dist
                       , data=data, method="full", weights= ~wht, estimand= "ATT",replace=FALSE)
summary(full.match)
bal.tab(full.match, m.threshold=0.25, un=TRUE)
bal.tab(full.match, v.threshold=2)
plot(full.match,type="jitter", interactive=FALSE)
plot(full.match,type="hist")
love.plot(bal.tab(full.match, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(full.match, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
data.matched1<-match.data(full.match)
pre.balance1<-summary(full.match)$sum.all
post.balance1<-summary(full.match)$sum.match
pre.balance1
full.data<-match.data(full.match)

full.model<-glm(notpoor ~access+male+age+education+spous_present+married+christian+islam+traditional+labforce+hsize+region+dist 
                   ,data=full.data, weights=wht)
summary(full.model)

opt.match<-matchit(tr ~ male+age+education+spous_present+married+christian+islam+traditional+labforce+hsize+region+dist
                    , data=data, method="optimal", weights= ~wht, estimand= "ATT",replace=FALSE)
summary(opt.match)
bal.tab(opt.match, m.threshold=0.25, un=TRUE)
bal.tab(opt.match, v.threshold=2)
plot(opt.match,type="jitter", interactive=FALSE)
plot(opt.match,type="hist")
love.plot(bal.tab(opt.match, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(opt.match, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
data.matched2<-match.data(opt.match)
pre.balance2<-summary(opt.match)$sum.all
post.balance2<-summary(opt.match)$sum.match
pre.balance2
opt.data<-match.data(opt.match)

opt.model<-glm(notpoor ~access+male+age+education+spous_present+married+christian+islam+traditional+labforce+hsize+region+dist 
                ,data=opt.data, weights=wht)
summary(full.model)



gen.match<-matchit(tr ~ male+age + primary + secondary + tertiary + spous_present+married+christian+islam+traditional+labforce+hsize+region+dist
                   , data=data2, method="genetic", s.weights= ~wht, distance = "mahalanobis", estimand= "ATT",replace=FALSE)
summary(gen.match)
bal.tab(gen.match, m.threshold=0.25, un=TRUE)
bal.tab(gen.match, v.threshold=2)
plot(gen.match,type="jitter", interactive=FALSE)
plot(gen.match,type="hist")
love.plot(bal.tab(gen.match, m.threshold=0.25), stat="mean.diffs", grid=TRUE, stars="raw", abs = F)
love.plot(bal.tab(gen.match, v.threshold=2), stat="variance.ratios", grid=TRUE, stars="raw", abs = F)
data.matched2<-match.data(gen.match)
pre.balance2<-summary(gen.match)$sum.all
post.balance2<-summary(gen.match)$sum.match
pre.balance2
gen.data<-match.data(gen.match)

write_dta(gen.data, "mydata.dta")

gen.model<-glm(notpoor ~access+male+age+primary + secondary + tertiary +spous_present+married+christian+islam+traditional+labforce+hsize+region 
               ,data=gen.data, weights=wht)
summary(gen.model)

#two stage sls
first.model<-glm(access ~male+age+primary + secondary + tertiary +spous_present+married+christian+islam+traditional+labforce+hsize+region+dist 
               ,data=gen.data, weights=wht)
summary(first.model)
predicted<-predict(first.model)
summary(predicted)
gen.data$predicted<-predicted
second.model<-glm(notpoor ~predicted+male+age+primary + secondary + tertiary+spous_present+married+christian+islam+traditional+labforce+hsize+region 
               ,data=gen.data, weights=wht)
summary(second.model)