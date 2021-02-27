# paper_code-list
#菜真第一次敲完R代码记录一下
#merge data
library(haven)
d_adult <- read_dta("D:\\research2020\\advanced statistics\\essay\\data\\cfps2014adult_201906.dta")
#d_fcom <- read_dta("D:\\research2020\\【请勿移动和编辑】数据\\[CFPS+Public+Data]+CFPS+2014+in+STATA+(Chinese)\\cfps2014famconf_170630.dta")
d_feco <- read_dta("D:\\research2020\\advanced statistics\\essay\\data\\cfps2014famecon_201906.dta")
d_fcom <- read_dta("D:\\research2020\\advanced statistics\\essay\\data\\cfps_dfcom.dta")

#??????家用电脑这个大麻烦
d_feco$computer<- 2
d_feco$computer[d_feco$fs6_s_1!=7 &d_feco$fs6_s_2 !=7&d_feco$fs6_s_3 !=7&d_feco$fs6_s_4 !=7&d_feco$fs6_s_5 !=7&d_feco$fs6_s_6 !=7&d_feco$fs6_s_7 !=7&d_feco$fs6_s_8 !=7&d_feco$fs6_s_9 !=7&d_feco$fs6_s_10!=7&d_feco$fs6_s_11 !=7&d_feco$fs6_s_12 !=7&d_feco$fs6_s_13 !=7&d_feco$fs6_s_14 !=7&d_feco$fs6_s_15 !=7]<-0
d_feco$computer[d_feco$computer>0]<-1
summary(d_feco$computer)
table(d_feco$computer)

#delete uesless variables
myvars1<-names(d_fcom)%in% c("fid14","tf201pid")
new_dfcom<-d_fcom[myvars1]

myvars2<-names(d_adult)%in% c("provcd14","countyid14","cid14","urban14","pid","fid14","cfps_gender","cfps_minzu","cfps2014_age","cfps2014eduy_im","qp201","cfps2012_marriage_update","cfps_party","qa301","ku250m","ku2")
new_dault<-d_adult[myvars2]

myvars3<-names(d_feco)%in% c("fid14","fm1","fml2014num","fincome1","savings","ft1001","fu201","computer")
new_dfeco<-d_feco[myvars3]  
names(new_dfeco)
#merge to d
new_df<-merge(new_dfeco,new_dfcom,by="fid14")
#rename adult to match
library(reshape)
names(new_df)
names(new_df)[9]<-"pid"
new_d<-merge(new_df,new_dault,by="pid")

####recode and rename some variables
ls(new_d)
names(new_d)
names(new_d)[3:6]<-c("fsize","chuangye","loan","renqing")
names(new_d)[19:20]<-c("hukou","health")
names(new_d)[23:24]<-c("net","nettime")
#new_d<-rename(new_d,c(ku2="net",ku250m="nettime",qp201="health",qa301="hukou",fu201="renqing",ft1001="loan",fm1="chuangye",fml2014num="fsize"))
new_d$nettime[new_d$nettime<=0]<-0
new_d$cfps_gender[new_d$cfps_gender<0] <- NA
new_d$cfps_minzu[new_d$cfps_minzu<0]<- NA
new_d$cfps2014_age[new_d$cfps2014_age<0]<-NA
new_d$cfps2014eduy_im[new_d$cfps2014eduy_im<0]<-NA
new_d$cfps2012_marriage_update[new_d$cfps2012_marriage_update<0]<-NA
new_d$cfps_party[new_d$cfps_party<0]<-NA
new_d$net[new_d$net<0] <- NA
new_d$urban14[new_d$urban14<0]<-NA
new_d$health[new_d$health<0]<-NA
#生成年龄平方
library(tidyverse)
new_d<- mutate(new_d,age2=cfps2014_age*cfps2014_age)
new_d$edul<-new_d$cfps2014eduy_im
new_d$edul[new_d$edul<=6]<-1
new_d$edul[new_d$edul>6&new_d$edul<=12]<-2
new_d$edul[new_d$eddul>12]<-3
#对婚姻状况处理为已婚=1，未婚=0
new_d$marriage<-new_d$cfps2012_marriage_update
new_d$marriage[new_d$marriage>1&new_d$marriage<=2] <- 7
new_d$marriage[new_d$marriage>2&new_d$marriage<=5|new_d$marriage<=1] <- 6
new_d<-transform(new_d,marriage=marriage-6)
table(new_d$marriage)
#处理户口
table(new_d$hukou)
new_d$hukou[new_d$hukou==1]<-0
new_d$hukou[new_d$hukou==3]<-1
new_d$hukou[new_d$hukou==5|new_d$hukou==79]<-NA
table(new_d$hukou)
#收入存款之类的取对数
new_d<-transform(new_d,lnrq=log10(renqing+9),
                 lnsavings=log10(savings+1),
                 lnfincome=log10(fincome1+1),
                 lnloan=log10(loan+9))
#选择年龄小于60岁
new_d <- subset(new_d,cfps2014_age<60&cfps2014_age>18 ,select=pid:lnloan)
#处理债务删除变量
d_vari<-names(new_d)%in% c("loan","lnloan")
new_d<-new_d[!d_vari]
#########0222数据整理完成啦哈哈哈哈去喝水####
library(gmodels)
CrossTable(new_d$chuangye,new_d$net)
CrossTable(new_d$urban14,new_d$net)
ls(new_d)
library(psych)
d1 <- describe(new_d$fsize)
d2 <- describe(new_d$lnfincome)
d3 <- describe(new_d$lnsavings)
d4<- describe(new_d$lnloan)
d5<- describe(new_d$lnrq)
d6<- describe(new_d$cfps_gender)
d7<- describe(new_d$cfps_minzu)
d8<- describe(new_d$cfps2014_age)
d9<- describe(new_d$cfps2014eduy_im)
d10<-describe(new_d$health)
d11<- describe(new_d$cfps2012_marriage_update)
d12<-describe(new_d$cfps_party)
d13<-describe(new_d$urban14)
d14<-describe(new_d$net)
d15<-describe(new_d$nettime)
d16<- describe(new_d$chuangye)
d17<- describe(new_d$computer)
#横向合并成为表格输出
total<-rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17)
total
###select some variables 画一个相关系数矩阵new_d1 for plot
options(digits=2)
myvars4<-names(new_d)%in% c("fsize","lnfincome","lnsavings","cfps_gender","cfps2014_age","edul","marriage","cfps_party","urban14","chuangye","net")
new_d1<-new_d[myvars4]
library(corrgram)
corrgram(new_d1,order=TRUE,lower.panel=panel.shade,upper.panel=panel.pie,text.panel=panel.txt)
#logit
#transform to factor
new_d$chuangye[new_d$chuangye>0]<-1
new_d$chuangye[new_d$chuangye==0]<-0
new_d$chuangye <- factor(new_d$chuangye,levels=c(1,0),labels=c('yes','no'))
fit_try<-glm(formula = chuangye ~ fsize+lnfincome+lnsavings+lnrq+
               cfps_gender +cfps_minzu + cfps2014_age+ age2 +edul +health+ marriage +cfps_party+hukou+urban14
             +net+nettime+provcd14,
             family = binomial(),
             data = new_d)
fit_try_home<-glm(formula = chuangye ~ fsize+lnfincome+lnsavings+lnrq+provcd14,
                  family = binomial(),
                  data = new_d)
fit_try_person<-glm(chuangye ~ fsize+lnfincome+lnsavings+lnrq+
                      cfps_gender +cfps_minzu + cfps2014_age+ age2 +edul +health+ marriage +cfps_party+hukou+urban14+provcd14,
                    family = binomial(),
                    data = new_d)
summary(fit_try_person)
summary(fit_try_home)
summary(fit_try)
exp(coef(fit_try))
##内生转换模型
library(endoSwitch)
d_na<-na.omit(new_d)
is.na(d_na)
d_na$net
OutcomeDep <- 'chuangye'
SelectDep <- 'net'
OutcomeCov <- c("fsize","lnfincome","lnsavings","lnrq","cfps_gender" ,"cfps_minzu" ,"cfps2014_age" ,"age2","edul", "health","marriage","cfps_party","hukou","urban14","provcd14")
SelectCov <- c("fsize","lnfincome","lnsavings","lnrq","cfps_gender" ,"cfps_minzu" ,"cfps2014_age" ,"age2","edul", "health","marriage","cfps_party","hukou","urban14","provcd14","computer")
endoReg1 <- endoSwitch(d_na, OutcomeDep, SelectDep, OutcomeCov, SelectCov)
summary(endoReg1)

##finite mixture model
library(lattice)
library(flexmix)
flx3 <- flexmix(chuangye~ fsize+lnfincome+age2+edul+urban14+
                  net+nettime, data = d_na, k =3,
                model = FLXMRglm(family = "poisson"))
summary(flx_n)
#parameters(flx_n, component = 1)
rf_n<-refit(flx_n)
summary(rf_n)
plot(flx_n)
#倾向值匹配
library(MatchIt)
m.out1 <- matchit(net ~ fsize+lnfincome+
                    cfps_gender + cfps2014_age+edul+ marriage +urban14,
                  data = d_na,
                  method = "subclass", distance = "glm")
summary(m.out1, un = FALSE)
plot(summary(m.out1))

fit_psm <- glm(chuangye~  fsize+lnfincome+lnsavings+lnrq+
                 cfps_gender +cfps_minzu + cfps2014_age+ age2 +edul +health+ marriage +cfps_party+hukou+urban14+provcd14+net, 
               data =d_na)
coef(fit_psm, vcov. = vcovCL, cluster = ~subclass)
library(lmtest)
library(sandwich)
coeftest(fit_psm,vcov. = vcovCL)
